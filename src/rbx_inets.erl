%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%

-module(rbx_inets).

-behaviour(gen_server).

-include_lib("inets/include/httpd.hrl").

%% gen_server callbacks
-export([start/0, start/1, start_link/1, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

-export([do/1]).

-export([attach/1, detach/0, attached_node/0]).

-record(state, {document_root, node, utc_log = false}).

%=======================================================================================================================
% public interface
%=======================================================================================================================
-spec attach(node()) -> ok.
attach(Node) ->
   gen_server:call(rbx_inets, {attach, Node}).

-spec detach() -> ok.
detach() ->
   gen_server:cast(rbx_inets, detach).

-spec attached_node() -> atom().
attached_node() ->
   gen_server:call(rbx_cons, attached_node).

%=======================================================================================================================
% gen_server interface functions
%=======================================================================================================================
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup,
           	   {rbx_inets, {rbx_inets, start_link, [Options]},
			    temporary, brutal_kill, worker, [rbx_inets]}).

start_link(Options) ->
   gen_server:start_link({local, rbx_inets}, ?MODULE, Options, []).

init(Options) ->
   inets:start(),
   DocRoot = filename:nativename(rbx_utils:get_option(document_root, Options, ".")),
   {ok, Pid} = inets:start(httpd, [
      {port, rbx_utils:get_option(inets_port, Options, 8000)},
      {server_name, "rbx"},
      {server_root, "."},
      {document_root, DocRoot},
      {modules, [?MODULE]},
      {mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
   ]),
   link(Pid),
   UtcLog = case application:get_env(sasl, utc_log) of
      {ok, true} -> true;
      _ -> false
   end,
   {ok, #state{document_root = DocRoot, node = node(), utc_log = UtcLog}}.

handle_call(get_types, _, State) ->
   {reply, rbx:get_types(State#state.node), State};
handle_call({get_reports, Filters}, _From, State) ->
   Records = rbx:list(State#state.node, Filters),
   {reply, {Records, State#state.utc_log}, State};
handle_call({get_sel_reports, RecList}, _From, State) ->
   Records = rbx:show(State#state.node, RecList),
   FmtRecords = lists:foldr(fun(R, Acc) -> [report_formatter_html:format(R, State#state.utc_log) | Acc] end, [], Records),
   {reply, FmtRecords, State};
handle_call(get_doc_root, _From, State) ->
   {reply, State#state.document_root, State};
handle_call({attach, Node}, _From, State) ->
   case net_adm:ping(Node) of
      pong ->
         Res = rpc:call(Node, erlang, whereis, [rbx]),
         if
            is_pid(Res) ->
               io:format("Attached to node ~p~n", [Node]),
               {reply, ok, State#state{node = Node}};
            true ->
               io:format("rbx not started on ~p~n", [Node]),
               {reply, error, State}
         end;
      pang ->
         io:format("Handshake with ~p failed.~n", [Node]),
         {reply, error, State}
   end;
handle_call(attached_node, _From, State) ->
   {reply, State#state.node, State};
handle_call(_, _, State) ->
   {reply, ok, State}.

handle_cast({rescan, MaxRecords}, State) ->
   rbx:rescan(State#state.node, MaxRecords),
   {noreply, State};
handle_cast(detach, State) ->
   {noreply, State#state{node = node()}};
handle_cast(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _) ->
   ok.

handle_info(_Info, State) ->
   {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%=======================================================================================================================
% inets httpd callbacks
%=======================================================================================================================
do(#mod{request_uri = Uri, entity_body = Query}) when Uri == "/rescan" ->
   Response = rescan(Query),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri, entity_body = Query}) when Uri == "/get_reports" ->
   Response = get_reports(Query),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri, entity_body = RecList}) when Uri == "/get_sel_reports" ->
   Response = get_sel_reports(RecList),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri})  when Uri == "/" ->
   {ok, Bin} = file:read_file(get_doc_root() ++ "/www/index.html"),
   Response = io_lib:format(binary_to_list(Bin), [node()]),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri})  when Uri == "/favicon.ico" ->
   {proceed, [{response, {404, ""}}]};
do(#mod{request_uri = Uri}) ->
   case file:read_file(get_doc_root() ++ Uri) of
      {ok, Bin} ->
         {proceed, [{response, {200, binary_to_list(Bin)}}]};
      {error, Reason} ->
         error_logger:error_msg("Unable to read file '~s'. Reason = ~p~n", [Uri, Reason]),
         {proceed, [{response, {404, "ERROR: Page " ++ Uri ++ " not found."}}]}
   end.

%=======================================================================================================================
%  private
%=======================================================================================================================

get_doc_root() ->
   gen_server:call(rbx_inets, get_doc_root).

rescan(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   rescan(Term);
rescan({MaxRecords, RecOnPage, Filters}) ->
   gen_server:cast(rbx_inets, {rescan, MaxRecords}),
   get_reports({Filters, 1, RecOnPage}).

get_reports(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   get_reports(Term);
get_reports({Filters, Page, RecOnPage}) ->
   AllTypes = gen_server:call(rbx_inets, get_types),
   {Records, UtcLog} = gen_server:call(rbx_inets, {get_reports, Filters}),
   lists:concat(["{\"types\":", list_to_json(AllTypes, UtcLog, fun(T, _) -> "\"" ++ atom_to_list(T) ++ "\"" end), ',',
                  "\"reports_count\":", length(Records), ',',
                 "\"reports\":", get_reports(Records, UtcLog, Page, RecOnPage), '}']).
get_reports(Records, UtcLog, Page, RecOnPage) ->
   StartFrom = lists:nthtail((Page - 1) * RecOnPage, Records),
   PageRecords = lists:sublist(StartFrom, min(length(StartFrom), RecOnPage)),
   list_to_json(PageRecords, UtcLog, fun report_to_json/2).

get_sel_reports(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   gen_server:call(rbx_inets, {get_sel_reports, Term}).

report_to_json({No, RepType, Pid, Date}, UtcLog) ->
   lists:concat(["{\"no\":\"", No, "\",",
   "\"type\":\"", RepType, "\",",
   "\"pid\":\"", Pid, "\",",
   "\"date\":\"", rbx_utils:date_to_str(Date, UtcLog), "\"}"]).

list_to_json(List, UtcLog, Fun) ->
   list_to_json(List, UtcLog, Fun, "[").
list_to_json([], _UtcLog, _Fun, Acc) ->
   Acc ++ "]";
list_to_json([Last], UtcLog, Fun, "[") ->
   lists:concat(["[", Fun(Last, UtcLog), "]"]);
list_to_json([Last], UtcLog, Fun, Acc) ->
   lists:concat([Acc, Fun(Last, UtcLog), "]"]);
list_to_json([H|T], UtcLog, Fun, Acc) ->
   list_to_json(T, UtcLog, Fun, lists:concat([Acc, Fun(H, UtcLog), ","])).
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

-record(state, {document_root, nodes, client_state, utc_log = false}).
-record(clstate, {re = "", ignored_rtypes = [], node = undef, do_rescan = false, max_reports = 100, page = 1,
      rec_on_page = 30}).

-define(TIMEOUT, 1000).

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
   Nodes = get_rbx_nodes(),
   {ok, #state{document_root = DocRoot, nodes = Nodes, client_state = #clstate{node = hd(Nodes)}, utc_log = UtcLog}, ?TIMEOUT}.

handle_call(get_state, _From, State = #state{nodes = Nodes, client_state = ClState}) ->
   RTypes = rbx:get_rtypes(ClState#clstate.node),
   {reply, {Nodes, RTypes, ClState}, State};
handle_call({get_replist, ClState}, _From, State = #state{client_state = OldClState}) ->
   DoRescan = ClState#clstate.do_rescan orelse ClState#clstate.node =/= OldClState#clstate.node orelse
      ClState#clstate.max_reports =/= OldClState#clstate.max_reports,
   RTypes = case DoRescan of
      true -> rbx:rescan(ClState#clstate.node, ClState#clstate.max_reports);
      false -> rbx:get_rtypes(ClState#clstate.node)
   end,
   DesiredRTypes = lists:subtract(RTypes, ClState#clstate.ignored_rtypes),
   RepList = rbx:list(ClState#clstate.node, [{types, DesiredRTypes}, {reg_exp, ClState#clstate.re}]),
   {reply, {RepList, RTypes, State#state.utc_log}, State#state{client_state = ClState}, ?TIMEOUT};
handle_call({get_sel_reports, Node, RecList}, _From, State) ->
   Records = rbx:show(Node, RecList),
   FmtRecords = lists:foldr(fun(R, Acc) -> [report_formatter_html:format(R, State#state.utc_log) | Acc] end, [], Records),
   {reply, FmtRecords, State, ?TIMEOUT};
handle_call(get_doc_root, _From, State) ->
   {reply, State#state.document_root, State, ?TIMEOUT};
handle_call(_, _, State) ->
   {reply, ok, State, ?TIMEOUT}.

handle_cast(_Msg, State) ->
   {noreply, State, ?TIMEOUT}.

terminate(_Reason, _) ->
   ok.

handle_info(timeout, State) ->
   Nodes = get_rbx_nodes(),
   {noreply, State#state{nodes = Nodes}, ?TIMEOUT};
handle_info(_Info, State) ->
   {noreply, State, ?TIMEOUT}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%=======================================================================================================================
% inets httpd callbacks
%=======================================================================================================================
do(#mod{request_uri = Uri, entity_body = Query}) when Uri == "/get_replist" ->
   Response = get_replist(Query),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri}) when Uri == "/get_state" ->
   Response = get_state(),
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

get_state() ->
   {Nodes, RTypes, ClState} = gen_server:call(rbx_inets, get_state),
   lists:concat(["{\"nodes\":", list_to_json(Nodes, true, fun(T, _) -> "\"" ++ atom_to_list(T) ++ "\"" end), ',',
                  "\"rtypes\":", list_to_json(RTypes, true, fun(T, _) -> "\"" ++ atom_to_list(T) ++ "\"" end), ',',
                  "\"re\":\"", ClState#clstate.re, "\",",
                  "\"ignored_rtypes\":", list_to_json(ClState#clstate.ignored_rtypes, true, fun(T, _) -> "\"" ++ atom_to_list(T) ++ "\"" end), ',',
                  "\"node\":\"", ClState#clstate.node, "\",",
                  "\"do_rescan\":", ClState#clstate.do_rescan, ',',
                  "\"max_reports\":\"", ClState#clstate.max_reports, "\",",
                  "\"page\":\"", ClState#clstate.page, "\",",
                  "\"rec_on_page\":\"", ClState#clstate.rec_on_page, "\"",
                  "}"]).

get_rbx_nodes() ->
   Nodes = nodes([this, visible]),
   lists:foldr(fun(Node, Acc) ->
      Res = rpc:call(Node, erlang, whereis, [rbx]),
      if is_pid(Res) ->
         [Node | Acc];
      true ->
         Acc
      end
   end, [], Nodes).

get_doc_root() ->
   gen_server:call(rbx_inets, get_doc_root).

get_replist(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   get_replist(Term);
get_replist(ClState) ->
   {RepList, RTypes, UtcLog} = gen_server:call(rbx_inets, {get_replist, ClState}),
   lists:concat(["{\"rtypes\":", list_to_json(RTypes, UtcLog, fun(T, _) -> "\"" ++ atom_to_list(T) ++ "\"" end), ',',
                  "\"reports_count\":", length(RepList), ',',
                 "\"reports\":", get_replist(RepList, UtcLog, ClState#clstate.page, ClState#clstate.rec_on_page), '}']).
get_replist(RepList, UtcLog, Page, RecOnPage) ->
   StartFrom = lists:nthtail((Page - 1) * RecOnPage, RepList),
   PageReports = lists:sublist(StartFrom, min(length(StartFrom), RecOnPage)),
   list_to_json(PageReports, UtcLog, fun report_to_json/2).

get_sel_reports(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, {Node, RecList}} = erl_parse:parse_term(Tokens),
   gen_server:call(rbx_inets, {get_sel_reports, Node, RecList}).

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
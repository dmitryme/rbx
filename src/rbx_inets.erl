-module(rbx_inets).

-behaviour(gen_server).

-include_lib("inets/include/httpd.hrl").

%% gen_server callbacks
-export([start/0, start/1, start_link/1, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

-export([do/1]).

-record(state, {document_root}).

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
      {port, get_option(inets_port, Options, 8000)},
      {server_name, "rbx"},
      {server_root, "."},
      {document_root, DocRoot},
      {modules, [?MODULE]},
      {mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
   ]),
   link(Pid),
   {ok, #state{document_root = DocRoot}}.

handle_call(get_types, _, State) ->
   {reply, rbx:get_types(), State};
handle_call({get_records, Filters}, _From, State) ->
   Records = rbx:list(Filters),
   {reply, Records, State};
handle_call({get_record, RecNum}, _From, State) ->
   FmtRecord = record_formatter_html:format(rbx:show(RecNum)),
   {reply, FmtRecord, State};
handle_call(get_doc_root, _From, State) ->
   {reply, State#state.document_root, State}.

handle_cast({rescan, MaxRecords}, State) ->
   rbx:rescan(MaxRecords),
   {noreply, State};
handle_cast(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _) ->
   ok.

handle_info(_Info, State) ->
   {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

do(#mod{request_uri = Uri, entity_body = Query}) when Uri == "/rescan" ->
   Response = rescan(Query),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri, entity_body = Query}) when Uri == "/get_records" ->
   Response = get_records(Query),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri, entity_body = RecNum}) when Uri == "/get_record" ->
   Response = get_record(list_to_integer(RecNum)),
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


%%=============================================================
%% utils
%%=============================================================

get_doc_root() ->
   gen_server:call(rbx_inets, get_doc_root).

rescan(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   rescan(Term);
rescan({MaxRecords, RecOnPage, Filters}) ->
   gen_server:cast(rbx_inets, {rescan, MaxRecords}),
   get_records({Filters, 1, RecOnPage}).

get_records(Query) when is_list(Query) ->
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   get_records(Term);
get_records({Filters, Page, RecOnPage}) ->
   AllTypes = gen_server:call(rbx_inets, get_types),
   Records = gen_server:call(rbx_inets, {get_records, Filters}),
   lists:concat(["{\"types\":", list_to_json(AllTypes, fun(T) -> "\"" ++ atom_to_list(T) ++ "\"" end), ',',
                  "\"records_count\":", length(Records), ',',
                 "\"records\":", get_records(Records, Page, RecOnPage), '}']).
get_records(Records, Page, RecOnPage) ->
   StartFrom = lists:nthtail((Page - 1) * RecOnPage, Records),
   PageRecords = lists:sublist(StartFrom, min(length(StartFrom), RecOnPage)),
   list_to_json(PageRecords, fun record_to_json/1).

get_record(RecNum) ->
   gen_server:call(rbx_inets, {get_record, RecNum}).

record_to_json({No, RepType, Pid, Date}) ->
   lists:concat(["{\"no\":\"", No, "\",",
   "\"type\":\"", RepType, "\",",
   "\"pid\":\"", Pid, "\",",
   "\"date\":\"", rbx_utils:date_to_str(Date, true), "\"}"]).

list_to_json(List, Fun) ->
   list_to_json(List, Fun, "[").
list_to_json([], _Fun, Acc) ->
   Acc ++ "]";
list_to_json([Last], Fun, "[") ->
   lists:concat(["[", Fun(Last), "]"]);
list_to_json([Last], Fun, Acc) ->
   lists:concat([Acc, Fun(Last), "]"]);
list_to_json([H|T], Fun, Acc) ->
   list_to_json(T, Fun, lists:concat([Acc, Fun(H), ","])).

-module(log_viewer_httpd).

-behaviour(gen_server).

-include_lib("inets/include/httpd.hrl").

%% gen_server callbacks
-export([start/0, start/1, start_link/1, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

-export([do/1]).

-record(state, {css, index, reports, rdisplay, jquery}).

start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup,
           	   {log_viewer_httpd, {log_viewer_https, start_link, [Options]},
			    temporary, brutal_kill, worker, [log_viewer_httpd]}).

start_link(Options) ->
   gen_server:start_link({local, log_viewer_httpd}, ?MODULE, Options, []).

init(Options) ->
   inets:start(),
   {ok, Pid} = inets:start(httpd, [
      {port, get_port(Options)},
      {server_name, "log_viewer"},
      {server_root, "."},
      {document_root, "."},
      {modules, [?MODULE]},
      {mime_types, [{"css", "text/css"}, {"js", "text/javascript"}, {"html", "text/html"}]}
   ]),
   link(Pid),
   {ok, Css} = file:read_file("static/style.css"),
   {ok, Index} = file:read_file("static/index.html"),
   {ok, Reports} = file:read_file("static/reports.html"),
   {ok, RDisplay} = file:read_file("static/rdisplay.html"),
   {ok, JQuery} = file:read_file("static/jquery.js"),
   {ok, #state{
         css = binary_to_list(Css),
         index = binary_to_list(Index),
         reports = binary_to_list(Reports),
         rdisplay = binary_to_list(RDisplay),
         jquery = binary_to_list(JQuery)}}.

handle_call(get_css, _, State) ->
   {reply, State#state.css, State};
handle_call(get_index, _, State) ->
   Res = lists:flatten(io_lib:format(State#state.index, [node()])),
   {reply, Res, State};
handle_call(get_reports, _, State) ->
   {reply, State#state.reports, State};
handle_call(get_rdisplay, _, State) ->
   {reply, State#state.rdisplay, State};
handle_call(get_jquery, _, State) ->
   {reply, State#state.jquery, State};
handle_call(get_types, _, State) ->
   {reply, log_viewer:get_types(), State};
handle_call({get_records, Grep, Types}, _From, State) ->
   Records = log_viewer:list(Grep, Types),
   {reply, Records, State}.

handle_cast(_Msg, State) ->
   {norepy, State}.

terminate(_Reason, _) ->
   ok.

handle_info(_Info, State) ->
   {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

do(#mod{request_uri = Uri})  when Uri == "/" ->
   Response = gen_server:call(log_viewer_httpd, get_index),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri}) when Uri == "/static/style.css" ->
   Response = gen_server:call(log_viewer_httpd, get_css),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri}) when Uri == "/static/reports.html" ->
   Response = gen_server:call(log_viewer_httpd, get_reports),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri}) when Uri == "/static/rdisplay.html" ->
   Response = gen_server:call(log_viewer_httpd, get_rdisplay),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri}) when Uri == "/static/jquery.js" ->
   Response = gen_server:call(log_viewer_httpd, get_jquery),
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri}) when Uri == "/get_types" ->
   Types = gen_server:call(log_viewer_httpd, get_types),
   Response = "{\"types\": " ++ list_to_json(Types, fun(T) -> "\"" ++ atom_to_list(T) ++ "\"" end) ++ " }",
   {proceed, [{response, {200, Response}}]};
do(#mod{request_uri = Uri, entity_body = Query}) when Uri == "/get_records" ->
   Response = get_records(Query),
   io:format("~p~n", [Response]),
   {proceed, [{response, {200, Response}}]};
do(Mod) ->
   %io:format("~p~n", [Mod]),
   {proceed, [{response, {404, "ERROR: Page " ++ Mod#mod.request_uri ++ " not found."}}]}.

get_port(Options) ->
   case proplists:get_value(inets_port, Options) of
      undefined ->
         case application:get_env(log_viewer, inets_port) of
            undefined ->
               8000;
            {ok, Val} ->
               Val
         end;
      Port ->
         Port
   end.

get_records({Grep, Page, RecOnPage, Types}) ->
   Records = gen_server:call(log_viewer_httpd, {get_records, Grep, Types}),
   lists:concat(["{\"pages\":", get_pages(Records, RecOnPage), ',',
                 "\"records\":", get_records(Records, Page, RecOnPage), '}']);
get_records(Query) ->
   io:format("~p~n", [Query]),
   {ok, Tokens, _} = erl_scan:string(Query),
   {ok, Term} = erl_parse:parse_term(Tokens),
   io:format("~p~n", [Term]),
   get_records(Term).

get_records(Records, Page, RecOnPage) ->
   PageRecords = lists:nthtail((Page - 1) * RecOnPage, Records),
   list_to_json(PageRecords, fun record_to_json/1).

get_pages(Records, RecOnPage) ->
   case get_pages(Records, RecOnPage, 1) of
      Pages when length(Pages) =< 1 ->
         "[]";
      Pages ->
         list_to_json(Pages, fun(P) -> lists:concat(['"', P, '"']) end)
   end.
get_pages([], _, _PageNum) ->
   [];
get_pages(Records, RecOnPage, PageNum) when length(Records) < RecOnPage ->
   [PageNum];
get_pages(Records, RecOnPage, PageNum) ->
   [PageNum | get_pages(lists:nthtail(RecOnPage, Records), RecOnPage, PageNum + 1)].

record_to_json({No, RepType, Pid, Date}) ->
   lists:concat(["{\"no\":\"", No, "\",",
   "\"type\":\"", RepType, "\",", 
   "\"pid\":\"", Pid, "\",",
   "\"date\":\"", Date, "\"}"]).

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
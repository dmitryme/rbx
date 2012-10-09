-module(rbx_cons).

-behaviour(gen_server).

%% gen_server callbacks
-export([start/0, start/1, start_link/1, reload_static/0, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

-record(state, {}).

start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup,
           	   {rbx_cons, {rbx_cons, start_link, [Options]},
			    temporary, brutal_kill, worker, [rbx_cons]}).

start_link(Options) ->
   gen_server:start_link({local, rbx_cons}, ?MODULE, Options, []).

reload_static() ->
   gen_server:cast(rbx_cons, reload_static).

init(_Options) ->
   {ok, #state{}}.

handle_call(_, _, State) ->
   {reply, ok, State}.

handle_cast(_, State) ->
   {noreply, State}.

terminate(_Reason, _) ->
   ok.

handle_info(_Info, State) ->
   {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

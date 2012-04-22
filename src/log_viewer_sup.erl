-module(log_viewer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Options), {I, {I, start_link, [Options]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {
       ok,
       {
          {one_for_one, 5, 10},
          [
             ?CHILD(log_viewer, worker, [])
             %, ?CHILD(log_viewer_inets, worker, [])   % uncomment, if you want web ui
             %, ?CHILD(log_viewer_cons, worker, [])
          ]
       }
    }.

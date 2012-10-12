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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%

-module(rbx_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Options), {I, {I, start_link, [Options]}, permanent, 5000, Type, [I]}).

%=======================================================================================================================
% supervisor interface functions
%=======================================================================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {
       ok,
       {
          {one_for_one, 5, 10},
          [
             ?CHILD(rbx, worker, []),
             ?CHILD(rbx_inets, worker, []),
             ?CHILD(rbx_cons, worker, [])
          ]
       }
    }.

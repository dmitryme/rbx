%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(log_viewer).

%% External exports
-export([list/0, list/2, rescan/1, show/1, get_types/0]).

list() ->
   list([], all).
list(RegExp, Types)
      when is_list(RegExp) =/= true orelse (is_list(Types) == false andalso Types =/= all) ->
   {error, wrong_args};
list(RegExp, Types) ->
   call({list, RegExp, Types}).

rescan(Max) ->
   call({rescan, Max}).

show(Number) when is_integer(Number) ->
   call({show_number, Number}).

get_types() ->
   call(get_types).

%%-----------------------------------------------------------------
%% Internal functions.
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(log_viewer_srv, Req, infinity).
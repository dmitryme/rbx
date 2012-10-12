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

-module(rbx_utils).

-export([date_to_str/2, list_to_atom/1, get_option/3]).

-compile({no_auto_import, [list_to_atom/1]}).

%=======================================================================================================================
%  public
%=======================================================================================================================

list_to_atom(Lst) ->
   case catch(list_to_existing_atom(Lst)) of
      {'EXIT', {badarg, _}} ->
         erlang:list_to_atom(Lst);
      Res ->
         Res
   end.

date_to_str(DateTime, true) ->
   {{YY,MoMo,DD},{HH,MiMi,SS}} = local_time_to_universal_time(DateTime),
   lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:" "~2.2.0w:~2.2.0w UTC", [YY,MoMo,DD,HH,MiMi,SS]));
date_to_str({{Y,Mo,D},{H,Mi,S}}, false) ->
   lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:" "~2.2.0w:~2.2.0w", [Y,Mo,D,H,Mi,S])).

get_option(OpName, Options, Default) ->
   case proplists:get_value(OpName, Options) of
      undefined ->
         case application:get_env(OpName) of
            undefined ->
               Default;
            {ok, Val} ->
               Val
         end;
      Value ->
         Value
   end.


%=======================================================================================================================
%  private
%=======================================================================================================================
local_time_to_universal_time({Date,Time}) ->
   case calendar:local_time_to_universal_time_dst({Date,Time}) of
      [UCT] ->
         UCT;
      [UCT1,_UCT2] ->
         UCT1;
      [] -> % should not happen
         {Date,Time}
   end.

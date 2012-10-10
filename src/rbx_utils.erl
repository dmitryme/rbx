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
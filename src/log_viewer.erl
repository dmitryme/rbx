-module(log_viewer).

-type rec_type() :: atom().
-type filter() :: {'types', [rec_type()]} |
                  {'reg_exp', string()} |
                  {'interval', {calendar:datetime(), calendar:datetime()}} |
                  {'interval', {calendar:datetime(), from}} |
                  {'interval', {calendar:datetime(), to}}.
-type filters() :: [filter()].
-type list_result() :: list() | {'error', term()}.

%% External exports
-export([list/0, list/1, rescan/1, show/1, get_types/0]).

-spec list() -> list_result().
list() ->
   list([]).

-spec list(filters()) -> list_result().
list(Filter)
      when is_list(Filter) =/= true ->
   {error, wrong_args};
list(Filter) ->
   call({list, Filter}).

-spec rescan(pos_integer()) -> filters() | {'error', term()}.
rescan(Max) ->
   call({rescan, Max}).

-spec show(all | pos_integer()) -> term().
show(Number) when is_integer(Number) ->
   call({show_number, Number}).

-spec get_types() -> filters() | {'error', term()}.
get_types() ->
   call(get_types).

%%-----------------------------------------------------------------
%% Internal functions.
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(log_viewer_srv, Req, infinity).
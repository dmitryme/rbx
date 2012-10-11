-module(record_formatter_cons).

%% intermodule exports
-export([format/3]).

-define(LINE_W, 79).

format(Device, UtcLog, {Date, {error_report, _GL, {Pid, crash_report, CrashReport}}}) ->
   print_info(Device, format_h("CRASH REPORT", Pid, Date, UtcLog), format_c(CrashReport));
format(Device, UtcLog, {Date, {error_report, _GL, {Pid, supervisor_report, SupReport}}})->
   print_info(Device, format_h("SUPERVISOR REPORT", Pid, Date, UtcLog), format_s(SupReport));
format(Device, UtcLog, {Date, {error_report, _GL, {Pid, _Type, Report1}}})->
   print_info(Device, format_h("ERROR REPORT", Pid, Date, UtcLog), [{data, Report1}]);
format(Device, UtcLog, {Date, {info_report, _GL, {Pid, progress, SupProgress}}})->
   print_info(Device, format_h("PROGRESS REPORT", Pid, Date, UtcLog), format_p(SupProgress));
format(Device, UtcLog, {Date, {info_report, _GL, {Pid, _Type, Report1}}})->
   print_info(Device, format_h("INFO REPORT", Pid, Date, UtcLog), [{data, Report1}]);
format(Device, UtcLog, {Date, {warning_report, _GL, {Pid, _Type, Report1}}})->
   print_info(Device, format_h("WARNING REPORT", Pid, Date, UtcLog), [{data, Report1}]);
format(Device, UtcLog, {Date, {error, _GL, {Pid, Format, Args}}})->
   print_info(Device, format_h("ERROR REPORT", Pid, Date, UtcLog), {text, io_lib:format(Format, Args)});
format(Device, UtcLog, {Date, {info_msg, _GL, {Pid, Format, Args}}})->
   print_info(Device, format_h("INFO REPORT", Pid, Date, UtcLog), {text, io_lib:format(Format, Args)});
format(Device, UtcLog, {Date, {warning_msg, _GL, {Pid, Format, Args}}})->
   print_info(Device, format_h("WARNING REPORT", Pid, Date, UtcLog), {text, io_lib:format(Format, Args)});
format(Device, UtcLog, {Date, {Type, _GL, TypeReport}})->
   io:format(Device, "~nInfo type <~w> ~s~n~p",
      [Type, rbx_utils:date_to_str(Date, UtcLog), TypeReport]);
format(Device, _UtcLog, Report) ->
   io:format(Device, "Unknown report type: ~s", [Report]).

format_h(Header, Pid, Date, UtcLog) ->
   NHeader = lists:flatten(io_lib:format("~s  ~w", [Header, Pid])),
   io_lib:format("~-60s~19s", [NHeader, rbx_utils:date_to_str(Date, UtcLog)]).

%%-----------------------------------------------------------------
%% Crash report
%%-----------------------------------------------------------------
format_c([OwnReport, LinkReport]) ->
   [{items, {"Crashing process", OwnReport}}, format_neighbours(LinkReport)].

format_neighbours([Data| Rest]) ->
   [{newline, 1},{items, {"Neighbour process", Data}} | format_neighbours(Rest)];
format_neighbours([]) -> [].

%%-----------------------------------------------------------------
%% Supervisor report
%%-----------------------------------------------------------------
format_s(Data) ->
   SuperName = proplists:get_value(supervisor, Data),
   ErrorContext = proplists:get_value(errorContext, Data),
   Reason = proplists:get_value(reason, Data),
   ChildInfo = proplists:get_value(offender, Data),
   [{data, [{"Reporting supervisor", SuperName}]},
    {newline, 1},
    {items, {"Child process",
      [{errorContext, ErrorContext},
      {reason, Reason} | lists:map(fun(CI) -> transform_mfa(CI) end, ChildInfo)]}}].

transform_mfa({mfa, Value}) ->
   {start_function, Value};
transform_mfa(X) ->
   X.

%%-----------------------------------------------------------------
%% Progress report
%%-----------------------------------------------------------------
format_p(Data) ->
   [{data, Data}].

print_info(Device, Header, Report) ->
   Format =lists:concat(["~s~n~", ?LINE_W, "..=s~n~s~n"]),
   io:format(Device, Format, [Header, "", print_report(Report)]).

print_report([]) ->
   [];
print_report({text, Text}) ->
   lists:flatten(Text);
print_report([{data, Data}|T]) ->
   [print_data(Data), print_report(T)];
print_report([{items, Items}|T]) ->
   [print_items(Items), print_report(T)];
print_report([{newline, N}|T]) ->
   [print_newlines(N), print_report(T)];
print_report([_|T]) ->  % ignore any erroneous format.
    print_report(T).

print_data([]) -> [];
print_data([{Key, Value}|T]) ->
   [print_one_line(Key, Value) | print_data(T)];
print_data([Value|T]) ->
   StrData = io_lib:format("~s~n", [term_to_string(Value)]),
   [StrData, print_data(T)].

print_items({Name, Items}) ->
   print_items(Name, Items).

print_newlines(0) -> [];
print_newlines(N) when N > 0 ->
   [io_lib:format("~n", []), print_newlines(N-1)].

print_one_line(Key, Value) ->
   StrKey = term_to_string(Key),
   KeyLen = lists:min([length(StrKey), ?LINE_W]),
   ValueLen = ?LINE_W - KeyLen,
   Try = term_to_string(Value),
   Length = length(Try),
   if
	   Length < ValueLen ->
         Format1 = lists:concat(["~-", KeyLen, "s~", ValueLen, "s~n"]),
	      io_lib:format(Format1, [StrKey, Try]);
	   true ->
	      Format2 = lists:concat(["~-", KeyLen, "s~n         ~", ?LINE_W, ".9p~n"]),
	      io_lib:format(Format2, [StrKey, Value])
   end.

term_to_string(Value) ->
   lists:flatten(io_lib:format(get_format(Value), [Value])).

get_format(Value) ->
   case misc_supp:is_string(Value) of
	   true -> "~s";
	   false -> "~p"
   end.

%%-----------------------------------------------------------------
%% Items
%%-----------------------------------------------------------------
print_items(Name, Items) ->
   [print_one_line(Name, " ") | print_item_elements(Items)].

print_item_elements([]) -> [];
print_item_elements([{Key, Value}|T]) ->
   [print_one_line(Key, Value), print_item_elements(T)].

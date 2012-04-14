%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(record_formatter).

%% intermodule exports
-export([format/1]).

format({Date, {error_report, _GL, {Pid, crash_report, CrashReport}}}) ->
   print_info(format_h("CRASH REPORT", Pid, Date), format_c(CrashReport));
format({Date, {error_report, _GL, {Pid, supervisor_report, SupReport}}})->
   print_info(format_h("SUPERVISOR REPORT", Pid, Date), format_s(SupReport));
format({Date, {error_report, _GL, {Pid, _Type, Report1}}})->
   print_info(format_h("ERROR REPORT", Pid, Date), [{data, Report1}]);
format({Date, {info_report, _GL, {Pid, progress, SupProgress}}})->
   print_info(format_h("PROGRESS REPORT", Pid, Date), format_p(SupProgress));
format({Date, {info_report, _GL, {Pid, _Type, Report1}}})->
   print_info(format_h("INFO REPORT", Pid, Date), [{data, Report1}]);
format({Date, {warning_report, _GL, {Pid, _Type, Report1}}})->
   print_info(format_h("WARNING REPORT", Pid, Date), [{data, Report1}]);
format({Date, {error, _GL, {Pid, Format, Args}}})->
   print_info(format_h("ERROR REPORT", Pid, Date), {text, io_lib:format(Format, Args)});
format({Date, {info_msg, _GL, {Pid, Format, Args}}})->
   print_info(format_h("INFO REPORT", Pid, Date), {text, io_lib:format(Format, Args)});
format({Date, {warning_msg, _GL, {Pid, Format, Args}}})->
   print_info(format_h("WARNING REPORT", Pid, Date), {text, io_lib:format(Format, Args)});
format({Date, {Type, _GL, TypeReport}})->
    io_lib:format("~nInfo type <~w> ~s~n~p",
         [Type, common_utils:date_to_str(Date, true), TypeReport]);
format(Report) ->
   io_lib:format("Unknown report type: ~s", [Report]).

format_h(Header, Pid, Date) ->
   io_lib:format("<tr><th>~s</th><th>~w</th><th>~s</th></tr>", [Header, Pid, common_utils:date_to_str(Date, true)]).

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
   SuperName = get_opt(supervisor, Data),
   ErrorContext = get_opt(errorContext, Data),
   Reason = get_opt(reason, Data),
   ChildInfo = get_opt(offender, Data),
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

get_opt(Key, List) ->
   case lists:keysearch(Key, 1, List) of
     {value, {_Key, Val}} ->
         Val;
	   _ ->
         undefined
   end.

print_info(Header, Report) ->
   "<table class='rdisplay_header'>" ++ Header ++ "</table><br/><table class='rdisplay_data'>" ++ print_report(Report) ++ "</table>".

print_report([]) ->
   [];
print_report({text, Text}) ->
   "<tr><td colspan='2'>" ++ replace_to_html_entities(Text) ++ "</td></tr>";
print_report([{data, Data}|T]) ->
   [print_data(Data), print_report(T)];
print_report([{table, Table}|T]) ->
   [print_table(Table), print_report(T)];
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
   StrData = io_lib:format("<tr><td class='item_value' colspan='2'>~s</td></tr>", [term_to_string(Value)]),
   [StrData, print_data(T)].

print_items({Name, Items}) ->
   print_items(Name, Items).

print_table({TableName, ColumnNames, Columns}) ->
   print_table(TableName, ColumnNames, Columns).

print_newlines(0) -> [];
print_newlines(N) when N > 0 ->
   ["<tr><td colspan='2'></td></tr>", print_newlines(N-1)].

print_one_line(Key, Value) ->
   io_lib:format("<tr><td class='item_key'>~s</td><td class='item_value'>~s</td></tr>", [term_to_string(Key), term_to_string(Value)]).

term_to_string(Value) ->
   FmtValue = lists:flatten(io_lib:format(get_format(Value), [Value])),
   lists:flatten(replace_to_html_entities(FmtValue)).

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

%%-----------------------------------------------------------------
%% Table handling
%%-----------------------------------------------------------------
print_table(TableName, _TupleOfColumnNames, []) ->
   "<table><tr><td>Table:&nbsp;" ++ TableName ++ "</td></tr><tr><td>&lt;empty table&gt;</td></tr></table>";
print_table(TableName, TupleOfColumnNames, ListOfTuples)
                when is_list(ListOfTuples), is_tuple(TupleOfColumnNames) ->
   Table = "<table><tr><td>Table:&nbsp;" ++ TableName ++ "</td></tr>",
   ListOfColumnNames = tuple_to_list(TupleOfColumnNames),
   Header = lists:flatten(["<tr>", lists:foldr(fun(ColName, Acc) -> ["<th>", ColName, "</th>", Acc] end,
      [], ListOfColumnNames), "</tr>"]),
	ListOfLists = lists:map(fun(Tuple) -> tuple_to_list(Tuple) end, ListOfTuples),
   Body = lists:foldr(fun(Row, Acc) -> [print_row(Row), Acc] end, [], ListOfLists),
   lists:flatten([Table, Header, Body, "</table>"]).

print_row(Row) ->
   lists:flatten(["<tr>", lists:foldl(fun(ColVal, Acc) ->
                           ["<td>", term_to_string(ColVal), "</td>", Acc]
            end, [], Row), "</tr>"]).

replace_to_html_entities(Str) ->
   lists:foldr(fun(32, Acc) ->
                     ["&nbsp;", Acc];
                  (10, Acc) ->
                     ["<br/>", Acc];
                  (Ch, Acc) ->
                     [Ch | Acc] end, [], Str).

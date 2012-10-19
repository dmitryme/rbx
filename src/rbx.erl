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

-module(rbx).

-behaviour(gen_server).

%% gen_server callbacks
-export([start/0, start/1, start_link/1, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

-export([list/1, list/2, rescan/2, show/2, get_rtypes/1]).

-define(def_max, 100).

-record(state, {dir, data, device, max = ?def_max, types, log}).

-type rep_type() :: atom().
-type filter() :: {'types', [rep_type()]} |
                  {'reg_exp', string()} |
                  {'datetime', {calendar:datetime(), calendar:datetime()}} |
                  {'datetime', {from, calendar:datetime()}} |
                  {'datetime', {to, calendar:datetime()}}.
-type filters() :: [filter()].
-type list_result() :: list() | {'error', term()}.

%=======================================================================================================================
%  public interface
%=======================================================================================================================
-spec list(node()) -> list_result().
list(Node) ->
   list(Node, []).

-spec list(node(), filters()) -> list_result().
list(_Node, Filter)
      when is_list(Filter) =/= true ->
   {error, wrong_args};
list(Node, Filter) ->
   gen_server:call({rbx, Node}, {list, Filter}, infinity).

-spec rescan(node(), pos_integer()) -> filters() | {'error', term()}.
rescan(Node, Max) ->
   gen_server:call({rbx, Node}, {rescan, Max}, infinity).

-spec show(node(), all | pos_integer() | [pos_integer()]) -> term().
show(Node, Number) when is_integer(Number) ->
   gen_server:call({rbx, Node}, {show_number, Number}, infinity);
show(Node, NumList) when is_list(NumList) ->
   gen_server:call({rbx, Node}, {show_number, NumList}, infinity);
show(Node, all) ->
   gen_server:call({rbx, Node}, {show_number, all}, infinity).

-spec get_rtypes(node()) -> filters() | {'error', term()}.
get_rtypes(Node) ->
   gen_server:call({rbx, Node}, get_rtypes, infinity).

%=======================================================================================================================
% gen_server interface functions
%=======================================================================================================================
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup,
         		   {rbx, {rbx, start_link, [Options]},
			    temporary, brutal_kill, worker, [rbx]}).

start_link(Options) ->
   gen_server:start_link({local, rbx}, ?MODULE, Options, []).

init(Options) ->
   process_flag(priority, low),
   process_flag(trap_exit, true),
   Dir = get_report_dir(Options),
   {Data, Types} = scan_files(Dir ++ "/", ?def_max),
   {ok, #state{dir = Dir ++ "/", data = Data, types = Types}}.

handle_call({rescan, Max}, _From, State) ->
   {Data, Types} = scan_files(State#state.dir ++ "/", Max),
   {reply, Types, State#state{data = Data, types = Types, max = Max}};
handle_call(get_rtypes, _From, State) ->
   {reply, State#state.types, State};
handle_call(_, _From, #state{data = undefined}) ->
   {reply, {error, no_data}, #state{}};
handle_call({list, Filters}, _From, State) ->
   try print_list(State#state.types, State#state.dir, State#state.data, Filters) of
      Res ->
         {reply, Res, State}
   catch
      throw:Error ->
         {reply, {error, Error}, State};
      _:Error ->
         error_logger:error_msg(
            "rbx::list call failed: ~p", [erlang:get_stacktrace()]),
         {reply, {error, Error}, State}
   end;
handle_call({show_number, NumList}, _From, State = #state{dir = Dir, data = Data}) when is_list(NumList) ->
   SorterNumList = lists:usort(fun(A, B) -> A > B end, NumList),
   try print_report_by_num_list(Dir, Data, SorterNumList) of
      Res ->
         {reply, Res, State}
   catch
      throw:Error ->
         {reply, {error, Error}, State};
      _:Error ->
         error_logger:error_msg(
            "rbx::show_number call failed: ~p", [erlang:get_stacktrace()]),
         {reply, {error, Error}, State}
   end;
handle_call({show_number, Number}, _From, State = #state{dir = Dir, data = Data}) when is_number(Number) ->
   try print_report_by_num(Dir, Data, Number) of
      Res ->
         {reply, Res, State}
   catch
      throw:Error ->
         {reply, {error, Error}, State};
      _:Error ->
         error_logger:error_msg(
            "rbx::show_number call failed: ~p", [erlang:get_stacktrace()]),
         {reply, {error, Error}, State}
   end;
handle_call({show_number, all}, _From, State = #state{dir = Dir, data = Data}) ->
   try print_all_reports(Dir, Data) of
      Res ->
         {reply, Res, State}
   catch
      throw:Error ->
         {reply, {error, Error}, State};
      _:Error ->
         error_logger:error_msg(
            "rbx::show_number call failed: ~p", [erlang:get_stacktrace()]),
         {reply, {error, Error}, State}
   end;
handle_call(Msg, _From, State) ->
   {reply, {error, {unknown_call, Msg}}, State}.

terminate(_Reason, #state{}) ->
   ok.

handle_cast(_Msg, State) ->
   {noreply, State}.
handle_info(_Info, State) ->
   {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

get_report_dir(Options) ->
   case lists:keysearch(report_dir, 1, Options) of
      {value, {_Key, RptDir}} ->
         RptDir;
      _ ->
         case application:get_env(report_dir) of
            undefined ->
               case application:get_env(sasl, error_logger_mf_dir) of
                  {ok, Dir} ->
                     Dir;
                  _ ->
                     exit("cannot locate report directory")
               end;
            {ok, Dir} ->
               Dir
         end
   end.

%%-----------------------------------------------------------------
%% Func: scan_files(RptDir, Max)
%% Args: RptDir ::= string().
%%       Max ::= integer() | all, describing how many reports
%5               to read.
%% Purpose: Scan all report files one time, and build a list of
%%          small elements
%% Returns: Data, where Data is a list of
%%          {Number, Type, ShortDescr, Date, Fname, FilePosition}.
%%-----------------------------------------------------------------
scan_files(RptDir, Max) ->
   case file:open(RptDir ++ "/index", [raw, read]) of
      {ok, Fd} ->
         case catch file:read(Fd, 1) of
            {ok, [LastWritten]} ->
               file:close(Fd),
               Files = make_file_list(RptDir, LastWritten),
               scan_files(RptDir, Files, Max);
            _X ->
               file:close(Fd),
               exit("cannot read the index file")
         end;
      _X ->
         exit("cannot read the index file")
   end.

make_file_list(Dir, FirstFileNo) ->
   case file:list_dir(Dir) of
      {ok, FileNames} ->
         FileNumbers = lists:zf(
               fun(Name) ->
                     case catch list_to_integer(Name) of
                        Int when is_integer(Int) ->
                           {true, Int};
                        _ ->
                           false
                     end
               end,
               FileNames),
         shift(lists:sort(FileNumbers), FirstFileNo);
      _ -> exit({bad_directory, Dir})
   end.

shift(List, First) ->
   shift(List, First, []).

shift([H | T], H, Res) ->
   [H | Res] ++ lists:reverse(T);
shift([H | T], First, Res) ->
   shift(T, First, [H | Res]);
shift([], _, Res) ->
   Res.

%%-----------------------------------------------------------------
%% Func: scan_files(Dir, Files, Max)
%% Args: Files is a list of FileName.
%% Purpose: Scan the report files in the index variable.
%% Returns: {Number, Type, ShortDescr, Date, FileName, FilePosition}
%%-----------------------------------------------------------------
scan_files(Dir, Files, Max) ->
   scan_files(Dir, 1, Files, {[], []}, Max).
scan_files(_Dir, _, [], {ResData, ResTypes}, _Max) ->
   {ResData, lists:usort(ResTypes)};
scan_files(_Dir, _, _Files, {ResData, ResTypes}, Max) when Max =< 0 -> {ResData, lists:usort(ResTypes)};
scan_files(Dir, No, [H|T], {ResData, ResTypes}, Max) ->
   {Data, Types} = get_report_data_from_file(Dir, No, H, Max),
   Len = length(Data),
   NewMax = dec_max(Max, Len),
   NewNo = No + Len,
   NewData = Data ++ ResData,
   NewTypes = Types ++ ResTypes,
   scan_files(Dir, NewNo, T, {NewData, NewTypes}, NewMax).

dec_max(all, _) -> all;
dec_max(X,Y) -> X-Y.

get_report_data_from_file(Dir, No, FileNr, Max) ->
   Fname = integer_to_list(FileNr),
   FileName = lists:concat([Dir, Fname]),
   case file:open(FileName, [read]) of
      {ok, Fd} when is_pid(Fd) ->
         read_reports(No, Fd, Fname, Max);
      _ ->
         [{No, unknown, "Can't open file " ++ Fname, "???", Fname, 0}]
   end.

%%-----------------------------------------------------------------
%% Func: read_reports(No, Fd, Fname, Max)
%% Purpose: Read reports from one report file.
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePosition}
%% Note: We have to read all reports, and then check the max-
%%       variable, because the reports are reversed on the file, and
%%       we may need the last ones.
%%-----------------------------------------------------------------
read_reports(No, Fd, Fname, Max) ->
   case catch read_reports(Fd, {[], []}) of
      {ok, {ResData, ResTypes}} ->
         file:close(Fd),
         NewRes =
                  if
            length(ResData) > Max ->
               lists:sublist(ResData, 1, Max);
            true ->
               ResData
         end,
         {add_report_data(NewRes, No, Fname), ResTypes};
      {error, {[Problem | ResData], ResTypes}} ->
         file:close(Fd),
         NewRes =
                  if
            length([Problem|ResData]) > Max ->
               lists:sublist([Problem|ResData], 1, Max);
            true ->
               [Problem|ResData]
         end,
         {add_report_data(NewRes, No, Fname), ResTypes};
      _Else ->
         {[{No, unknown, "Can't read reports from file " ++ Fname, "???", Fname, 0}], [unknown]}
   end.

%%-----------------------------------------------------------------
%% Func: add_report_data(Res, No, FName)
%% Args: Res is a list of {Type, ShortDescr, Date, FilePos}
%% Purpose: Convert a list of {Type, ShortDescr, Date, FilePos} to
%%          a list of {No, Type, ShortDescr, Date, FileName, FilePos}
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePos}
%%-----------------------------------------------------------------
add_report_data(Res, No, FName) ->
   add_report_data(Res, No, FName, []).
add_report_data([{Type, ShortDescr, Date, FilePos}|T], No, FName, Res) ->
   add_report_data(T, No+1, FName,
                   [{No, Type, ShortDescr, Date, FName, FilePos}|Res]);
add_report_data([], _No, _FName, Res) -> Res.

read_reports(Fd, {ResData, ResTypes}) ->
   {ok, FilePos} = file:position(Fd, cur),
   case catch read_report(Fd) of
      {ok, Report} ->
         RealType = get_type(Report),
         {ShortDescr, Date} = get_short_descr(Report),
         Rep = {RealType, ShortDescr, Date, FilePos},
         read_reports(Fd, {[Rep | ResData], [RealType | ResTypes]});
      {error, Error} ->
         {error, {[{unknown, Error, [], FilePos} | ResData], ResTypes}};
      eof ->
         {ok, {ResData, ResTypes}};
      {'EXIT', Reason} ->
         {[{unknown, Reason, [], FilePos} | ResData], ResTypes}
   end.

read_report(Fd) ->
   case io:get_chars(Fd,'',2) of
      [Hi,Lo] ->
         Size = get_int16(Hi,Lo),
         case io:get_chars(Fd,'',Size) of
            eof ->
               throw("Premature end of file");
            List ->
               Bin = list_to_binary(List),
               Ref = make_ref(),
               case (catch {Ref,binary_to_term(Bin)}) of
                  {'EXIT',_} ->
                     throw("Incomplete erlang term in log");
                  {Ref,Term} ->
                     {ok, Term}
               end
         end;
      eof ->
         eof
   end.

get_int16(Hi,Lo) ->
   ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).


%%-----------------------------------------------------------------
%% Update these functions with the reports that should be possible
%% to browse with rb.
%%-----------------------------------------------------------------
get_type({_Time, {error_report, _Pid, {_, crash_report, _}}}) ->
   crash_report;
get_type({_Time, {error_report, _Pid, {_, supervisor_report, _}}}) ->
   supervisor_report;
get_type({_Time, {info_report, _Pid, {_, progress, _}}}) ->
   progress;
get_type({_Time, {Type, _, _}}) -> Type;
get_type(_) -> unknown.

get_short_descr({Date, {error_report, Pid, {_, crash_report, Rep}}}) ->
   [OwnRep | _] = Rep,
   Name =
          case lists:keysearch(registered_name, 1, OwnRep) of
      {value, {_Key, []}} ->
         case lists:keysearch(initial_call, 1, OwnRep) of
            {value, {_K, {M,_F,_A}}} -> M;
            _ -> Pid
         end;
      {value, {_Key, N}} -> N;
      _ -> Pid
   end,
   NameStr = lists:flatten(io_lib:format("~w", [Name])),
   {NameStr, Date};
get_short_descr({Date, {error_report, Pid, {_, supervisor_report,Rep}}}) ->
   Name =
          case lists:keysearch(supervisor, 1, Rep) of
      {value, {_Key, N}} when is_atom(N) -> N;
      _ -> Pid
   end,
   NameStr = lists:flatten(io_lib:format("~w", [Name])),
   {NameStr, Date};
get_short_descr({Date, {_Type, Pid, _}}) ->
   NameStr = lists:flatten(io_lib:format("~w", [Pid])),
   {NameStr, Date};
get_short_descr(_) ->
   {"???", "???"}.

print_list(_, _, [], _) -> [];
print_list(Types, Dir, [Report = {_, RealType, _, Date, _, _}|Tail], Filters) ->
   SelTypes = proplists:get_value(types, Filters, Types),
   case lists:member(RealType, SelTypes) of
      true ->
         Interval = proplists:get_value(datetime, Filters),
         case compare_dates(Date, Interval) of
            true ->
               RegExp = proplists:get_value(reg_exp, Filters, ""),
               case check_grep_report(Dir, Report, RegExp) of
                  match ->
                     [print_short_descr(Report) | print_list(Types, Dir, Tail, Filters)];
                  nomatch ->
                     print_list(Types, Dir, Tail, Filters)
               end;
            false ->
               print_list(Types, Dir, Tail, Filters)
         end;
      false ->
         print_list(Types, Dir, Tail, Filters)
   end.

print_short_descr({No, Type, ShortDescr, Date, _, _}) ->
   {No, Type, ShortDescr, Date}.

print_all_reports(_Dir, []) ->
   [];
print_all_reports(Dir, [{_No, _Type, _Descr, _Date, Fname, FilePosition} | Rest]) ->
   FileName = lists:concat([Dir, Fname]),
   case file:open(FileName, [read]) of
      {ok, Fd} ->
         [read_rep(Fd, FilePosition) | print_all_reports(Dir, Rest)];
      _ ->
         throw(lists:flatten(io_lib:format("can't open file ~p", [Fname])))
   end.

print_report_by_num_list(_Dir, _Data, []) ->
   [];
print_report_by_num_list(Dir, Data, [Num | Tail]) ->
   [print_report_by_num(Dir, Data, Num) | print_report_by_num_list(Dir, Data, Tail)].

print_report_by_num(Dir, Data, Number) ->
   print_report(Dir, Data, Number).

print_report(Dir, Data, Number) ->
   case find_report(Data, Number) of
      {Fname, FilePosition} ->
         FileName = lists:concat([Dir, Fname]),
         case file:open(FileName, [read]) of
            {ok, Fd} ->
               read_rep(Fd, FilePosition);
            _ ->
               throw(lists:flatten(io_lib:format("can't open file ~p", [Fname])))
         end;
      no_report ->
         {error, not_found}
   end.

find_report([{No, _Type, _Descr, _Date, Fname, FilePosition}|_T], No) ->
   {Fname, FilePosition};
find_report([_H|T], No) ->
   find_report(T, No);
find_report([], No) ->
   throw(lists:flatten(io_lib:format("there is no report with number ~p.", [No]))).

get_compare_dates(Date, CompareDate) ->
    case application:get_env(sasl, utc_log) of
	{ok, true} ->
	    {rbx_utils:local_time_to_universal_time(Date),
	     rbx_utils:local_time_to_universal_time(CompareDate)};
	_ ->
	    {Date, CompareDate}
    end.
get_compare_dates(Date, From, To) ->
    case application:get_env(sasl, utc_log) of
	{ok, true} ->
	    {rbx_utils:local_time_to_universal_time(Date),
	     rbx_utils:local_time_to_universal_time(From),
	     rbx_utils:local_time_to_universal_time(To)};
	_ ->
	    {Date, From, To}
    end.

compare_dates(_Date, undefined) ->
   true;
compare_dates(Date, {from, CompareDate}) ->
   {Date2, DateFrom} = get_compare_dates(Date, CompareDate),
   calendar:datetime_to_gregorian_seconds(Date2) >= calendar:datetime_to_gregorian_seconds(DateFrom);
compare_dates(Date, {to, CompareDate}) ->
   {Date2, DateTo} = get_compare_dates(Date, CompareDate),
   calendar:datetime_to_gregorian_seconds(Date2) =< calendar:datetime_to_gregorian_seconds(DateTo);
compare_dates(Date, {From, To}) ->
   {Date2, DateFrom, DateTo} = get_compare_dates(Date, From, To),
   calendar:datetime_to_gregorian_seconds(Date2) >= calendar:datetime_to_gregorian_seconds(DateFrom) andalso
   calendar:datetime_to_gregorian_seconds(Date2) =< calendar:datetime_to_gregorian_seconds(DateTo).

check_grep_report(_Dir, _Report, []) ->
   match;
check_grep_report(Dir, {_No, _Type, _Descr, _Date, Fname, FilePosition}, RegExp) ->
   FileName = lists:concat([Dir, Fname]),
   case file:open(FileName, [read]) of
      {ok, Fd} when is_pid(Fd) ->
         check_rep(Fd, FilePosition, RegExp);
      _ ->
         throw(lists:flatten(io_lib:format("can't open file ~p", [Fname])))
   end.

check_rep(Fd, FilePosition, RegExp) ->
   Report = read_rep(Fd, FilePosition),
   ReportStr = lists:flatten(io_lib:format("~p",[Report])),
   run_re(ReportStr, RegExp).

run_re(Subject, {Regexp, Options}) ->
   run_re(Subject, Regexp, Options);
run_re(Subject, Regexp) ->
   run_re(Subject, Regexp, []).

run_re(Subject, Regexp, Options) ->
   case re:run(Subject, Regexp, Options) of
      nomatch ->
         nomatch;
      _ ->
         match
   end.

read_rep(Fd, FilePosition) ->
   file:position(Fd, {bof, FilePosition}),
   R =
       case read_report(Fd) of
      {ok, Report} ->
         Report;
      eof ->
         eof
   end,
   file:close(Fd),
   R.
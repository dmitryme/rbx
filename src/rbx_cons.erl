-module(rbx_cons).

-behaviour(gen_server).

%% gen_server callbacks
-export([start/0, start/1, start_link/1, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

%% public exports
-export([list/0, list/1, rescan/1, show/0, show/1, start_log/1, stop_log/0, attach/1, detach/0]).

-record(state, {device, utc_log}).

%======================================================================================================================
%  gen_server interface functions
%=======================================================================================================================
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup,
                   {rbx_cons, {rbx_cons, start_link, [Options]},
                            temporary, brutal_kill, worker, [rbx_cons]}).

start_link(Options) ->
   gen_server:start_link({local, rbx_cons}, ?MODULE, Options, []).

rescan() ->
   rescan(all).

rescan(MaxRecords) ->
   gen_server:cast(rbx_cons, {rescan, MaxRecords}).

list() ->
   list([]).

list(Filter) ->
   gen_server:call(rbx_cons, {list, Filter}).

show() ->
   show(all).

show(Number) ->
   gen_server:call(rbx_cons, {show, Number}).

start_log(Filename) ->
   gen_server:cast(rbx_cons, {start_log, Filename}).

stop_log() ->
   gen_server:cast(rbx_cons, stop_log).

attach(_Node) -> ok.

detach() -> ok.

%=======================================================================================================================
%  gen_server interface functions
%=======================================================================================================================
init(Options) ->
   Log = rbx_utils:get_option(start_log, Options, standard_io),
   Device = open_log_file(Log),
   UtcLog = case application:get_env(sasl, utc_log) of
      {ok, true} -> true;
      _ -> false
   end,
   {ok, #state{device = Device, utc_log = UtcLog}}.

handle_call({list, Filters}, _From, State) ->
   Reports = rbx:list(Filters),
   print_list(Reports, State#state.device, State#state.utc_log),
   {reply, ok, State};
handle_call({show, Number}, _From, State) ->
   Report = rbx:show(Number),
   record_formatter_cons:format(State#state.device, State#state.utc_log, Report),
   {reply, ok, State};
handle_call(_, _, State) ->
   {reply, ok, State}.

handle_cast({rescan, MaxRecords}, State) ->
   rbx:rescan(MaxRecords),
   {noreply, State};
handle_cast({start_log, Filename}, State) ->
   NewDevice = open_log_file(Filename),
   {noreply, State#state{device = NewDevice}};
handle_cast(stop_log, State) ->
   close_device(State#state.device),
   {noreply, State#state{device = standard_io}};
handle_cast(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _) ->
   ok.

handle_info(_Info, State) ->
   {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%=======================================================================================================================
%  private
%=======================================================================================================================
open_log_file(standard_io) -> standard_io;
open_log_file(FileName) ->
   case file:open(FileName, [write,append]) of
      {ok, Fd} ->
         Fd;
           Error ->
              io:format("rbx: Cannot open file '~s' (~w).~n", [FileName, Error]),
              io:format("rbx: Using standard_io~n"),
         standard_io
   end.

close_device(Fd) when is_pid(Fd) ->
   catch file:close(Fd);
close_device(_) -> ok.

print_list(Reports, Device, UtcLog) ->
   Header = {"No", "Type", "Process", "Timestamp"},
   PidWidth = find_pid_width([Header | Reports], 0) + 1,
   Format = lists:concat(["~6s~20s ~", PidWidth, "s ~19s~n"]),
   io:format(Device, Format, tuple_to_list(Header)),
   Format1 = lists:concat(["~6..=s ~20..=s ~", PidWidth, "..=s ~19..=s~n"]),
   io:format(Device, Format1, ["", "", "", ""]),
   print_list(Reports, Device, UtcLog, PidWidth).
print_list([], _, _, _) -> true;
print_list([H|T], Device, UtcLog, PidWidth) ->
   print_one_report(H, Device, UtcLog, PidWidth),
   print_list(T, Device, UtcLog, PidWidth).

find_pid_width([], Width) -> Width;
find_pid_width([{_, _, Pid, _}|T], Width) ->
   Try = length(Pid),
   if
           Try > Width -> find_pid_width(T, Try);
           true -> find_pid_width(T, Width)
   end.

print_one_report({No, RepType, Pid, DateTime}, Device, UtcLog, PidWidth) ->
    Format = lists:concat(["~6w ~20w ~", PidWidth, "s ~19s~n"]),
    io:format(Device, Format, [No,
                       RepType,
                       Pid, rbx_utils:date_to_str(DateTime, UtcLog)]).
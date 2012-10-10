-module(rbx_cons).

-behaviour(gen_server).

%% gen_server callbacks
-export([start/0, start/1, start_link/1, init/1, terminate/2, handle_call/3,
         handle_cast/2, handle_info/2, code_change/3]).

%% public exports
-export([list/0, list/1, rescan/1, show/1, start_log/1, stop_log/0, attach/1, detach/0]).

-record(state, {device}).

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

list() ->
   list([]).

list(Filter) ->
   gen_server:cast(rbx_cons, {list, Filter}).

rescan(MaxRecords) ->
   gen_server:cast(rbx_cons, {rescan, MaxRecords}).

show() ->
   show(all).

show(Number) ->
   gen_server:cast(rbx_cons, {show, Number}).

start_log(Filename) ->
   gen_server:cast(rbx_cons, {start_log, Filename}).

stop_log() -> ok.

attach(Node) -> ok.

detach() -> ok.

%=======================================================================================================================
%  gen_server interface functions
%=======================================================================================================================
init(Options) ->
   Log = rbx_utils:get_option(Options, start_log, standard_io),
   Device = open_log_file(Log),
   {ok, #state{device = Device}}.

handle_call(_, _, State) ->
   {reply, ok, State}.

handle_cast({list, Filters}, _From, State) ->
   Reports = rbx:list(Filters),
   print_list(Reports, State#state.device),
   {reply, Res, State};
handle_cast({show, Number}, _From, State) ->
   rbx:show(Number),
   {reply, Res, State}.
handle_cast({rescan, MaxRecords}, State) ->
   rbx:rescan(MaxRecords),
   {noreply, State};
handle_cast({start_log, Filename}, State) ->
   NewDevice = open_log_file(FileName),
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

print_list(Reports, Device) ->
   Header = {"No", "Type", "Process", "Date     Time"},
   Width = find_width([Header | Reports], 0) + 1,
   DateWidth = find_date_width([Header | Data], 0) + 1,
   Format = lists:concat(["~4s~20s ~", Width, "s~20s~n"]),
   io:format(Device, Format, tuple_to_list(Header)),
   io:format(Device, Format, ["==", "====", "=======", "====     ===="]),
   print_list(Device, Reports, Width, DateWidth).
print_list(_, [], _, _, _) -> true;
print_list(Device, [H|T], Width, DateWidth) ->
   print_one_report(Device, H, Width, DateWidth),
   print_list(Device, T, Width, DateWidth).

find_width([], Width) -> Width;
find_width([H|T], Width) ->
   Try = length(element(3, H)),
   if
           Try > Width -> find_width(T, Try);
           true -> find_width(T, Width)
   end.

find_date_width([], Width) -> Width;
find_date_width([H|T], Width) ->
   Try = length(element(4, H)),
   if
           Try > Width -> find_date_width(T, Try);
           true -> find_date_width(T, Width)
   end.

print_one_report({No, RealType, ShortDescr, Date, _Fname, _FilePos},
                 Width, DateWidth) ->
    if
        WantedType == all ->
            print_short_descr(No, RealType, ShortDescr, Date, Width, 
                              DateWidth);
        WantedType == RealType ->
            print_short_descr(No, RealType, ShortDescr, Date, Width, 
                              DateWidth);
        true -> ok
    end.

print_short_descr(No, Type, ShortDescr, Date, Width, DateWidth) ->
    Format = lists:concat(["~4w~20w ~", Width, "s~", DateWidth,"s~n"]),
    io:format(Format, [No,
                       Type, 
                       io_lib:format("~s", [ShortDescr]),
                       Date]).
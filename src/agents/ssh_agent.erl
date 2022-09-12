-module(ssh_agent).

-behavior(gen_server).

-export([exec/2,
         exec/3,
         exec/4,
         get_file/2,
         put_file/3,
         start/1,
         stop/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("eknife.hrl").

-define(SSH_CONNECT_TIMEOUT, 10000).

-define(SSH_CHANNEL_TIMEOUT, 10000).

-define(SSH_COMMAND_TIMEOUT, 10000).

-define(SSH_TERM_WIDTH, 1000).

-define(SSH_TERM_HEIGHT, 1000).

-define(SSH_PACKET_SIZE, 32768).

-define(SSH_WINDOW_SIZE, 1024 * (?SSH_PACKET_SIZE)).

-record(state,
        {con :: pid(),
         log :: term(),
         channels :: map(),
         params :: map(),
         opts :: list()}).

-spec exec(pid(), string()) -> ok.

exec(Pid, Cmd) -> exec(Pid, Cmd, self(), result).

-spec exec(pid(), string(), pid() | term()) -> ok.

exec(Pid, Cmd, ReportP) ->
    exec(Pid, Cmd, ReportP, result).

-spec exec(pid(), string(), pid() | term(),
           atom()) -> ok.

exec(Pid, Cmd, ReportP, ReportType) ->
    gen_server:cast(Pid, {exec, Cmd, ReportP, ReportType}).

-spec get_file(pid(), string()) -> {ok, list(),
                                    binary} |
                                   {error, term()}.

get_file(Pid, Filename) ->
    gen_server:call(Pid, {get, Filename}, infinity).

-spec put_file(pid(), string(), iolist()) -> ok |
                                             {error, term()}.

put_file(Pid, Filename, Content) ->
    gen_server:call(Pid,
                    {put, Filename, Content},
                    infinity).

start(Params) -> gen_server:start(?MODULE, Params, []).

stop(Pid) -> gen_server:cast(Pid, stop).

init(#{user := User, host := Host, port := _Port,
       base_dir := BaseDir} =
         Params) ->
    process_flag(trap_exit, true),
    LogFD = case maps:get(log_file, Params, undefined) of
                undefined -> undefined;
                Filename ->
                    case file:open(Filename, [append, unicode]) of
                        {ok, FD} ->
                            file:write(FD,
                                       list_to_binary("\n[HOST] " ++ Host ++ "\n\n")),
                            FD;
                        Error ->
                            ?LOG_ERROR("Can't open log file ~p, ignored - ~p",
                                       [Filename, Error]),
                            undefined
                    end
            end,
    UserDir = case os:getenv("USER") of
                  "root" -> "/root/.ssh";
                  _ -> filename:join([priv_dir(), BaseDir, ".ssh"])
              end,
    Options = [{silently_accept_hosts, true},
               {user_interaction, false},
               {user_dir, UserDir},
               {user, User},
               {connect_timeout, ?SSH_CONNECT_TIMEOUT}],
    {ok,
     connect(#state{con = undefined, log = LogFD,
                    channels = #{}, params = Params, opts = Options})}.

priv_dir() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    filename:join(filename:dirname(Ebin), "priv").

%
% gen_server
%

handle_call(Msg, _From,
            #state{con = undefined, params = #{host := Host}} =
                State) ->
    ?LOG_ERROR("Can't process ~p for ~p - not connected",
               [Msg, Host]),
    {reply, {error, not_connected}, State};
handle_call({get, Filename}, _From,
            #state{con = Con} = State) ->
    R = sftp(Con,
             fun (Pid) ->
                     case ssh_sftp:read_file_info(Pid, Filename) of
                         {ok, Fileinfo} ->
                             case ssh_sftp:read_file(Pid, Filename) of
                                 {ok, Data} -> {ok, Fileinfo, Data};
                                 Error -> Error
                             end;
                         Error -> Error
                     end
             end),
    {reply, R, State};
handle_call({put, Filename, Content}, _From,
            #state{con = Con} = State) ->
    R = sftp(Con,
             fun (Pid) -> ssh_sftp:write_file(Pid, Filename, Content)
             end),
    {reply, R, State};
handle_call(_Msg, _From, State) -> {reply, ok, State}.

handle_cast({exec, Cmd, ReportP},
            #state{con = undefined, params = #{host := Host}} =
                State) ->
    ?LOG_ERROR("Can't execute ~p for ~p - not connected",
               [Cmd, Host]),
    report(ReportP, {error, not_connected}),
    {noreply, State};
handle_cast({exec, Cmd, ReportP, ReportType}, State) ->
    {noreply, execute(Cmd, ReportP, ReportType, State)};
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(connect, State) ->
    {noreply, connect(State)};
handle_info({ssh_cm, Con, Res}, State)
    when State#state.con =:= Con ->
    {noreply, process(Res, State)};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown info - ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{con = Con, log = Log}) ->
    case Con of
        undefined -> ok;
        _ -> ssh:close(Con)
    end,
    case Log of
        undefined -> ok;
        _ -> file:close(Log)
    end,
    ok;
terminate(_Reason, _) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
% local
%

connect(#state{params = #{host := Host, port := Port},
               opts = Options} =
            State) ->
    case ssh:connect(Host, Port, Options) of
        {ok, Con} -> State#state{con = Con};
        Any ->
            ?LOG_ERROR("Can't create SSH connection to ~p - ~p",
                       [Host, Any]),
            State#state{con = undefined, channels = #{}}
    end.

execute(Cmd, ReportP, ReportType,
        #state{con = Con, channels = Channels, log = Log} =
            State) ->
    case ssh_connection:session_channel(Con,
                                        ?SSH_WINDOW_SIZE,
                                        ?SSH_PACKET_SIZE,
                                        ?SSH_CHANNEL_TIMEOUT)
        of
        {ok, Ch} ->
            ssh_connection:exec(Con, Ch, Cmd, ?SSH_COMMAND_TIMEOUT),
            file:write(Log,
                       list_to_binary("\n[CMD] " ++ Cmd ++ "\n\n")),
            State#state{channels =
                            maps:put(Ch,
                                     #{cmd => Cmd, pid => ReportP, type => ReportType,
                                       streams => #{0 => <<>>, 1 => <<>>}},
                                     Channels)};
        {error, closed} ->
            ?LOG_ERROR("Command ~p was dropped cause channel "
                       "was closed",
                       [Cmd]),
            ?AFTER((?SSH_CONNECT_TIMEOUT), connect),
            State#state{con = undefined};
        Any ->
            ?LOG_ERROR("Can't get channel ~p", [Any]),
            State
    end.

process(Msg,
        #state{channels = Channels, log = Log} = State) ->
    [_, Ch | _] = tuple_to_list(Msg),
    case maps:get(Ch, Channels, undefined) of
        undefined ->
            ?LOG_ERROR("Unknown data ~p for channel ~p - no "
                       "such channel",
                       [Msg, Ch]),
            State;
        ChInfo ->
            State#state{channels =
                            maps:put(Ch, process_ch(Msg, ChInfo, Log), Channels)}
    end.

process_ch({data, _Ch, Stream, Data},
           #{pid := ReportP, type := online} = ChInfo,
           undefined) ->
    report(ReportP, {data, {Stream, Data}}),
    ChInfo;
process_ch({data, _Ch, Stream, Data},
           #{type := result, streams := Streams} = ChInfo,
           undefined) ->
    NewStreams = case maps:get(Stream, Streams, undefined)
                     of
                     undefined -> maps:put(Stream, Data, Streams);
                     PrevData ->
                         maps:put(Stream,
                                  <<PrevData/binary, Data/binary>>,
                                  Streams)
                 end,
    ChInfo#{streams := NewStreams};
process_ch({data, _Ch, _Stream, Data}, ChInfo, Log) ->
    file:write(Log, Data),
    ChInfo;
process_ch({eof, _Ch},
           #{pid := ReportP, type := online} = ChInfo, _) ->
    report(ReportP, eof),
    ChInfo;
process_ch({eof, _Ch}, ChInfo, _) -> ChInfo;
process_ch({exit_signal,
            _Ch,
            ExitSignal,
            ErrorMsg,
            _LanguageString},
           #{pid := ReportP, type := online} = ChInfo, _) ->
    report(ReportP, {signal, ExitSignal, ErrorMsg}),
    ChInfo;
process_ch({exit_status, _Ch, ExitStatus},
           #{pid := ReportP, type := online} = ChInfo, _) ->
    report(ReportP, {status, ExitStatus}),
    ChInfo;
process_ch({exit_signal,
            _Ch,
            ExitSignal,
            ErrorMsg,
            _LanguageString},
           #{pid := ReportP, type := result, streams := Streams} =
               ChInfo,
           _) ->
    ?LOG_INFO("Error ~p", [ErrorMsg]),
    report(ReportP, {done, {signal, ExitSignal}, Streams}),
    ChInfo;
process_ch({exit_status, _Ch, ExitStatus},
           #{pid := ReportP, type := result, streams := Streams} =
               ChInfo,
           _) ->
    report(ReportP, {done, {status, ExitStatus}, Streams}),
    ChInfo;
process_ch({closed, _Ch}, _ChInfo, _) -> undefined.

report(P, Msg) when is_pid(P) -> P ! {ssh, Msg};
report(P, Msg) -> ?PUB(P, {ssh, Msg}).

sftp(Con, F) ->
    case ssh_sftp:start_channel(Con) of
        {ok, Pid} ->
            Res = F(Pid),
            ok = ssh_sftp:stop_channel(Pid),
            {ok, Res};
        Err -> Err
    end.

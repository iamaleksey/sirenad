-module(sirenad_app).


-behaviour(application).


%% API exports
-export([get_env/1, get_env/2]).


%% application callback exports
-export([start/2, prep_stop/1, stop/1, config_change/3]).


-define(APP, sirenad).


%% -------------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------------


-spec get_env/1 :: (any()) -> any().

get_env(Key) ->
    element(2, application:get_env(?APP, Key)).


-spec get_env/2 :: (any(), any()) -> any().

get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.


%% -------------------------------------------------------------------------
%% application callback functions
%% -------------------------------------------------------------------------


start(normal, _StartArgs) ->
    ok = setup_logger(),
    log4erl:info("sirenad: starting up"),
    sirenad_sup:start_link().


%% This function is called when ?APP application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(St) ->
    log4erl:info("sirenad: stopping"),
    srn_client:stop(),
    timer:sleep(200), % leave a little time for http responses.
    St.


%% Perform necessary cleaning up *after* ?APP application has stopped.
stop(_St) ->
    log4erl:info("sirenad: stopped"),
    ok.


config_change(_Changed, _New, _Removed) ->
    ok.


%% -------------------------------------------------------------------------
%% private functions
%% -------------------------------------------------------------------------


setup_logger() ->
    FileAppenderSpec = {
        get_env(file_log_dir, ""),               % log dir.
        "sirenad",                               % log file name.
        {size, get_env(file_log_size, 5000000)}, % individual log size in bytes.
        get_env(file_log_rotations, 1),          % number of rotations.
        "log",                                   % suffix.
        get_env(file_log_level, none),           % minimum log level to log.
        "%j %T [%L] %l%n"                        % format.
    },
    log4erl:add_file_appender(default_logger_file, FileAppenderSpec),
    ConsoleAppenderSpec = {
        get_env(console_log_level, none), % minimum log level to log.
        "%j %T [%L] %l%n"                 % format.
    },
    log4erl:add_console_appender(default_logger_console, ConsoleAppenderSpec),
    ok.

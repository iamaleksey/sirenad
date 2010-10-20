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
    sirenad_sup:start_link().


%% This function is called when ?APP application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(St) ->
    srn_client:stop(),
    srn_http:stop(),
    St.


%% Perform necessary cleaning up *after* ?APP application has stopped.
stop(_St) ->
    ok.


config_change(_Changed, _New, _Removed) ->
    ok.

-module(sirenad_sup).


-behaviour(supervisor).


%% API exports
-export([start_link/0]).


%% suprevisor exports
-export([init/1]).


-define(GEN_CHILD_SPEC(Mod, Restart, Shutdown),
    {Mod, {Mod, start_link, []}, Restart, Shutdown, worker, [Mod]}).


%% -------------------------------------------------------------------------
%% API
%% -------------------------------------------------------------------------


-spec start_link/0 :: () -> {'ok', pid()} | 'ignore' | {'error', any()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% -------------------------------------------------------------------------
%% supervisor callback functions
%% -------------------------------------------------------------------------


init([]) ->
    ChildSpecs = [
        ?GEN_CHILD_SPEC(srn_client, permanent, 5000)
    ],
    {ok, {{one_for_one, 5, 30}, ChildSpecs}}.

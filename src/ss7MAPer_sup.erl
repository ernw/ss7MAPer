-module(ss7MAPer_sup).
-author('Daniel Mende <mail@c0decafe.de>').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
%~ -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Configfile) ->
    supervisor:start_link(?MODULE, [Configfile]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Configfile) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => ss7MAPer,
                    start => {ss7MAPer, start_link, Configfile},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [ss7MAPer]}],
    {ok, {SupFlags, ChildSpecs}}.

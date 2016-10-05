-module(ss7MAPer_app).
-author('Daniel Mende <mail@c0decafe.de>').

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Configfile) ->
    ss7MAPer_sup:start_link(Configfile).

stop(_State) ->
    ok.

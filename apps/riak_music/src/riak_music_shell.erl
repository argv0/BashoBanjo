-module(riak_music_shell).
-export([
         start/0,
         local_allowed/3, 
         non_local_allowed/3
        ]).
-include("riak_music.hrl").

start() ->
    spawn(fun() -> shell:start_restricted(?MODULE) end).

%%% LOCAL COMMANDS %%%

local_allowed(_,_,State) ->
    {true, State}.

%%% NON-LOCAL (MODULE) COMMANDS %%%

non_local_allowed({sync, go}, [], State) ->
    {true, State};

non_local_allowed(_,_,State) ->
    {false, State}.


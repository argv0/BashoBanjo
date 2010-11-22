-module(user_default).
-export([help/0, play/1, stop/0, members/0, join/1]).

help() ->
    io:format([
               "The Amazing Basho Banjo:\n",
               "  help()     - Display this screen.\n",
               "  members()    - View nodes currently in the cluster.\n",
               "  play(Note) - Play a midi note. (45 to 127)\n",
               "  play(File) - Play a midi file.\n",
               "  stop()     - Stop playing all music.\n",
               "  q()              - Quit.\n\n"
              ]).
        

play(Thing) ->
    riak_music:play(Thing).

stop() ->
    riak_music:stop().

members() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    riak_core_ring:all_members(Ring).

join(NodeToJoin) ->
    case net_adm:ping(NodeToJoin) of
        pong ->
            riak_core_gossip:send_ring(NodeToJoin, node()),
            io:format("Joining to node ~p~n", [NodeToJoin]),
            ok;
        pang ->
            io:format("Could not find node ~p~n", [NodeToJoin]),
            error
    end.


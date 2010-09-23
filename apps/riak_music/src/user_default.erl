-module(user_default).
-export([help/0, play/1, stop/0]).

help() ->
    io:format([
               "The Amazing Riakophone:\n",
               "  help()     - Display this screen.\n",
               "  play(Note) - Play a midi note. (45 to 127)\n",
               "  play(File) - Play a midi file.\n",
               "  stop()     - Stop playing all music.\n",
               "  q()              - Quit.\n\n"
              ]).
        

play(Thing) ->
    riak_music:play(Thing).

stop() ->
    riak_music:stop().

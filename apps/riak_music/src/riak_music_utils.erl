-module(riak_music_utils).
-export([detect_audio_method/0]).

detect_audio_method() ->
    AFPlayExists = filelib:wildcard("/usr/bin/afplay") /= [],
    APlayExists = filelib:wildcard("/usr/bin/aplay") /= [],
    if 
        AFPlayExists -> 
            afplay;
        APlayExists -> 
            aplay;
        true -> 
            undefined
    end.

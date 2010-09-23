-module(riak_music_utils).
-export([detect_audio_method/0]).

detect_audio_method() ->
    AFPlayExists = filelib:wildcard("/usr/bin/afplay") /= [],
    SoundDeviceExists = filelib:wildcard("/dev/sound") /= [],
    AudioDeviceExists = filelib:wildcard("/dev/audio") /= [],
    if 
        AFPlayExists -> 
            afplay;
        SoundDeviceExists -> 
            sound_device;
        AudioDeviceExists -> 
            audio_device;
        true -> 
            undefined
    end.

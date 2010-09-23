-module(riak_music).
-export([
         play/1,
         stop/0, 
         stop/1,
         join/0
        ]).

-include("riak_music.hrl").

join() ->
    riak_core_gossip:send_ring('rphone1@192.168.1.137', node()).

%% Play the supplied filename.
play(Filename) when is_list(Filename)->
    %% Play the tune...
    Callback = fun play_note/4,
    {ok, Pid} = riak_music_midi:play(Filename, Callback),

    %% Store our pid in process dictionary for easy stopping later.
    case erlang:get(playing_pids) of
        undefined -> erlang:put(playing_pids, [Pid]);
        Pids -> erlang:put(playing_pids, [Pid|Pids])
    end,
    %% Return the pid.
    Pid;

%% Play the specified midi note.
play(MidiNote) when is_integer(MidiNote) ->
    play_note(1, MidiNote, 1, 1).

stop() ->
    case erlang:get(playing_pids) of
        undefined -> 
            ok;
        Pids -> 
            [stop(X) || X <- Pids]
    end,
    erlang:put(playing_pids, []),
    ok.

stop(Pid) ->
    riak_music_midi:stop(Pid).

%% Private Functions

play_note(MidiController, MidiNote, Amplitude, Duration) 
  when MidiNote >= 0 andalso MidiNote =< 127,
       Amplitude >= 0 andalso Amplitude =< 1 ->
    %% Route based on the MidiController/MidiNote combination...
    ObjectName = {MidiController, MidiNote},
    Key = chash:key_of(term_to_binary(ObjectName)),

    %% Get the preflist...
    NVal = 1,
    PrefList = riak_core_apl:get_apl(Key, NVal, riak_music),

    %% Send the play command...
    Message = {play, MidiController, MidiNote, Amplitude, Duration},
    riak_core_vnode_master:command(PrefList, Message, riak_music_vnode_master),
    ok;
play_note(_, _, _, _) ->
    ok.

    
    

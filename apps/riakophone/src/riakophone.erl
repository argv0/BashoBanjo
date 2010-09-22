-module(riakophone).
-export([
         play/1,
         stop/0, 
         stop/1
        ]).

-include("riakophone.hrl").

%% Play the supplied filename.
play(Filename) when is_list(Filename)->
    {ok, Notes, MSPerTick} = riakophone_midi:read(Filename),
    Pid = spawn(fun() -> play_notes(Notes, MSPerTick, 0) end),
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
    Pid ! stop.

%% Private Functions

play_notes([Note|Notes], MSPerTick, Ticks) ->
    %% Delay the proper amount of ticks...
    case Ticks < Note#note.start of
        true ->
            Sleep = trunc(MSPerTick * (Note#note.start - Ticks)),
            timer:sleep(Sleep);
        false ->
            ok
    end,

    %% Play the next note...
    Controller = Note#note.controller,
    MidiNote = Note#note.note,
    Amplitude = Note#note.amplitude,
    Duration = (MSPerTick * Note#note.length) / 1000,
    play_note(Controller, MidiNote, Amplitude, Duration),

    %% Check if we have been stopped...
    receive stop -> 
            stopped
    after 0 ->
            play_notes(Notes, MSPerTick, Note#note.start)
    end;
play_notes([], _, _) -> 
    ok.
    
play_note(MidiController, MidiNote, Amplitude, Duration) ->
    %% Route based on the MidiController/MidiNote combination...
    ObjectName = {MidiController, MidiNote},
    Key = chash:key_of(term_to_binary(ObjectName)),

    %% Get the preflist...
    NVal = 1,
    PrefList = riak_core_apl:get_apl(Key, NVal, riakophone),

    %% Send the play command...
    Message = {play, MidiController, MidiNote, Amplitude, Duration},
    riak_core_vnode_master:command(PrefList, Message, riakophone_vnode_master).

    
    

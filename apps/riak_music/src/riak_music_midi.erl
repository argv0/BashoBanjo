-module(riak_music_midi).
-export([play/2, stop/1]).
-include("riak_music.hrl").

play(Filename, Callback) ->
    {ok, Notes, MSPerTick} = read_midi(Filename),
    Pid = spawn(fun() -> play_notes(Notes, MSPerTick, 0, Callback) end),
    {ok, Pid}.

stop(Pid) ->
    Pid ! stop.

read_midi(Filename) ->
    {seq, {header, _, Time}, GlobalTrack, Tracks} = midifile:read(Filename),
    case <<Time:16/integer>> of
        <<0:1/integer, TicksPerBeat:15/integer>> ->
            ok;
        <<1:1/integer, _:15/integer>> ->
            throw(not_yet_supported),
            TicksPerBeat = 0
    end,
    MSPerTick = calculate_ms_per_tick(TicksPerBeat, GlobalTrack),
    Notes = transform_tracks([GlobalTrack|Tracks]),
    {ok, Notes, MSPerTick}.



calculate_ms_per_tick(TicksPerBeat, {track, TrackInfo}) ->
    {tempo, _, [MicroSecPerQuarterNote]} = lists:keyfind(tempo, 1, TrackInfo),
    (MicroSecPerQuarterNote * (1 / TicksPerBeat)) / 1000.

transform_tracks(Tracks) ->
    lists:sort(lists:flatten([transform_track(X) || X <- Tracks])).
transform_track({track, Events}) ->
    %% Filter out old events, calculate absolute times and relative velocities.
    Events1 = transform_events1(Events, 0),
    %% Sort by Note/Time
    Events2 = transform_events2(Events1),
    %% Collapse on/off into notes.
    Events3 = transform_events3(Events2),
    lists:sort(Events3).
transform_events1([{OnOff, DeltaTime, [Controller, Note, Velocity]}|Rest], Ticks) when OnOff == 'on' orelse OnOff == 'off' ->
    NewTicks = Ticks+DeltaTime,
    NewEvent = {OnOff, NewTicks, Controller, Note, Velocity/127 * 0.8},
    [NewEvent|transform_events1(Rest, NewTicks)];
transform_events1([{program, _, [9, _]}|_Rest], _Ticks) ->
    %% Don't play percussion.
    [];
transform_events1([_|Rest], Ticks) ->
    transform_events1(Rest, Ticks);
transform_events1([], _) ->
    [].

transform_events2(Events) ->
    SortFun = fun({_, TicksA, _, NoteA, _}, {_, TicksB, _, NoteB, _}) ->
                   {NoteA, TicksA} < {NoteB, TicksB}
              end,
    lists:sort(SortFun, Events).
transform_events3([On,Off|Rest]) when is_record(On, on) andalso
                                      is_record(Off, off) andalso
                                      On#on.note == Off#off.note andalso 
                                      On#on.controller == Off#off.controller ->
    Note = #note {
      start = On#on.ticks,
      length = Off#off.ticks - On#on.ticks,
      controller = On#on.controller,
      note = On#on.note,
      amplitude = On#on.amplitude
     },
    [Note|transform_events3(Rest)];
transform_events3([_|Rest]) ->
    transform_events3(Rest);
transform_events3([]) ->
    [].

play_notes([Note|Notes], MSPerTick, Ticks, Callback) ->
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
    Callback(Controller, MidiNote, Amplitude, Duration),

    %% Check if we have been stopped...
    receive stop -> 
            stopped
    after 0 ->
            play_notes(Notes, MSPerTick, Note#note.start, Callback)
    end;
play_notes([], _, _, _) -> 
    ok.

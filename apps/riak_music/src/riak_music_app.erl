-module(riak_music_app).
-export([start/2, stop/1]).
-behaviour(application).
-include("riak_music.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Ensure that we can detect an audio playing method, or abort.
    case riak_music_utils:detect_audio_method() of
        undefined ->
            error_logger:error_msg("~n~n~n~n~n~nERROR: Could not find method to play audio!~n~n~n~n~n~n"),
            init:stop();
        Method ->
            error_logger:info_msg("Playing audio with: '~s'~n", [Method])
    end,

    %% Start the riak_music supervisor. If successful, then register
    %% with riak_core.
    case riak_music_sup:start_link() of
        {ok, Pid} ->
            %% Register our vnode module with Riak Core.
            riak_core:register_vnode_module(riak_music_vnode),
            riak_core_node_watcher:service_up(riak_music, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

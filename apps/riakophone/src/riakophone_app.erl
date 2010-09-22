-module(riakophone_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Start any dependent applications.
    riak_core_util:start_app_deps(riakophone),

    %% Ensure that we can detect an audio playing method, or abort.
    case riakophone_utils:detect_audio_method() of
        undefined ->
            error_logger:error_msg("~n~n~n~n~n~nERROR: Could not find method to play audio!~n~n~n~n~n~n"),
            init:stop();
        Method ->
            error_logger:info_msg("Playing audio with: '~s'~n", [Method])
    end,

    %% Start the riakophone supervisor. If successful, then register
    %% with riak_core.
    case riakophone_sup:start_link() of
        {ok, Pid} ->
            riak_core:register_vnode_module(riakophone_vnode),
            riak_core_node_watcher:service_up(riakophone, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

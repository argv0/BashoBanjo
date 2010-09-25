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

            %% Add webmachine route to reflect an IP address. This
            %% should technically be in autoname, but it has to happen
            %% here because we need to start it after Riak Core starts...
            Route = {["ip"], autoname_wm_ipaddress, []},
            webmachine_router:add_route(Route),

            %% %% Automatically join to the cluster. This is here for
            %% %% demonstration purposes, normally this would be done on
            %% %% the command line, and it really only needs to be done
            %% %% once, not every time you start the application.
            {ok, AutoJoinNode} = application:get_env(riak_music, auto_join),
            riak_core_gossip:send_ring(node(), AutoJoinNode),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

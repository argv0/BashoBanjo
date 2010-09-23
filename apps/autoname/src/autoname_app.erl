-module(autoname_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Create the name...
    {ok, NodeName} = application:get_env(autoname, nodename),

    %% Get the ip address...
    {ok, Hostname} = inet:gethostname(),
    {ok, {A,B,C,D}} = inet:getaddr(Hostname, inet),

    %% Create the node name...
    S1 = io_lib:format("~s@~w.~w.~w.~w", [NodeName, A, B, C, D]),
    S2 = lists:flatten(S1),
    S3 = list_to_atom(S2),
    error_logger:info_msg("~n~nSetting node name to '~s'.~n~n", [S3]),

    %% Attempt to start net_kernel...
    case net_kernel:start([S3, longnames]) of
        {ok, _Pid} ->
            autoname_sup:start_link();
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

-module(autoname_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% Ensure epmd is started...
    io:format("Ensuring epmd is started...~n"),
    os:cmd("epmd -daemon"),
    timer:sleep(1000),
    
    %% Create the name...
    Name = get_node_name(),

    %% Get the ip address...
    IPAddress = get_ipaddress(),

    %% Create the node name...
    S3 = list_to_atom(Name ++ "@" ++ IPAddress),
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


get_node_name() ->
    case application:get_env(autoname, nodename) of
        {ok, NodeName} -> 
            NodeName;
        _ ->
            string_clean(os:cmd("whoami"))
    end.

string_clean([H|T]) when H >= $a andalso H =< $z; H >= $A andalso H =< $Z ->
    [H|string_clean(T)];
string_clean([_|T]) ->
    string_clean(T);
string_clean([]) ->
    [].
            

get_ipaddress() ->
    try
        inets:start(),
        {ok, IPUrl} = application:get_env(autoname, ip_url),
        {ok, {{_, 200, _}, _, IPAddress}} = http:request('get', {IPUrl, []}, [], [{full_result, true}]),
        IPAddress
    catch _ : _ ->
            {ok, Hostname} = inet:gethostname(),
            {ok, {A,B,C,D}} = inet:getaddr(Hostname, inet),
            lists:flatten(io_lib:format("~p.~p.~p.~p", [A,B,C,D]))
    end.

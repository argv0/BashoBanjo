-module(autoname_wm_ipaddress).

%% webmachine resource exports
-export([
         init/1,
         to_html/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.

to_html(ReqData, Ctx) ->
    IP = wrq:peer(ReqData),
    {IP, ReqData, Ctx}.

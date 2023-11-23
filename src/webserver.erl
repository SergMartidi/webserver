%% @author smartov
%% @doc @todo Add description to webserver.


-module(webserver).

-export([start/0]).

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(webserver),
    ok.



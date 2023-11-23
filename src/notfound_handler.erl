%% @author smartov
%% @doc @todo Add description to notfound_handler.


-module(notfound_handler).

-behavior(cowboy_rest).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Callback Callbacks
-export([notfound_from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->  
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, notfound_from_json}
    ], Req, State}.

notfound_from_json(Req, State) ->
    {[], utils:reply(404, <<"Page not found">>, Req), State}.


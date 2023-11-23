%% @author smartov
%% @doc @todo Add description to logout_handler.


-module(logout_handler).
-behavior(cowboy_rest).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).

%% Callback Callbacks
-export([logout_from_json/2]).


%% Cowboy REST callbacks
init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}. 

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, logout_from_json}
    ], Req, State}.

resource_exists(Req, State) ->
  {false, Req, State}.

is_authorized(Req, State) ->
    case utils:auth(Req) of
        {false, Req1} ->
            {{false, <<"Unauthorized">>}, Req1, State};
        {true, _User, Req1} ->
            {true, Req1, State}
    end.

logout_from_json(Req, State) ->
    {ok, Req1} = cowboy_session:expire(Req),
    {true, Req1, State}.



%% @author smartov
%% @doc @todo Add description to login_handler.


-module(login_handler).
-behavior(cowboy_rest).


%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).


%% Callback Callbacks
-export([login_from_json/2]).

-include ("test_block.hrl").

%% Cowboy REST callbacks
init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}. 

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, login_from_json}
    ], Req, State}.

resource_exists(Req, State) ->
  {false, Req, State}.


login_from_json(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
    try
        {ok, Input, _} = ?assertmatch (utils:get_body(Body, Req1), {ok, _, _}),
         Model = [
             {<<"email">>, required, string, email, [non_empty,
                 fun(V) ->
                     validator:email(V)
                 end
             ]},
             {<<"pass">>, required, string, pass, [non_empty, 
                 fun(V) -> 
                     validator:min_length(6, V)
                 end
             ]}
         ],
         {ok, Emodel} = ?assertmatch (utils:get_model(Input, Model, Req1), {ok, _}),
         {ok, User, Req2} = ?assertmatch (login(Emodel, Req1), {ok, _, _}),
         {true, utils:reply(200, User, Req2), State}
    catch
        _:{error, {error, EReply}} -> {false, EReply, State};
        _:Error -> io:format ("Error: ~p~n", [Error])
    end.


login(Data, Req) ->
    case utils:auth(Req) of
        {false, Req1} ->
            Email = maps:get(email, Data),
            Pass = maps:get(pass, Data),
            case database:get_user(pgdb, Email, utils:pwd2hash(Pass)) of
                none ->
                    {error, utils:reply(412, <<"These credentials do not match our records.">>, Req1)};
                {ok, User} ->
                    {ok, Req2} = cowboy_session:set(<<"user">>, User, Req1),
                    {ok, User, Req2}
            end;
        {true, _User, Req1} -> {error, utils:reply(400, <<"User already authorized">>, Req1)}
    end.


%% @author smartov
%% @doc @todo Add description to register_handler.


-module(register_handler).
-behavior(cowboy_rest).

%% REST Callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Callback Callbacks
-export([register_from_json/2]).
-export([register_from_text/2]).

-include ("test_block.hrl").

%% Cowboy REST callbacks
init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}. 

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, register_from_text},
        {<<"text/html">>, register_from_text}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, register_from_json}
    ], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> {true, Req, State};
        <<"POST">> -> {false, Req, State}
    end.

register_from_json(Req, State)  ->
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
            ]},
            {<<"fname">>, required, string, fname, [non_empty]},
            {<<"lname">>, required, string, lname, [non_empty]}                
        ],
        {ok, Emodel} = ?assertmatch (utils:get_model(Input, Model, Req1), {ok, _}),
        {ok, User, Req2} = ?assertmatch (registration(Emodel, Req1), {ok, _, _}),
        {true, utils:reply(200, User, Req2), State}
    catch
        _:{error, {error, EReply}} -> {false, EReply, State};
        _:Error -> io:format ("Error: ~p~n", [Error])
    end.

register_from_text(Req, State) ->
    #{token := Token} = cowboy_req:match_qs([{token, nonempty, undefined}], Req),
    try
       ?assert (Token /= undefined, {token_undefined, Req}),
       Session = {Register, Req1} = cowboy_session:get(<<"register">>, Req),
       ?assertnotmatch (Session, {undefined, _}, token_expired),
       SToken = maps:get(token, Register),
       ?assert (SToken =:= Token, {token_wrong, Req1}),
       {ok, Req2} = cowboy_session:set(register, undefined, Req1),
       ?assert (database:check_user(pgdb, maps:get(email, Register)) == false, {user_exists, Req2}),
       Email = maps:get(email, Register),
       Pass = maps:get(pass, Register),
       Fname = maps:get(fname, Register),
       Lname = maps:get(lname, Register),
       ?assert (database:add_user(pgdb, Email, Fname, Lname, Pass) == {ok, 1}, {cannot_add_user, Req2}),
       User = #{email => Email, fname => Fname, lname => Lname},
       {ok, Req3} = cowboy_session:set(<<"user">>, User, Req2),
       {jiffy:encode(User), Req3, State}
    catch
        _:{error, {token_undefined, EReq}} -> {[], utils:reply(400, <<"No token">>, EReq), State};
        _:{error, {token_expired, {undefined, EReq}}} -> {[], utils:reply(400, <<"Token expired">>, EReq), State};
        _:{error, {token_wrong, EReq}} -> {[], utils:reply(400, <<"Wrong token!">>, EReq), State};
        _:{error, {user_exists, EReq}} -> {[], utils:reply(400, <<"User already exists">>, EReq), State};
        _:{error, {cannot_add_user, EReq}} -> {[], utils:reply(500, <<"Cannot add new user in database">>, EReq), State};
        _:Error -> io:format ("Error: ~p~n", [Error])
    end.


registration(Data, Req) ->
    io:format ("registration: Data: ~p~n", [Data]),
    case utils:auth(Req) of
        {false, Req1} ->
            case database:check_user(pgdb, maps:get(email, Data)) of
                false ->
                    Pass = maps:get(pass, Data),
                    Register_safe_pass = maps:update(pass, utils:pwd2hash(Pass), Data),
                    Register = maps:put(token, utils:get_token(), Register_safe_pass),
                    {ok, Req2} = cowboy_session:set(<<"register">>, Register, Req1),
                    {ok, Register, Req2};
                _ ->
                    {error, utils:reply(400, <<"User already exists">>, Req1)}
            end;
        {true, _User, Req1} -> {error, utils:reply(400, <<"User already authorized">>, Req1)}
    end.




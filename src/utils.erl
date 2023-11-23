%% @author smartov
%% @doc @todo Add description to utils.


-module(utils).

-export([get_body/2]).
-export([get_model/3]).
-export([reply/3]).
-export([pwd2hash/1]).
-export([get_token/0]).
-export([auth/1]).
-export([already_auth/1]).


get_body(Body, Req) ->
    case Body of
        [{Input, true}] ->
            {ok, Input, Req};
        [] ->
            {error, reply(400, <<"Missing body">>, Req)};
        _ ->
            {error, reply(400, <<"Bad request">>, Req)}
    end.

get_model(Input, Model, Req) ->
            try
                Data = jiffy:decode(Input, [return_maps]),
                    case  emodel:from_map(Data, #{}, Model) of
                        {error, Reason} -> {error, reply(412, {Reason}, Req)};
                        Res -> Res
                    end
            catch
                _:_ ->
                    {error, reply(400, <<"Invalid json">>, Req)}
            end.

reply(Code, Body, Req) ->
    cowboy_req:reply(Code, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Body), Req).

pwd2db(MD5Bin) ->
    list_to_binary(smd5(<<MD5Bin/binary, (application:get_env(erl, salt, <<"boombang">>))/binary>>)).

pwd2hash(Bin) when is_binary(Bin) ->
    pwd2hash(binary_to_list(Bin));

pwd2hash(L) ->
    pwd2db(list_to_binary(smd5(L))).

smd5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)]).

get_token() ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(rand:uniform(ChrsSize), Chrs) | R] end,
    list_to_binary(lists:foldl(F, "", lists:seq(1, 64))).

auth(Req) ->
    case cowboy_session:get(<<"user">>, Req) of
        {undefined, Req1} ->
            {false, Req1};
        {User, Req1} ->
            {true, User, Req1}
    end.

already_auth(Req) ->
    case auth(Req) of
        {true, User, Req1} ->
            {true, Req1};
        {false, Req1} ->
            {false, Req1}
    end.

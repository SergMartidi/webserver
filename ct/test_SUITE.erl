%% @author smartov
%% @doc @todo Add description to test_SUITE.


-module(test_SUITE).

-include_lib ("common_test/include/ct.hrl").

-include ("test_block.hrl").

-export ([all/0,
          groups/0,
          init_per_testcase/2,
          end_per_testcase/2,
          init_per_group/2,
          end_per_group/2,
          db_add_user/1,
          register/1,
          login/1,
          not_found/1]).



all () -> [{group, one}].

groups () -> [{one, [], [db_add_user, register, login, not_found]}].

init_per_group (one, Config) ->
    ct:pal ("init_per_group~n"),
    application:ensure_all_started (webserver),
    pgapp:connect(pgdb, [{database, "erl"}, {username, "postgres"}, {password, "52754318"}, {size, 5}, {max_overflow, 20}, {host, localhost}]),
    Config;

init_per_group (_, Config) ->
    Config.

end_per_group (one, _Config) ->
    database:init_db (pgdb),
    application:stop (webserver),
    ok;

end_per_group (_, _Config) ->
    ok.

init_per_testcase (_, Config) ->
    database:init_db (pgdb),
    Config.

%end_per_testcase (test, _Config) ->
%    test:stop (),
%    ok;

end_per_testcase (_, _Config) ->
    ok.

db_add_user (_Config) ->
    ct:pal ("db_add_user~n"),
    {ok, 1} = database:add_user(pgdb, "a@b.com", "Fname", "Lname", "Pass").

register (_Config) ->
  ct:pal ("Registration without info~n"),
  ct:pal ("curl -s -b cookiejar -c cookiejar -i -H \"Content-Type: application/json\" -X POST" ++
                    " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'}~n"),
  Output = os:cmd ("curl -s -b cookiejar -c cookiejar -i -H \"Content-Type: application/json\" -X POST" ++
                    " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} "),
  ct:pal ("~s~n", [Output]),
  [] = Output,

  ct:pal ("Registration without fname~n"),
  ct:pal ("curl -s -c cookiejar -i -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"lname\":\"Lastname\",\"pass\":\"xyzXYZ\"}'" ++
                    " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'}~n"),
  Output1 = os:cmd ("curl -s -c cookiejar -i -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"lname\":\"Lastname\",\"pass\":\"xyzXYZ\"}'" ++
                    " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} "),
  ct:pal ("~s~n", [Output1]),
  [] = Output1,

  ct:pal ("Normal registration"),
  JSON = "{\"email\":\"x@xyz.com\",\"fname\":\"Firstname\",\"lname\":\"Lastname\",\"pass\":\"xyzXYZ\"}",
  ct:pal ("curl -s -c cookiejar -i -H \"Content-Type: application/json\" -X POST -d '" ++ JSON ++ "'" ++
                   " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'}~n"),
  Output2 = os:cmd ("curl -s -c cookiejar -i -H \"Content-Type: application/json\" -X POST -d '" ++ JSON ++ "'" ++
                   " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} "),
  Token = string:trim (Output2, both, "\"\n"),
  ct:pal ("Token: ~s~n", [Token]),
  64 = length (Token),

  ct:pal ("Registration request with no cookie~n"),
  ct:pal ("curl -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/register?token=" ++ Token),
  Output3 = os:cmd ("curl -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/register?token=" ++ Token),
  ct:pal ("~s~n", [Output3]),
  ?assert (string:find (Output3, "400 Bad Request") /= nomatch, error),
  ?assert (string:find (Output3, "Token expired") /= nomatch, error),

  ct:pal ("Registration request with cookie~n"),
  ct:pal ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/register?token=" ++ Token),
  Output4 = os:cmd ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/register?token=" ++ Token),
  ct:pal ("~s~n", [Output4]),
  ?assert (string:find (Output4, "HTTP/1.1 200 OK") /= nomatch, error),
  JSON2 = lists:last (string:split (Output4, "\n", all)),
  Map = jiffy:decode (JSON, [return_maps]),
  MapNoPass = maps:remove (<<"pass">>, Map),
  Map2 = jiffy:decode (JSON2, [return_maps]),
  MapNoPass = Map2.

login (_Config) ->
  ct:pal ("Hello page without authorization~n"),
  ct:pal ("curl -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  Output = os:cmd ("curl -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  ct:pal ("~s~n", [Output]),
  ?assert (string:find (Output, "HTTP/1.1 200 OK") /= nomatch, error),
  ?assert (string:find (Output, "[\"hello\",\"Good day, stranger\"]") /= nomatch, error),

  ct:pal ("Registration~n"),
  JSON = "{\"email\":\"x@xyz.com\",\"fname\":\"Firstname\",\"lname\":\"Lastname\",\"pass\":\"xyzXYZ\"}",
  ct:pal ("curl -s -c cookiejar -i -H \"Content-Type: application/json\" -X POST -d '" ++ JSON ++ "'" ++
                   " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} "),
  Output2 = os:cmd ("curl -s -c cookiejar -i -H \"Content-Type: application/json\" -X POST -d '" ++ JSON ++ "'" ++
                   " http://localhost:8080/register | grep 'token' | awk -F ',' {'print $1'} | awk -F ':' {'print $2'} "),
  Token = string:trim (Output2, both, "\"\n"),
  ct:pal ("Token: ~s~n", [Token]),
  64 = length (Token),

  ct:pal ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/register?token=" ++ Token),
  Output3 = os:cmd ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/register?token=" ++ Token),
  ct:pal ("~s~n", [Output3]),
  ?assert (string:find (Output3, "HTTP/1.1 200 OK") /= nomatch, error),
  JSON2 = lists:last (string:split (Output3, "\n", all)),
  Map = jiffy:decode (JSON, [return_maps]),
  MapNoPass = maps:remove (<<"pass">>, Map),
  Map2 = jiffy:decode (JSON2, [return_maps]),
  MapNoPass = Map2,

  ct:pal ("Hello page after registration~n"),
  ct:pal ("curl -i -b cookiejar -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  Output4 = os:cmd ("curl -i -b cookiejar -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  ct:pal ("~s~n", [Output4]),
  ?assert (string:find (Output4, "HTTP/1.1 200 OK") /= nomatch, error),
  Map = jiffy:decode (JSON, [return_maps]),
  FName = binary_to_list (maps:get (<<"fname">>, Map)),
  LName = binary_to_list (maps:get (<<"lname">>, Map)),
  ?assert (string:find (Output4, "[\"hello\",\"Good day, " ++  FName ++ " " ++ LName ++ "\"]") /= nomatch, error),

  ct:pal ("Login after authorization~n"),
  ct:pal ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"xyzXYZ\"}' http://localhost:8080/login"),
  Output4_1 = os:cmd ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"xyzXYZ\"}' http://localhost:8080/login"),
  ct:pal ("~s~n", [Output4_1]),
  ?assert (string:find (Output4_1, "HTTP/1.1 400 Bad Request") /= nomatch, error),
  ?assert (string:find (Output4_1, "User already authorized") /= nomatch, error),

  ct:pal ("Logout~n"),
  ct:pal ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"xyz\",\"password\":\"xyz\"}' http://localhost:8080/logout"),
  Output5 = os:cmd ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"xyz\",\"password\":\"xyz\"}' http://localhost:8080/logout"),
  ct:pal ("~s~n", [Output5]),
  ?assert (string:find (Output5, "HTTP/1.1 204 No Content") /= nomatch, error),

  ct:pal ("Logout with no logging before~n"),
  ct:pal ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"xyz\",\"password\":\"xyz\"}' http://localhost:8080/logout"),
  Output6 = os:cmd ("curl -b cookiejar -i -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"xyz\",\"password\":\"xyz\"}' http://localhost:8080/logout"),
  ct:pal ("~s~n", [Output6]),
  ?assert (string:find (Output6, "HTTP/1.1 401 Unauthorized") /= nomatch, error),

  ct:pal ("Hello page after logout~n"),
  ct:pal ("curl -i -b cookiejar -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  Output7 = os:cmd ("curl -i -b cookiejar -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  ct:pal ("~s~n", [Output7]),
  ?assert (string:find (Output7, "HTTP/1.1 200 OK") /= nomatch, error),
  ?assert (string:find (Output7, "[\"hello\",\"Good day, stranger\"]") /= nomatch, error),

  ct:pal ("Login with wrong password~n"),
  ct:pal ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"yyyyyy\"}' http://localhost:8080/login"),
  Output8 = os:cmd ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"yyyyyy\"}' http://localhost:8080/login"),
  ct:pal ("~s~n", [Output8]),
  ?assert (string:find (Output8, "HTTP/1.1 412 Precondition Failed") /= nomatch, error),
  ?assert (string:find (Output8, "These credentials do not match our records.") /= nomatch, error),

  ct:pal ("Login with right credentials~n"),
  ct:pal ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"xyzXYZ\"}' http://localhost:8080/login"),
  Output9 = os:cmd ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"xyzXYZ\"}' http://localhost:8080/login"),
  ct:pal ("~s~n", [Output9]),
  ?assert (string:find (Output9, "HTTP/1.1 200 OK") /= nomatch, error),
  JSON3 = lists:last (string:split (Output9, "\n", all)),
  Map3 = jiffy:decode (JSON3, [return_maps]),
  Id = maps:get (<<"id">>, Map3),
  ?assert (is_integer (Id), error),
  Map3NoId = maps:remove (<<"id">>, Map3),
  MapNoPass = Map3NoId,

  ct:pal ("Hello page after login~n"),
  ct:pal ("curl -i -b cookiejar -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  Output10 = os:cmd ("curl -i -b cookiejar -H \"Content-Type: application/json\" -X GET http://localhost:8080/"),
  ct:pal ("~s~n", [Output10]),
  ?assert (string:find (Output10, "HTTP/1.1 200 OK") /= nomatch, error),
  ?assert (string:find (Output10, "[\"hello\",\"Good day, " ++  FName ++ " " ++ LName ++ "\"]") /= nomatch, error),
  ct:pal ("Login when already been logged~n"),
  Output11 = os:cmd ("curl -i -b cookiejar -c cookiejar -H \"Content-Type: application/json\" -X POST -d '{\"email\":\"x@xyz.com\",\"pass\":\"xyzXYZ\"}' http://localhost:8080/login"),
  ?assert (string:find (Output11, "HTTP/1.1 400 Bad Request") /= nomatch, error),
  ?assert (string:find (Output11, "User already authorized") /= nomatch, error).

not_found (_Config) ->
  ct:pal ("Not found page~n"),
  ct:pal ("curl -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/fff"),
  Output = os:cmd ("curl -i -H \"Content-Type: application/json\" -X GET http://localhost:8080/fff"),
  ct:pal ("~s~n", [Output]),
  ?assert (string:find (Output, "HTTP/1.1 404 Not Found") /= nomatch, error),
  ?assert (string:find (Output, "Page not found") /= nomatch, error).
































   
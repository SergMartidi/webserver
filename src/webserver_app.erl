%% @author smartov
%% @doc @todo Add description to webserver_app.


-module(webserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([         
            {'_', [
                {"/", index_handler, []},
                {"/register", register_handler, []},
                {"/login", login_handler, []},
                {"/logout", logout_handler, []},
                {'_', notfound_handler, []}
            ]}     
    ]),     
    {ok, _} = cowboy:start_clear(listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}
    }),
    webserver_sup:start_link().

stop(_State) ->
    ok.




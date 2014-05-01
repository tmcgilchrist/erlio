-module(erlio).
-export([start/0,
         start_link/0,
         stop/0]).

%% @doc Starts the app for inclusion in a supervisor tree
-spec start_link() -> {ok,Pid::pid()}.
start_link() ->
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(lager),
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_log),
    ensure_started(webmachine),
    erlio_sup:start_link().

%% @doc Start the erlio server.
-spec start() -> ok.
start() ->
    ensure_started(compiler),
    ensure_started(syntax_tools),
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module,
                        webmachine_log),
    ensure_started(webmachine),
    application:start(erlio).

%% @doc Stop the erlio server.
-spec stop() -> ok.
stop() ->
    Res = application:stop(erlio),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    application:stop(syntax_tools),
    application:stop(compiler),
    Res.

%% Private

%% @doc Ensure application is started.
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

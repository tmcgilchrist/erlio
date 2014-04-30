-module(erlio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% Setup Webmachine
    Ip =  "0.0.0.0",
    Dispatch = load_wm_resources(),
    io:format("Dispatch: [~p]~n", [Dispatch]),
    WebConfig = [ {ip, Ip},
                  {port, 8000},
                  {log_dir, "priv/log"},
                  {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    Store = {erlio_store,
             {erlio_store, start_link, []},
             permanent, 5000, worker, [erlio_store]},

    Children = [Web, Store],

    {ok, {{one_for_one, 1, 1}, Children}}.


load_wm_resources() ->
    Resources = [erlio_link_resource],
    lists:flatten([Module:routes() || Module <- Resources]).

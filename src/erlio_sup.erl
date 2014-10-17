-module(erlio_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

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
    Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
    Dispatch = load_wm_resources(),
    P = case os:getenv("PORT") of false -> "8000"; _Any -> _Any end,
    Port = list_to_integer(P),
    WebConfig = [ {ip, Ip},
                  {port, Port},
                  {log_dir, "priv/log"},
                  {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},

    Store = {erlio_store,
             {erlio_store, start_link, []},
             permanent, 5000, worker, [erlio_store]},

    %% This is the publish-subscribe process, a gen_event.
    Events = {erlio_stats,
              {erlio_stats, start_link, []},
              permanent, 5000, worker, [erlio_stats]},

    Children = [Web, Store, Events],

    {ok, {{one_for_one, 1, 1}, Children}}.

load_wm_resources() ->
    Resources = [erlio_links_resource
                 , erlio_link_resource
                 , erlio_stats_resource
                 , erlio_assets_resource
                ],
    lists:flatten([Module:routes() || Module <- Resources]).

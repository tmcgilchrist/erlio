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
    Web = #{id => webmachine_mochiweb,
            start => {webmachine_mochiweb, start, [erlio_config:web_config()]},
            restart => permanent, 
            shutdown => 5000, 
            type => worker, 
            modules => [mochiweb_socket_server]},

    Store = #{id => erlio_store,
              start => {erlio_store, start_link, []},
              restart => permanent, 
              shutdown => 5000, 
              type => worker, 
              modules => [erlio_store]},

    %% This is the publish-subscribe process, a gen_event.
    Events = #{id => erlio_stats,
               start => {erlio_stats, start_link, []},
               restart => permanent, 
               shutdown => 5000, 
               type => worker, 
               modules => [erlio_stats]},

    Children = [Web, Store, Events],
    
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 3600
                },
    {ok, {SupFlags, Children}}.

 %% p 137

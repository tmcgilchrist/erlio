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
    _ = create_table(),
    _ = seed_links(),

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

    Children = [Web],

    {ok, {{one_for_one, 1, 1}, Children}}.


create_table() ->
    ets:new(links, [public,
                    ordered_set,
                    named_table,
                    {read_concurrency, true},
                    {write_concurrency, true}]).

seed_links() ->
    ets:insert(links, [
                       {"1",
                        [{url, <<"http://erlang.com">>},
                         {hits, <<"0">>}]},
                       {"2",
                        [{url, <<"http://www.openbsd.org">>},
                         {hits, <<"0">>}]}
                       ]).


load_wm_resources() ->
    Resources = [erlio_link_resource],
    lists:flatten([Module:routes() || Module <- Resources]).

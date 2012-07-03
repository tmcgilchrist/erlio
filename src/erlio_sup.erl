
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
    Ip =  "0.0.0.0",
    {ok, Dispatch} = file:consult(filename:join(
                                    [filename:dirname(code:which(?MODULE)),
                                     "..", "priv", "dispatch.conf"])),
    WebConfig = [ {ip, Ip},
                  {port, 8000},
                  {log_dir, "priv/log"},
                  {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    {ok, { {one_for_one, 5, 10}, [Web]} }.

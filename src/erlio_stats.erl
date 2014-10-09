%% erlio_stats provides the API for
-module(erlio_stats).

-behaviour(gen_event).

-export([start_link/0, new_link/1, access_link/1, add_handler/0]).

-export([init/1, handle_event/2,
         handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

new_link(Link) ->
    gen_event:notify(?MODULE, {link_added, Link}).

access_link(Link) ->
    gen_event:notify(?MODULE, {link_accessed, Link}).

add_handler() ->
    %% Using add_sup_handler here so we get notified if
    %% there are errors in this event_handler.
    %% Otherwise the handler is sliently removed and we wouldn't know what's going on.
    gen_event:add_sup_handler(?MODULE, ?MODULE, [self()]).

init(_Args) ->
    add_handler(),
    {ok, #state{}}.

%% Add extra pattern matches here for new messages.
handle_event({link_added, Url}, State) ->
    io:format("*** Link Added: [~p] ***~n", [Url]),
    {ok, State};
handle_event({link_accessed, Url}, State) ->
    io:format("*** Link Accessed: [~p] ***~n", [Url]),
    {ok, State};
handle_event(Msg, State) ->
    io:format("***Message*** ~p~n", [Msg]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_, _) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

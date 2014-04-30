-module(erlio_store).

-behaviour(gen_server).

-export([start/0, start_link/0, lookup_link/1, link_exists/1, create_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3,
         handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          %% store persistent data store details on here.
}).


%% =========================================================================================
%% Public API
%% =========================================================================================

start() ->
    gen_server:start(?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, erlio_store}, ?MODULE, [], []).

lookup_link(Id) ->
    gen_server:call(erlio_store, {lookup_link, Id}).

link_exists(Id) ->
    gen_server:call(erlio_store, {link_exists, Id}).

create_link(Link) ->
    gen_server:call(erlio_store, {create_link, Link}).

%% =========================================================================================
%% gen_server Callbacks
%% =========================================================================================

init([]) ->
    %% Create table and setup some seed data
    _ = create_table(),
    _ = seed_links(),

    {ok, #state{}}.
handle_call({lookup_link, Id}, _From, State) ->
    case ets:lookup(links, Id) of
        [] ->
            Reply = {not_found},
            {reply, Reply, State};
        [{_Key, Link}] ->
            Reply = {ok, Link},
            {reply, Reply, State}
    end;
handle_call({link_exists, Id}, _From, State) ->
    case ets:lookup(links, Id) of
        [] ->
            {reply, false, State};
        [{_Key, _Link}] ->
            {reply, true, State}
    end;
handle_call({create_link, Link}, _From, State) ->
    ets:insert(links, [Link]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

create_table() ->
    ets:new(links, [public,
                    ordered_set,
                    named_table,
                    {read_concurrency, true},
                    {write_concurrency, true}]).

seed_links() ->
    ets:insert(links, [
                       {generate_id("http://erlang.com"),
                        [{url, <<"http://erlang.com">>},
                         {hits, <<"0">>}]},
                       {generate_id("http://www.openbsd.org"),
                        [{url, <<"http://www.openbsd.org">>},
                         {hits, <<"0">>}]}
                       ]).

generate_id(Url) ->
    crypto:hash_init(md5),
    B = crypto:hash(md5, Url),
    mochihex:to_hex(B).

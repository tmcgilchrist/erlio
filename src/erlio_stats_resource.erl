-module(erlio_stats_resource).

%% webmachine callbacks
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2, to_stream/2]).

%% API
-export([routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

%% =========================================================================================
%% API functions
%% =========================================================================================

routes() ->
    [{["stats"], ?MODULE, []}].

%% =========================================================================================
%% webmachine Callbacks
%% =========================================================================================

init([]) ->
    {ok, #context{}}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json},
      {"text/event-stream", to_stream}], ReqData, Context}.

to_json(ReqData, Context) ->
    Response = mochijson2:encode(erlio_store:all_links()),
    {Response, ReqData, Context}.

to_stream(ReqData, Context) ->
    case erlio_stats:add_handler() of
        ok ->
            {{stream, {<<>>, fun stream/0}},
             ReqData, Context};
        _ ->
            {{halt, 500}, ReqData, Context}
    end.

stream() ->
    receive
        %% The gen_event crashed or exited, so we should kill the stream
        {gen_event_EXIT, erlio_stats, _} ->
            {<<>>, done};

        %% We added a new link
        {new_link, {Id, Link}=_Msg} ->
            Response = Link ++ [{id, time_to_timestamp(Id)}],
            io:format("Response: [~p]~n", [Response]),
            Body = io_lib:format("id: ~s~ndata: ~s~n~n",
                                 [Id, "Some string"]),
            {Body, fun stream/0}
    end.

time_to_timestamp({Mega, Sec, Micro}) ->
    Time = Mega * 1000000 * 1000000 + Sec * 1000000 + Micro,
    list_to_binary(integer_to_list(Time)).

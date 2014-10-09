-module(erlio_link_resource).

%% webmachine callbacks
-export([init/1, content_types_provided/2, allowed_methods/2, to_json/2, resource_exists/2]).

%% API
-export([routes/0]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

%% =========================================================================================
%% API functions
%% =========================================================================================

routes() ->
    [{["link", link_id], ?MODULE, []}].

%% =========================================================================================
%% webmachine Callbacks
%% =========================================================================================

init([]) ->
    {ok, #context{}}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

resource_exists(ReqData, _Context) ->
    Id = wrq:path_info(link_id, ReqData),
    NewContext = #context{id=Id},

    {erlio_store:link_exists(Id), ReqData, NewContext}.

to_json(ReqData, Context = #context{id=Id}) ->
    case erlio_store:lookup_link(Id) of
        {ok, Link} ->
            Response = mochijson2:encode({struct, Link}),
            {Response, ReqData, Context};
        {not_found} ->
            Response = mochijson2:encode({struct, []}),
            {Response, ReqData, Context}
    end.

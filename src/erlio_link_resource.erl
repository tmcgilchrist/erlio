-module(erlio_link_resource).

-export([init/1, routes/0, content_types_provided/2, allowed_methods/2, to_json/2, resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {link}).

init([]) ->
    %% Do initialisation stuff here
    {ok, #context{}}.

routes() ->
    [{["link", link_id], ?MODULE, []}]. %% GET /link/:id

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) ->
    Id = wrq:path_info(link_id, ReqData),
    %% TODO This could be cached on the #context
    {erlio_store:link_exists(Id), ReqData, Context}.

to_json(ReqData, Context) ->
    Id = wrq:path_info(link_id, ReqData),
    case erlio_store:lookup_link(Id) of
        {ok, Link} ->
            Response = mochijson2:encode({struct, Link}),
            {Response, ReqData, Context};
        {not_found} ->
            Response = mochijson2:encode({struct, []}),
            {Response, ReqData, Context}
    end.

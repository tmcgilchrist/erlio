-module(erlio_links_resource).

-export([init/1, routes/0,
         allowed_methods/2, post_is_create/2, content_types_accepted/2,
         allow_missing_post/2,
         create_path/2, from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id, url}).

init([]) ->
    {ok, #context{}}.

routes() ->
    [{["links"], ?MODULE, []}]. %% GET /link/:id

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/json", from_json}], ReqData, Context}.

create_path(ReqData, _Context) ->
    Attributes = wrq:req_body(ReqData),
    {struct,[{<<"url">>, Url}]} = mochijson2:decode(Attributes),
    Id = generate_id(Url),
    Resource = "/link/" ++ Id,

    NewContext = #context{id=Id, url=Url},
    {Resource, ReqData, NewContext}.

from_json(ReqData, Context = #context{id=Id, url=Url}) ->
    Link = {Id,
            [{url, Url},
             {hits, <<"0">>}]},
    erlio_store:create_link(Link),
    {true, ReqData, Context}.

allow_missing_post(ReqData, Context) ->
    {true, ReqData, Context}.

generate_id(Url) ->
    crypto:hash_init(md5),
    B = crypto:hash(md5, Url),
    mochihex:to_hex(B).

-module(erlio_short_resource).

-export([init/1, routes/0, resource_exists/2, previously_existed/2, moved_temporarily/2]).
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {}).

init([]) ->
    {ok, #context{}}.

routes() ->
    [{['*'], ?MODULE, []}].

resource_exists(ReqData, State) ->
    {false, ReqData, State }.

previously_existed(ReqData, State) ->
    Key = get_key(ReqData),
    {erlio_store:link_exists(Key), ReqData, State}.

moved_temporarily(ReqData, State) ->
    Key = get_key(ReqData),
    {ok, Link} = erlio_store:lookup_link(Key),
    Url = binary_to_list(proplists:get_value(url, Link)),
    {{halt, 302},
     wrq:set_resp_header("Location", Url, ReqData),
     State}.

get_key(ReqData) ->
    binary_to_list(iolist_to_binary(remove_slash(wrq:path(ReqData)))).

remove_slash(Path) ->
    re:replace(Path, "^\/", "").

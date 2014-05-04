-module(erlio_short_resource).

-export([init/1, routes/0, resource_exists/2, previously_existed/2, moved_temporarily/2, to_html/2]).
-include_lib("webmachine/include/webmachine.hrl").

-record(context, {id}).

init([]) ->
    {{trace, "traces"}, #context{}}.  %% debugging code
    %% {ok, #context{}}.  %% regular code

routes() ->
    [{["dev", "wmtrace", '*'], wmtrace_resource, [{trace_dir, "traces"}]},
     {['*'], ?MODULE, []}].

resource_exists(ReqData, _Context) ->
    Id = get_key(ReqData),
    NewContext = #context{id=Id},
    Exists = index_request(ReqData),
    {Exists, ReqData, NewContext }.

previously_existed(ReqData, Context=#context{id=Id}) ->
    {erlio_store:link_exists(Id), ReqData, Context}.

moved_temporarily(ReqData, Context=#context{id=Id}) ->
    {ok, Link} = erlio_store:lookup_link(Id),
    Url = binary_to_list(proplists:get_value(url, Link)),
    {{halt, 302},
     wrq:set_resp_header("Location", Url, ReqData),
     Context}.

get_key(ReqData) ->
    binary_to_list(iolist_to_binary(remove_slash(wrq:path(ReqData)))).

remove_slash(Path) ->
    re:replace(Path, "^\/", "").

index_request(ReqData) ->
   wrq:disp_path(ReqData) =:= [].

to_html(ReqData, Context) ->
    {ok, App} = application:get_application(?MODULE),
    Filename = filename:join([priv_dir(App), "www"] ++ ["index.html"]),
    {ok, Source} = file:read_file(Filename),
    {Source, ReqData, Context}.

%% @doc Extract the priv dir for the application.
-spec priv_dir(term()) -> list().
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, bad_name} ->
            Ebin = filename:dirname(code:which(Mod)),
            filename:join(filename:dirname(Ebin), "priv");
        PrivDir ->
            PrivDir
    end.

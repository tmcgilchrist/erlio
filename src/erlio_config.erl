-module(erlio_config).

-export([
    dispatch/0,
    web_config/0
]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    Resources = [erlio_links_resource
                 , erlio_link_resource
                 , erlio_stats_resource
                 , erlio_assets_resource
                ],
    lists:flatten([Module:routes() || Module <- Resources]).


web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch()}
    ].

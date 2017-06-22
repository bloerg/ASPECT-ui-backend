-module(aui_database_helpers).

-export([get_db_connection/0]).

get_db_connection() ->
    {ok, DB_connection} = epgsql:connect(
        ets:lookup_element(app_configuration, aui_db_host, 2), 
        ets:lookup_element(app_configuration, aui_db_user, 2), 
        ets:lookup_element(app_configuration, aui_db_password, 2), 
        [
            {database, 
             ets:lookup_element(app_configuration, aui_db_name, 2)
            }, 
            {timeout, 10000}
        ]),
    {ok, DB_connection}.

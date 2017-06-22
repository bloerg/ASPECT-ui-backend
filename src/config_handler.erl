-module(config_handler).
-behavior(cowboy_handler).

-export([
	init/2
]).


% get entry for som_id from map_configurations table and return list
% of selected configuration parameter
get_som_config_from_db(DB_connection, SOM_id) -> 
    {ok,[{column,<<"max_zoom">>,int4,4,-1,1},
	 {column,<<"max_x">>,int4,4,-1,1},
	 {column,<<"max_y">>,int4,4,-1,1},
	 {column,<<"som_description">>,varchar,-1,260,1}],
	Configuration}
    = epgsql:equery(DB_connection, "select max_zoom,max_x, max_y, som_description from map_configurations where som_id = $1", [SOM_id]),
    case Configuration of 
	[{Max_zoom,Max_x,Max_y,Som_description}] ->
	    {ok, 
		[
		    {max_zoom, Max_zoom}, 
		    {max_x, Max_x}, 
		    {max_y, Max_y}, 
		    {som_description, 
		    binary_to_list(Som_description)}
		]
	    };
	_Else ->
	    {error, som_does_not_exist}
    end.
	    

init(Req0, State) ->
    {ok, DB_connection} = aui_database_helpers:get_db_connection(),
    SOM_id = cowboy_req:binding(som_id, Req0), 

    
    case get_som_config_from_db(DB_connection, SOM_id) of
		{ok, [{max_zoom, Max_zoom}, {max_x, Max_x}, {max_y, Max_y}, {som_description, Som_description}]} ->
			_ = epgsql:close(DB_connection),
			{ok,
			cowboy_req:reply(
				200,
				#{<<"content-type">> => <<"application/json">>},
				list_to_binary(
					"{" ++ 
					string:join( 
						[   "\"min_x\":0,",
						    "\"min_y\":0,",
						    "\"max_zoom\":", io_lib:format("~tp", [Max_zoom]), ",",
						    "\"base_directory\":", "\"./\",",
						    "\"tile_size\":256,",
						    "\"max_x\":", io_lib:format("~tp", [Max_x]), ",",
						    "\"max_y\":", io_lib:format("~tp", [Max_y]), ",",
						    "\"page_title\":", io_lib:format("~tp", [Som_description])
						],
						""
					) ++
					"}"
				),
				Req0
			),
			State
			};
			
		_Else ->
			_ = epgsql:close(DB_connection),
			{ok,
			cowboy_req:reply(
				404,
				Req0
			),
			State
			}
	end.

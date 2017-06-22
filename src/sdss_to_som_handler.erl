-module(sdss_to_som_handler).
-behavior(cowboy_handler).

-export([
	init/2
]).


% get entry for som_id from map_configurations table and return list
% of selected configuration parameter
get_som_coordinates_from_db(DB_connection, {SOM_id, MJD, Plate, Fiberid}) -> 
    {ok,[{column,<<"som_x">>,int4,4,-1,1},
	 {column,<<"som_y">>,int4,4,-1,1}],
	SOM_coordinates}
    = epgsql:equery(
	DB_connection, 
	string:join([
	    "SELECT som_x, som_y FROM ", 
	    "SOM_", integer_to_list(SOM_id), 
	    " WHERE mjd = $1 and plate =$2 and fiberid=$3"
	], ""), 
	[MJD, Plate, Fiberid]
    ),
    case SOM_coordinates of 
	[{SOM_x,SOM_y}] ->
	    {ok, 
		[
		    {som_x, SOM_x}, 
		    {som_y, SOM_y}
		]
	    };
	_Else ->
	    []
    end.
	    

init(Req0, State) ->
    {ok, DB_connection} = aui_database_helpers:get_db_connection(),
    {SOM_id, MJD, Plate, Fiberid} = {
	cowboy_req:binding(som_id, Req0),
	cowboy_req:binding(mjd, Req0),
	cowboy_req:binding(plate, Req0),
	cowboy_req:binding(fiberid, Req0)
    },

    
    case get_som_coordinates_from_db(DB_connection, {SOM_id, MJD, Plate, Fiberid}) of
		{ok, [{som_x, SOM_x}, {som_y, SOM_y}]} ->
			_ = epgsql:close(DB_connection),
			{ok,
			cowboy_req:reply(
				200,
				#{<<"content-type">> => <<"application/json">>},
				list_to_binary(
					string:join( 
						[   "{",
						    "\"som_x\":", io_lib:format("~tp", [SOM_x]), ",",
						    "\"som_y\":", io_lib:format("~tp", [SOM_y]),
						    "}"
						],
						""
					)
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

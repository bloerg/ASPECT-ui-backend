-module(metadata_handler).
-behavior(cowboy_handler).

-export([
	init/2
]).


% get entry for som_id from map_configurations table and return list
% of selected configuration parameter
get_spec_metadata_from_db(DB_connection, {SOM_id, SOM_x, SOM_y}) -> 
    {ok,[{column,<<"mjd">>,int4,4,-1,1},
	 {column,<<"plate">>,int4,4,-1,1},
	 {column,<<"fiberid">>,int4,4,-1,1}],
	Spec_metadata}
    = epgsql:equery(
	DB_connection, 
	string:join([
	    "SELECT mjd, plate, fiberid FROM ", 
	    "SOM_", integer_to_list(SOM_id), 
	    " WHERE som_x = $1 and som_y =$2"
	], ""), 
	[SOM_x, SOM_y]
    ),
    case Spec_metadata of 
	[{MJD,Plate,Fiberid}] ->
	    {ok, 
		[
		    {mjd, MJD}, 
		    {fiberid, Fiberid}, 
		    {plate, Plate}
		]
	    };
	_Else ->
	    {error, som_does_not_exist}
    end.
	    

init(Req0, State) ->
    {ok, DB_connection} = aui_database_helpers:get_db_connection(),
    {SOM_id, SOM_x, SOM_y} = {
	cowboy_req:binding(som_id, Req0),
	cowboy_req:binding(som_x, Req0),
	cowboy_req:binding(som_y, Req0)
    },

    
    case get_spec_metadata_from_db(DB_connection, {SOM_id, SOM_x, SOM_y}) of
		{ok, [{mjd, MJD}, {fiberid, Fiberid}, {plate, Plate}]} ->
			_ = epgsql:close(DB_connection),
			{ok,
			cowboy_req:reply(
				200,
				#{<<"content-type">> => <<"application/json">>},
				list_to_binary(
					"{" ++ 
					string:join( 
						[   "\"mjd\":", io_lib:format("~tp", [MJD]), ",",
						    "\"plate\":", io_lib:format("~tp", [Plate]), ",",
						    "\"fiberid\":", io_lib:format("~tp", [Fiberid])
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

-module(specs_exist_handler).
-behavior(cowboy_handler).

-export([
	init/2
]).

get_existence_from_db(DB_connection, {SOM_x, SOM_y}, SOM_zoom, _Max_zoom, SOM_id) ->

   {ok,[{column,<<"tile_exists">>,{array,int4},_,_,-1,1}],Result}
         = epgsql:equery(
            DB_connection, 
	    string:join([
		"SELECT tile_exists
		FROM mv_datalayers_som_", integer_to_list(SOM_id), "
		WHERE tile_x = $1 and tile_y = $2 and zoom = $3"
	    ], ""),
        [
	    SOM_x,
	    SOM_y,
	    SOM_zoom
        ]
        ),
    case Result of 
	[{Existences}] when is_list(Result) -> Existences;
	_Else -> []
    end
.



% tiles of lower zoom levels consist of a number of subtiles
% get those tiles from the data base
% expect parameters to be valid input since these were subject
% to verification in the router
get_subtile_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) -> 
    {ok,[{column,<<"max_zoom">>,int4,_,_,-1,1}],[{Max_zoom}]} = epgsql:equery(DB_connection, "select max_zoom from map_configurations where som_id = $1", [SOM_id]),
    Existences = get_existence_from_db(DB_connection, {SOM_x, SOM_y}, SOM_zoom, Max_zoom, SOM_id),
    {ok, Existences}.

init(Req0, State) ->
    {ok, DB_connection} = aui_database_helpers:get_db_connection(),
    {SOM_id, SOM_x, SOM_y, SOM_zoom} = {
        cowboy_req:binding(som_id, Req0),
        cowboy_req:binding(som_x, Req0),
        cowboy_req:binding(som_y, Req0),
        cowboy_req:binding(som_zoom, Req0)
    },
    
    case get_subtile_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) of
		{ok, Subtile_data} ->
			_ = epgsql:close(DB_connection),
			{ok,
			cowboy_req:reply(
				200,
				#{<<"content-type">> => <<"application/json">>},
				list_to_binary(
					Subtile_data
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

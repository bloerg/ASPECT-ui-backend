-module(sn_median_handler).
-behavior(cowboy_handler).

-export([
	init/2
	%~ ,get_subtile_data_from_db/5 % for database debugging
	%~ ,get_redshifts_from_db/3 % for database debugging
]).


get_sn_medians_from_db(DB_connection, {SOM_x, SOM_y}, SOM_zoom, SOM_id) ->
    %~ Real_edge_length = trunc(math:pow(2, (Max_zoom - SOM_zoom))),
    
    {ok,[{column,<<"sn_median">>,{array,float4},_,_,-1,1}],Result}
         = epgsql:equery(
            DB_connection, 
	    string:join([
		"SELECT sn_median
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
	[{SN_medians}] when is_list(SN_medians) -> SN_medians;
	_Else -> []
    end
    .



% tiles of lower zoom levels consist of a number of subtiles
% get those tiles from the data base
% expect parameters to be valid input since these were subject
% to verification in the router
get_subtile_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) -> 
    SN_medians = get_sn_medians_from_db(DB_connection, {SOM_x, SOM_y}, SOM_zoom, SOM_id),
    {ok, SN_medians}.

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
					"[" ++ 
					string:join( 
						[io_lib:format("~tp", [X]) || X <- Subtile_data ],
						","
					) ++
					"]"
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

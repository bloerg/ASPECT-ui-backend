-module(specs_exist_handler).
-behavior(cowboy_handler).

-export([
	init/2
]).

%~ get_existence_from_db_old(DB_connection, {SOM_x, SOM_y}, SOM_zoom, Max_zoom, SOM_id) ->
    %~ Real_edge_length = trunc(math:pow(2, (Max_zoom - SOM_zoom))),

    %~ {ok,[{column,<<"exists">>,{array,int4},-1,-1,1}],[{Existences}]}
         %~ = epgsql:equery(
            %~ DB_connection, 
            %~ string:join([
		%~ "WITH 
		   %~ all_subtiles AS (
		     %~ SELECT * FROM
		     %~ (generate_series(", integer_to_list(Real_edge_length*SOM_x) , ",",integer_to_list(Real_edge_length*SOM_x + Real_edge_length - 1),") as som_x
		     %~ CROSS JOIN generate_series(", integer_to_list(Real_edge_length*SOM_y), ",", integer_to_list(Real_edge_length*SOM_y + Real_edge_length - 1), ") as som_y)
		   %~ ),
		   %~ existing_subtiles AS (
		     %~ SELECT 1 as exists, som_x, som_y FROM ", "som_", integer_to_list(SOM_id), " where som_x between $1 and $2 and som_y between $3 and $4 and plate > 0
		   %~ )
		   %~ SELECT array_agg(exists) as exists FROM (
		   %~ SELECT COALESCE(exists,0) as exists FROM all_subtiles LEFT OUTER JOIN existing_subtiles USING (som_x, som_y) ORDER BY som_x, som_y ) as temp"
	    %~ ],""), 
        %~ [
            %~ Real_edge_length*SOM_x, 
            %~ Real_edge_length*SOM_x + Real_edge_length - 1,
            %~ Real_edge_length*SOM_y, 
            %~ Real_edge_length*SOM_y + Real_edge_length - 1
        %~ ]
        %~ ),
    %~ Existences
    %~ .

get_existence_from_db(DB_connection, {SOM_x, SOM_y}, SOM_zoom, _Max_zoom, SOM_id) ->

   {ok,[{column,<<"tile_exists">>,{array,int4},-1,-1,1}],Result}
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

%~ get_existence_from_db(DB_connection, Tile_coordinates, SOM_id) ->
    %~ {SOM_x, SOM_y} = Tile_coordinates,
    
    %~ {ok,[{column,<<"exists">>,int4,4,-1,1}],
        %~ Values} = epgsql:equery(
            %~ DB_connection, 
            %~ string:join(["select 1 as exists from ", SOM_id, " as id where som_x = $1 and som_y = $2"],""), 
            %~ [SOM_x,SOM_y]
        %~ ),
    %~ case Values of
        %~ [{1}] -> 1;
        %~ [] -> 0
    %~ end
    %~ .


% tiles of lower zoom levels consist of a number of subtiles
% get those tiles from the data base
% expect parameters to be valid input since these were subject
% to verification in the router
get_subtile_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) -> 
    {ok,[{column,<<"max_zoom">>,int4,4,-1,1}],[{Max_zoom}]} = epgsql:equery(DB_connection, "select max_zoom from map_configurations where som_id = $1", [SOM_id]),
    % how many subtiles are there in x or y dimension
    %~ Real_edge_length = trunc(math:pow(2, (Max_zoom - SOM_zoom))),
    %~ Subtile_som_coordinates = [ {X, Y} ||
        %~ X <- lists:seq(Real_edge_length*SOM_x, Real_edge_length*SOM_x + Real_edge_length - 1),
        %~ Y <- lists:seq(Real_edge_length*SOM_y, Real_edge_length*SOM_y + Real_edge_length - 1)
    %~ ],
    %~ Existences = [ get_existence_from_db(DB_connection, Tile_coordinates, SOM_id) || Tile_coordinates <- Subtile_som_coordinates],
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
					%~ "[" ++ 
					%~ string:join( 
						%~ [io_lib:format("~tp", [X]) || X <- Subtile_data ],
						%~ ","
					%~ ) 
					Subtile_data
					%~ ++ "]"
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

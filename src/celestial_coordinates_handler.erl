-module(celestial_coordinates_handler).
-behavior(cowboy_handler).

-export([
	init/2
	,get_celestial_coordinates_from_db/5
]).


get_celestial_coordinates_from_db(DB_connection, {SOM_x, SOM_y}, _SOM_zoom, Max_zoom, SOM_id) ->
    {ok,[
        {column,<<"ra_deg">>,{array,float8},_,_,-1,1},
        {column,<<"dec_deg">>,{array,float8},_,_,-1,1}
    ],
    Result
    }
     = epgsql:equery(
        DB_connection, 
            string:join([
            "SELECT ra_deg, dec_deg
            FROM mv_datalayers_som_", integer_to_list(SOM_id), "
            WHERE tile_x = $1 and tile_y = $2 and zoom = $3"
        ], ""),
        [
            SOM_x,
            SOM_y,
            Max_zoom
        ]
    ),
    case Result of
        [{[Ra_deg], [Dec_deg]}] -> [{ra_deg, Ra_deg}, {dec_deg, Dec_deg}];
        _Else -> []
    end
    .

% tiles of lower zoom levels consist of a number of subtiles
% get those tiles from the data base
% expect parameters to be valid input since these were subject
% to verification in the router
get_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) -> 
    {ok,[{column,<<"max_zoom">>,int4,_,_,-1,1}],[{Max_zoom}]} = epgsql:equery(DB_connection, "select max_zoom from map_configurations where som_id = $1", [SOM_id]),
    Celestial_coordinates = get_celestial_coordinates_from_db(DB_connection, {SOM_x, SOM_y}, SOM_zoom, Max_zoom, SOM_id),
    {ok, Celestial_coordinates}.

init(Req0, State) ->
    {ok, DB_connection} = aui_database_helpers:get_db_connection(),
    {SOM_id, SOM_x, SOM_y, SOM_zoom} = {
        cowboy_req:binding(som_id, Req0),
        cowboy_req:binding(som_x, Req0),
        cowboy_req:binding(som_y, Req0),
        cowboy_req:binding(som_zoom, Req0)
    },
    
    case get_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) of
        {ok, [{ra_deg, Ra_deg}, {dec_deg, Dec_deg}]} ->
            _ = epgsql:close(DB_connection),
            {ok,
            cowboy_req:reply(
                200,
                #{<<"content-type">> => <<"application/json">>},
                list_to_binary(
                    "{" ++ 
                    string:join( 
                        [   "\"ra_deg\":", io_lib:format("~tp", [Ra_deg]), ",",
                            "\"dec_deg\":", io_lib:format("~tp", [Dec_deg])
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

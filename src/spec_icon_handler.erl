-module(spec_icon_handler).
-behavior(cowboy_handler).

% for file_info record
-include_lib("kernel/include/file.hrl").


-export([
    init/2
    %~ info/3,
]).


%~ generate_tiles_with_R(Subtile_data) ->
    %~ MJDs = string:join(lists:map(fun erlang:integer_to_list/1, [MJD || {MJD, _, _} <- Subtile_data ]), ","),
    %~ Plates = string:join(lists:map(fun erlang:integer_to_list/1, [Plate || {_, Plate, _} <- Subtile_data ]), ","),
    %~ Fiberids = string:join(lists:map(fun erlang:integer_to_list/1, [Fiberid || {_, _, Fiberid} <- Subtile_data ]), ","),
    %~ <<"{ok,",_Tile_hash_from_R:64/binary,"}", _Tail/binary>> = list_to_binary(
        %~ os:cmd(
            %~ string:join(
                %~ [   "nice -n 19 ", 
                    %~ "/home/aspectui/aspectui_spectra_generator/aspectuiplot.R --mjd ",
                    %~ MJDs,
                    %~ " --plate ",
                    %~ Plates,
                    %~ " --fiberid ",
                    %~ Fiberids
                %~ ],
                %~ ""
            %~ )
        %~ )
    %~ ),
    %~ % decrease plotter counter by one after generating the tile
    %~ ets:insert(limits, {running_plotters, ets:lookup_element(limits, running_plotters, 2)  - 1})
%~ .

generate_tiles(Subtile_data) ->
    MJDs = string:join(lists:map(fun erlang:integer_to_list/1, [MJD || {MJD, _, _} <- Subtile_data ]), " "),
    Plates = string:join(lists:map(fun erlang:integer_to_list/1, [Plate || {_, Plate, _} <- Subtile_data ]), " "),
    Fiberids = string:join(lists:map(fun erlang:integer_to_list/1, [Fiberid || {_, _, Fiberid} <- Subtile_data ]), " "),
    <<"{ok,",_Tile_hash:64/binary,"}", _Tail/binary>> = list_to_binary(
    os:cmd(
            string:join(
                [   "nice -n 19 ", 
                    "/usr/local/bin/aspectuiplot --mjd ",
                    MJDs,
                    " --plate ",
                    Plates,
                    " --fiberid ",
                    Fiberids
                ],
                ""
            )
        )
    )
.

get_spec_ids_from_db(DB_connection, Tile_coordinates, SOM_id) ->
    {SOM_x, SOM_y} = Tile_coordinates,
    {ok,[{column,<<"mjd">>,int4,4,-1,1},
         {column,<<"plate">>,int4,4,-1,1},
         {column,<<"fiberid">>,int4,4,-1,1}],
        Values} = epgsql:equery(
            DB_connection, 
            string:join([
                "select mjd,plate,fiberid from ", 
                "som_", integer_to_list(SOM_id), 
                " where som_x=$1 and som_y=$2"],
            ""), 
            [SOM_x,SOM_y]
        ),
    case Values of
        [{MJD, Plate, Fiberid}] -> {MJD, Plate, Fiberid};
        [] -> {0,0,0}
    end
    .


% tiles of lower zoom levels consist of a number of subtiles
% get those tiles from the data base
% expect parameters to be valid input since these were subject
% to verification in the router
get_subtile_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y) -> 
    {ok,[{column,<<"max_zoom">>,int4,4,-1,1}],[{Max_zoom}]} = epgsql:equery(DB_connection, "select max_zoom from map_configurations where som_id = $1", [SOM_id]),
    % how many subtiles are there in x or y dimension
    Real_edge_length = trunc(math:pow(2, (Max_zoom - SOM_zoom))),
    Subtile_som_coordinates = [ {X, Y} ||
        X <- lists:seq(Real_edge_length*SOM_x, Real_edge_length*SOM_x + Real_edge_length - 1),
        Y <- lists:seq(Real_edge_length*SOM_y, Real_edge_length*SOM_y + Real_edge_length - 1)
    ],
    Spec_ids = [ get_spec_ids_from_db(DB_connection, Tile_coordinates, SOM_id) || Tile_coordinates <- Subtile_som_coordinates],
    {ok, Spec_ids}.


hash_subtile_data(Subtile_data) ->
    Subtile_data_string = lists:concat(
        lists:flatten(
            [[MJD,Plate,Fiberid] || {MJD,Plate,Fiberid} <- Subtile_data]
        )
    ),
    %crypto:start().
    <<X:256/big-unsigned-integer>> = crypto:hash(sha256, Subtile_data_string),
    lists:flatten(io_lib:format("~64.16.0b", [X]))
.

init(Req0, State) ->
    
    {ok, DB_connection} = aui_database_helpers:get_db_connection(),
        
    {SOM_id, SOM_x, SOM_y, SOM_zoom} = {
        cowboy_req:binding(som_id, Req0),
        cowboy_req:binding(som_x, Req0),
        cowboy_req:binding(som_y, Req0),
        cowboy_req:binding(som_zoom, Req0)
    },
    {ok, Subtile_data} = get_subtile_data_from_db(DB_connection, SOM_id, SOM_zoom, SOM_x, SOM_y),
    ok = epgsql:close(DB_connection),   

    Tile_hash = hash_subtile_data(Subtile_data),
    Filename = lists:concat(["/home/aspectui/spec_icons/", Tile_hash, ".png"]),
    Temp_filename = lists:concat(["/home/aspectui/spec_icons/", Tile_hash, ".png.tmp"]),
    
    % check if the tile is in the process of beeing generated
    case file:read_file_info(Temp_filename) of 
        {ok, _Temp_file_info} ->
            timer:sleep(1000),
            {
                ok, 
                cowboy_req:reply(202,
                    #{<<"content-type">> => <<"text/plain">>},
                    %~ <<"tile already requested, beeing generated, try again">>,
                    Req0
                ),
                State
            };
        {error, enoent} ->
            generate_tiles(Subtile_data),
            
            case {file:read_file_info(Filename), cowboy_req:binding(exists, Req0)} of 
                {{ok, File_info}, undefined} -> 
                    Size = File_info#file_info.size,
                    {
                        ok, 
                        cowboy_req:reply(200,
                            #{<<"content-type">> => <<"image/png">>},
                            {sendfile, 0, Size, Filename},
                            Req0),
                        State
                    }
                    ;
                {{error, enoent}, _} -> 
                    timer:sleep(1000),
                    Req = cowboy_req:reply(202,
                        #{<<"content-type">> => <<"text/plain">>}, 
                        %~ <<"tile not rendered, will be created, try again">>,
                        Req0
                    ),
                    {ok, Req, State};
                {{ok, _File_info}, <<"exists">>} ->
                    {
                        ok, 
                        cowboy_req:reply(201,
                            #{<<"content-type">> => <<"text/plain">>},
                            %~ <<"tile exists">>,
                            Req0),
                        State
                    }
                    
            end
        end.



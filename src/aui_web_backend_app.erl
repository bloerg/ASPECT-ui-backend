-module(aui_web_backend_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

parse_configuration(Configuration) when is_list(Configuration) ->
    
    Allowed_config_items = #{
        aui_db_host => 1,
        aui_db_name => 1,
        aui_db_user => 1,
        aui_db_password => 1
    },
    
    case Configuration of
        [{Key, Value}] -> 
            try maps:get(Key, Allowed_config_items) of
                1 -> ets:insert(app_configuration, {Key, Value})
            catch
                error:_ -> error_app_configuration
            end;
        [{Key, Value}| Rest] ->
            try maps:get(Key, Allowed_config_items) of
                1 -> 
                ets:insert(app_configuration, {Key, Value}),
                parse_configuration(Rest)
            catch
                error:_ -> error_app_configuration
            end;
        _Else -> error_app_configuration
    end.

start(_Type, _Args) ->

    try ets:new(app_configuration, [set, protected, named_table, {keypos, 1}]) of
        app_configuration ->
            ok
    catch
        error:badarg ->
            application:stop(aui_web_backend_app)
    end,

    try file:consult('/etc/aspectui/aui.config') of
        {ok, App_configuration} ->
            parse_configuration(App_configuration)
    catch
        {error, _} ->
            application:stop(aui_web_backend_app)
    end,
    
    try ets:new(limits, [set, public, named_table, {keypos, 1}]) of
        limits ->
            ets:insert(limits, {running_plotters, 0})
    catch
        error:badarg ->
            ok
    end,


    %~ SOM_id_constraints = [
        %~ % generate utf8 string from input
        %~ fun(Value) -> {true, unicode:characters_to_list(Value, utf8)} end,
        %~ % 256 characters should be enough for everyone
        %~ fun(Value) when length(Value) < 257 -> true end,
        %~ % check whether the Value cleaned by unallowed symbols is the same as the original Value
        %~ fun(Value) ->
            %~ case re:replace(Value, "[^[:alnum:]_-]", "", [global, {return, list}]) of
                %~ Value -> true;
                %~ _Other -> false
            %~ end
        %~ end
    %~ ],
%    Positive_integer_constraints = [int, fun(Value) when Value >= 0 -> true; (_) -> false end],
    Positive_integer_constraints = int,
    SOM_id_constraints = Positive_integer_constraints,
    SOM_zoom_constraints = Positive_integer_constraints,
    SOM_x_constraints = Positive_integer_constraints,
    SOM_y_constraints = Positive_integer_constraints,
    MJD_constraints = Positive_integer_constraints,
    Plate_constraints = Positive_integer_constraints,
    Fiberid_constraints = Positive_integer_constraints,

    Dispatch = cowboy_router:compile([
        {'_', 
            [   
                %~ {"/hello", hello_handler, []},
                
                
                %~ {
                    %~ "/som_metadata/[:mjd/:plate/:fiberid]", 
                    %~ [
                        %~ % constraints
                        %~ {mjd, MJD_constraints}, 
                        %~ {plate, Plate_constraints},
                        %~ {fiberid, Fiberid_constraints}
                        
                    %~ ],
                    %~ som_metadata_handler, 
                    %~ []
                
                %~ },
                
                % handler for retrieving som_x, som_y for mjd, plate, fiberid
                {
                    "/:som_id/sdss_to_som/:mjd/:plate/:fiberid", 
                    [
                        % constraints
                        {som_id, SOM_id_constraints},
                        {mjd, MJD_constraints}, 
                        {plate, Plate_constraints},
                        {fiberid, Fiberid_constraints}
                        
                    ],
                    sdss_to_som_handler, 
                    []
                },
                
                
                % handler for retrieving metadata for spectrum
                {
                    "/:som_id/spec_meta/:som_x/:som_y", 
                    [
                        % constraints
                        {som_id, SOM_id_constraints},
                        {som_x, SOM_x_constraints}, 
                        {som_y, SOM_y_constraints}
                        
                    ],
                    metadata_handler, 
                    []
                },
                
                % handler for the map_configuration retrieval
                {
                    "/:som_id/config.json", 
                    [
                        % constraints for the fields in the redshift request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        }
                    ],                
                    config_handler, 
                    []
                },
                
                % handler for the redshift background retrieval
                {
                    "/:som_id/redshift/:som_zoom/:som_x/:som_y", 
                    [
                        % constraints for the fields in the redshift request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        },
                        % som_zoom must be a an integer larger or equal to 0
                        {som_zoom, SOM_zoom_constraints},
                        % som_zoom must be a an integer larger or equal to 0
                        {som_x, SOM_x_constraints}, 
                        % som_zoom must be a an integer larger or equal to 0
                        {som_y, SOM_y_constraints}
                    ], 
                    redshift_handler, 
                    []
                },
                
                % handler for the sn_median background retrieval
                {
                    "/:som_id/sn_median/:som_zoom/:som_x/:som_y", 
                    [
                        % constraints for the fields in the redshift request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        },
                        % som_zoom must be a an integer larger or equal to 0
                        {som_zoom, SOM_zoom_constraints},
                        % som_zoom must be a an integer larger or equal to 0
                        {som_x, SOM_x_constraints}, 
                        % som_zoom must be a an integer larger or equal to 0
                        {som_y, SOM_y_constraints}
                    ], 
                    sn_median_handler, 
                    []
                },
                
                % handler for the ra, dec retrieval
                {
                    "/:som_id/cel_coords/:som_x/:som_y", 
                    [
                        % constraints for the fields in the redshift request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        },
                        % som_zoom must be a an integer larger or equal to 0
                        {som_x, SOM_x_constraints}, 
                        % som_zoom must be a an integer larger or equal to 0
                        {som_y, SOM_y_constraints}
                    ], 
                    celestial_coordinates_handler, 
                    []
                },
                
                % handler for the spectral classification background retrieval
                {
                    "/:som_id/spec_class/:som_zoom/:som_x/:som_y", 
                    [
                        % constraints for the fields in the redshift request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        },
                        % som_zoom must be a an integer larger or equal to 0
                        {som_zoom, SOM_zoom_constraints},
                        % som_zoom must be a an integer larger or equal to 0
                        {som_x, SOM_x_constraints}, 
                        % som_zoom must be a an integer larger or equal to 0
                        {som_y, SOM_y_constraints}
                    ], 
                    spec_class_handler, 
                    []
                },
                
                
                % handler which returns Array of zeros and ones if a tile is empty/or not
                {
                    "/:som_id/specs_exist/:som_zoom/:som_x/:som_y", 
                    [
                        % constraints for the fields in the redshift request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        },
                        % som_zoom must be a an integer larger or equal to 0
                        {som_zoom, SOM_zoom_constraints},
                        % som_zoom must be a an integer larger or equal to 0
                        {som_x, SOM_x_constraints}, 
                        % som_zoom must be a an integer larger or equal to 0
                        {som_y, SOM_y_constraints}
                    ], 
                    specs_exist_handler, 
                    []
                },

                % handler for the spec icon retrieval
                {
                    "/:som_id/spec_icons/:som_zoom/:som_x/:som_y/[:exists]", 
                    [
                        % constraints for the fields in the spec_icon request
                        {
                            % som_id must be an alphanumeric string that can contain the minus and the underscore symbol
                            som_id, 
                            SOM_id_constraints
                        },
                        % som_zoom must be a an integer larger or equal to 0
                        {som_zoom, SOM_zoom_constraints},
                        % som_zoom must be a an integer larger or equal to 0
                        {som_x, SOM_x_constraints}, 
                        % som_zoom must be a an integer larger or equal to 0
                        {som_y, SOM_y_constraints},
                        {exists, [fun(Value) -> case Value of <<>> -> true; <<"exists">> -> true; _ -> false end end]}
                    ], 
                    spec_icon_handler, 
                    []
                },
                {"/:som_id/[...]", cowboy_static, {dir, "/home/aspectui/sdssdr7"} }
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{ip, {127,0,0,1}}, {port, 8000}],
        #{env => #{dispatch => Dispatch}}
    ),
    aui_web_backend_sup:start_link().

stop(_State) ->
	ok.

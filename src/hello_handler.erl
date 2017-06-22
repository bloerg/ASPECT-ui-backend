-module(hello_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        <<"
            <html>
                <head>
                </head>
                <body>
                    This page does not exist. Please go to <a href=\"https://aspect-ui.de\" target=\"_blank\">https://aspect-ui.de</a> for further information.
                </body>
            </html>">>,
        Req0),
	{ok, Req, State}.

-module(media_converter_svc).
-export([start_services/1, load_objs/0, start/0, stop/0]).
-include("media_converter.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nktranscoder/include/nktranscoder.hrl").
-include_lib("nkimage/include/nkimage.hrl").

start_services(_) ->
    Spec = make_service_spec(),
    case nkservice:start(?SRV, Spec) of
        {ok, _} ->
            ok;
        {error, already_started} ->
            ok;
        {error, Error} ->
            lager:error("Could not start service: ~p (~p)", [Error, Spec]),
            error(service_start_error)
    end.

start() ->
    Spec = make_service_spec(),
    nkservice:start(?SRV, Spec).

stop() ->
    nkservice:stop(?SRV).

load_objs() ->
    nkdomain_node:make_objs(objs()).

objs() -> [
           #{ path => nklib_util:bjoin(["/services", ?SRV], <<"/">>),
              name => "Media Converter",
              srv_id => ?SRV,
              ?DOMAIN_SERVICE => #{
                 spec => make_service_spec()
                }
            },
           
           #{ path => "/media_converter",
              name => "Media Converter",
              srv_id => ?SRV
            }
          ].

make_service_spec() ->
    Host = media_converter_app:get(listen_ip),
    Port = media_converter_app:get(listen_port),
    Path = media_converter_app:get(listen_path),
    Secure = media_converter_app:get(listen_secure),
    Priv = code:priv_dir(sipstorm_c4),
    BinPort = nklib_util:to_binary(Port),
    Http1 = case Secure of true -> <<"https">>; false -> <<"http">> end,
    Http2 = <<Http1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,
    Ws1 = case Secure of true -> <<"wss">>; false -> <<"ws">> end,
    Ws2 = <<Ws1/binary, "://", Host/binary, ":", BinPort/binary, Path/binary>>,

    #{
        callback => media_converter,
        plugins => [nkapi, nkservice_webserver, nkservice_rest],
        nkservice_webserver => [#{
            id => <<"media_converter">>,
            url => <<Http2/binary, "/c4">>,
            file_path => filename:join([Priv, "c4"])
        }],
        nkapi_server => [#{
            id => media_converter_api,
            url => <<Http2/binary, "/_api, ", Ws2/binary, "/_api/ws">>
        }],
        nkservice_rest => [#{
            id => <<"media_converter">>,
            url => Http2
        }],
        debug => [
            {nkdomain_obj_util, all}
        ]
    }.


-module(media_converter_callbacks).
-export([service_api_syntax/3, service_api_allow/2, service_api_cmd/2, service_api_event/2]).
-export([nkservice_rest_http/4, api_server_http_auth/3]).
-include("media_converter.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").


service_api_syntax(<<"media_converter_api">>, Syntax, #nkreq{cmd = <<"media_converter/", Cmd/binary>>}=Req) ->
    {media_converter_api_syntax:syntax(Cmd, Syntax), Req};

service_api_syntax(<<"media_converter_api">>, _Syntax, _Req) ->
    continue.

service_api_allow(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/login", _/binary>>, user_id = <<>>}=Req) ->
    lager:info("Allowing media_converter/login for req: ~p", [Req]),
    true;

service_api_allow(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/", _/binary>>, user_id = <<>>}=Req) ->
    lager:info("Denying media_converter/login for req: ~p", [Req]),
    false;

service_api_allow(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/", _/binary>>}) ->
    true;

service_api_allow(<<"media_converter_api">>, Req) ->
    lager:info("Delegating media_converter req to other layers: ~p", [Req]),
    continue.

api_server_http_auth(<<"media_converter_api">>, _HttpReq, _NkReq) ->
    continue.

nkservice_rest_http(_Id, get, [<<"_file">>, FileId], Req) ->
    case nkdomain_file_obj:http_get(FileId, Req) of
        {ok, CT, Bin, Req2} ->
            {http, 200, [{<<"Content-Type">>, CT}], Bin, Req2};
        {error, Error} ->
            nkservice_rest_http:reply_json({error, Error}, Req)
    end;


nkservice_rest_http(_Id, post, [<<"_file">>], Req) ->
    case nkdomain_file_obj:http_post(Req) of
        {ok, ObjId, Path, _Obj, Req2} ->
            Reply = #{obj_id=>ObjId, path=>Path},
            {http, 200, cors(), nklib_json:encode(Reply), Req2};
        {error, Error} ->
            {http, 500, cors(), nklib_json:encode(#{reason => Error}), Req}
    end;

nkservice_rest_http(_Id, options, [<<"_file">>], Req) ->
    {http, 200, cors(), [], Req};

nkservice_rest_http(_Id, _Method, _Path, _Req) ->
    continue.

cors() ->
    Cors = [{"Content-Type", "application/json"},
            {"Access-Control-Allow-Origin", "*"},
            {"Access-Control-Allow-Headers", "X-NetComposer-Auth,Content-Type,Content-Disposition"}],
    Cors.


service_api_cmd(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/", Cmd/binary>>}=Req) ->
    Pid = spawn_link(
        fun() ->
            Reply1 = media_converter_api_cmd:cmd(Cmd, Req#nkreq{timeout_pending=false}),
            Reply2 = case Reply1 of
                ok ->
                    {ok, #{}, Req};
                {error, Error} ->
                    {error, Error, Req};
                Other ->
                    Other
            end,
            nkservice_api:reply(Reply2)
        end),
    {ack, Pid, Req};

service_api_cmd(<<"media_converter_api">>, _Req) ->
    continue.

service_api_event(_Id, #nkreq{srv_id=?SRV, data=Data}=Req) ->
    case Data of
        #{
            class := ?DOMAIN_EVENT_CLASS,
            subclass := ObjType,
            type := Type,
            obj_id := ObjId
        } ->
            Body = maps:get(body, Data, #{}),
            media_converter_api_event:event(ObjType, Type, ObjId, Body, Req);
        _ ->
            lager:warning("NKLOG MEDIA CONVERTER EV SKIP ~p", [Data]),
            ok
    end;

service_api_event(_Id, #nkreq{data=Data}=_Req) ->
    lager:warning("NKLOG MEDIA CONVERTER EV SKIP ~p", [Data]),
    continue.

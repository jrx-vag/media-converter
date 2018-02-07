-module(media_converter_callbacks).

-export([error/1]).
-export([service_api_syntax/3, service_api_allow/2, service_api_cmd/2, service_api_event/2]).
-export([nkservice_rest_http/4, api_server_http_auth/3]).

-include("media_converter.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").


error(no_thumbnail_needed)          -> "File don't need a thumbnail";
error(bigger_thumbnail)             -> "Thumbnail is bigger than original file";
error(_)                            -> continue.



%% ===================================================================
%% API Server
%% ===================================================================


%% @doc
service_api_syntax(<<"media_converter_api">>, Syntax, #nkreq{cmd = <<"media_converter/", Cmd/binary>>}=Req) ->
    {media_converter_api_syntax:syntax(Cmd, Syntax), Req};

%% Process or own object-class requests
service_api_syntax(<<"media_converter_api">>, _Syntax, _Req) ->
    continue.



%% @doc
service_api_allow(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/login", _/binary>>, user_id = <<>>}=Req) ->
    lager:info("Allowing media_converter/login for req: ~p", [Req]),
    true;

%% @doc
service_api_allow(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/", _/binary>>, user_id = <<>>}=Req) ->
    lager:info("Denying media_converter/login for req: ~p", [Req]),
    false;

%% @doc
service_api_allow(<<"media_converter_api">>, #nkreq{cmd = <<"media_converter/", _/binary>>}) ->
    true;

%% @doc
service_api_allow(<<"media_converter_api">>, Req) ->
    lager:info("Delegating media_converter req to other layers: ~p", [Req]),
    continue.



%% @doc
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



%% @doc
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



%% @doc
api_server_http_auth(<<"media_converter_api">>, _HttpReq, _NkReq) ->
    continue.


%% @doc
nkservice_rest_http(_Id, get, [<<"_file">>, FileId], Req) ->
    case nkdomain_file_obj:http_get(FileId, Req) of
        {ok, CT, Bin} ->
            {http, 200, [{<<"Content-Type">>, CT}], Bin};
        {error, Error} ->
            nkservice_rest_http:reply_json({error, Error}, Req)
    end;

nkservice_rest_http(_Id, post, [<<"_file">>], Req) ->
    case nkdomain_file_obj:http_post(Req) of
        {ok, ObjId, Path, _Obj} ->
            Reply = #{obj_id=>ObjId, path=>Path},
            {http, 200, cors(), nklib_json:encode(Reply)};
        {error, Error} ->
            {http, 500, cors(), nklib_json:encode(#{reason => Error})}
    end;

nkservice_rest_http(_Id, options, [<<"_file">>], _Req) ->
    {http, 200, cors(), []};

nkservice_rest_http(_Id, _Method, _Path, _Req) ->
    continue.

cors() ->
    Cors = [{"Content-Type", "application/json"},
            {"Access-Control-Allow-Origin", "*"},
            {"Access-Control-Allow-Headers", "X-NetComposer-Auth,Content-Type,Content-Disposition"}],
    Cors.


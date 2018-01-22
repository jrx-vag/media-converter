-module(media_converter_sample).
-compile(export_all).

-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include("media_converter.hrl").

-define(HTTP, "http://127.0.0.1:13091/mc/v09").
-define(WS, "ws://127.0.0.1:13091/mc/v09/_api/ws").
%-define(HTTP, "http://127.0.0.1:13091/mc/v09").
%-define(WS, "ws://127.0.0.1:13091/mc/v09/_api/ws").


login(Password) ->
    login(admin, Password).

login(User, Pass) ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{
        user_id => nklib_util:to_binary(User),
        password=> nklib_util:to_binary(Pass)
    },
    {ok, #{<<"session_id">>:=SessId}, Pid} = nkapi_client:start(?SRV, ?WS, Login, Fun, #{}, <<"media_converter/login">>),
    {ok, SessId, Pid}.

%logout() ->
%    cmd(<<"logout">>, #{}).

%logout_full() ->
%    cmd(<<"logout">>, #{close_websocket=>true}).

get_file(FileId) ->
    cmd(<<"get_file">>, #{file_id => FileId}).

update_file(FileId, Name, Desc, Icon) ->
    Data = #{
        file_id=>FileId,
        name=>Name,
        description=>Desc,
        icon_id=>Icon},
    cmd(<<"update_file">>, Data).

latest_files() ->
    cmd(<<"latest_files">>, #{}).

transcoding_jobs() ->
    cmd(<<"transcoding_jobs">>, #{}).

image_jobs() ->
    cmd(<<"image_jobs">>, #{}).

convert_video(FileId) ->
    convert_video(FileId, <<"video/mp4">>, #{}).

convert_video(FileId, Format, Options) ->
    Data = #{
        file_id=>FileId,
        format=>Format,
        options=>Options
    },
    cmd(<<"convert_video">>, Data).

convert_audio(FileId) ->
    convert_audio(FileId, <<"audio/mp3">>, #{}).

convert_audio(FileId, Format, Options) ->
    Data = #{
        file_id=>FileId,
        format=>Format,
        options=>Options
    },
    cmd(<<"convert_audio">>, Data).

convert_image(FileId) ->
    convert_image(FileId, <<"image/jpeg">>, #{}).

convert_image(FileId, Format, Options) ->
    Data = #{
        file_id=>FileId,
        format=>Format,
        options=>Options
    },
    cmd(<<"convert_image">>, Data).

resize_image(FileId) ->
    resize_image(FileId, <<"image/jpeg">>, 600).

resize_image(FileId, Format, Width) ->
    Data = #{
        file_id=>FileId,
        format=>Format,
        width=>Width,
        height=>Width},
    cmd(<<"resize_image">>, Data).


image_info(FileId) -> 
    Data = #{ file_id => FileId },
    cmd(<<"image_info">>, Data).

%% ===================================================================
%% HTTP
%% ===================================================================


% f(T), T = nkdomain_sample:http_login().
file_post(T) ->
    Url = list_to_binary([?HTTP, "/_file"]),
    {ok, #{<<"obj_id">>:=FileId}} = nkdomain_sample:upload(T, Url, "text/plain", "1,2,3,4"),
    FileId.


file_download(T, FileId) ->
    Url = list_to_binary([?HTTP, "/_file/", FileId]),
    nkdomain_sample:download(T, Url).


http_login(Password) ->
    Data = #{
        id => <<"admin"/utf8>>,
        password=> Password,
        ttl => 60,
        meta => #{b=>2}
    },
    {ok, #{<<"token_id">>:=TokenId}} = nkdomain_sample:http_api(?HTTP, [], <<"objects/user/get_token"/utf8>>, Data),
    TokenId.


http_get_file(T, F) ->
    nkdomain_sample:http_api(?HTTP, T,  <<"media_converter/get_file">>, #{file_id=>F}).



%% ===================================================================
%% Client fun
%% ===================================================================


api_client_fun(#nkreq{cmd = <<"event">>, data=Event}, UserData) ->
    lager:warning("CLIENT event (~p) ~p", [self(), lager:pr(Event, nkevent)]),
%    Pid = self(),
    case Event of
%        #{
%            event := <<"sipstorm/c4/invited_to_conversation">>,
%            <<"token">> := Token
%        } ->
%            %% spawn(fun() -> cmd(Pid, <<"reject_invitation">>, #{token=>Token}) end);
%            spawn(fun() -> cmd(Pid, <<"accept_invitation">>, #{token=>Token}) end);
%        #{
%            event := <<"sipstorm/c4/media/invite">>,
%            <<"invite_id">> := _InviteId
%        } ->
%            %spawn(fun() -> timer:sleep(1000), cmd(Pid, <<"media/reject_invite">>, #{invite_id=>_InviteId}) end);
%            spawn(fun() -> timer:sleep(1000), cmd(Pid, <<"media/accept_invite">>, #{sdp=>answer, audio=>true, video=>true, invite_id=>_InviteId}) end);
%            %ok;
        _ ->
            ok
    end,
    {ok, UserData};

api_client_fun(_Req, UserData) ->
    % lager:error("API REQ: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.

get_client() ->
    [{_, Pid}|_] = nkapi_client:get_all(),
    Pid.


%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Cmd, Data) ->
    Pid = get_client(),
    cmd(Pid, Cmd, Data).

cmd(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, <<"media_converter/", Cmd/binary>>, Data).




%% ===================================================================
%% HTTP
%% ===================================================================



%% f(C1), f(C2), f(C3), f(U1), f(U2), f(U3), {C1, C2, C3, U1, U2, U3} = sipstorm_c4_sample:init().
%init() ->
%    nkdomain:remove_path("/sipstorm/c4"),
%    {ok, _, _Pid1} = login(),
%    C1 = <<"/sipstorm/c4/conversations/c1">>,
%    C2 = <<"/sipstorm/c4/conversations/c2">>,
%    % U1 = <<">>,
%    % U2 = <<"/sipstorm/c4/users/u2">>,
%    % U3 = <<"/sipstorm/c4/users/u3">>,
%    _ = nkdomain_sample:domain_create("/", c4, c4, "C4 Test"),
%    {ok, #{<<"user_id">>:=U1}} = create_user(u1, s1, "n1@test", "1234"),        % /sipstorm/c4/users/u1"
%    {ok, #{<<"user_id">>:=U2}} = create_user(u2, s2, "n2@test", "1234"),        % /sipstorm/c4/users/u2"
%    {ok, #{<<"user_id">>:=U3}} = create_user(u3, s3, "n3@test", "1234"),        % /sipstorm/c4/users/u3"
%    {ok, _} = create_channel(c1, "Conv1"),                                      % /sipstorm/c4/conversations/c1
%    {ok, _} = add_member(C1, U1),
%    {ok, _} = add_member(C1, "/sipstorm/c4/users/u2"),
%    {ok, _} = create_conversation_members(c2, "Conv2", [U1, "/sipstorm/c4/users/u3"]),   % /sipstorm/c4/conversations/c2
%    {ok, #{<<"conversation_id">>:=C3}} = create_conversation_members_unique(c3, "Conv3", ["/sipstorm/c4/users/u2", U3]),
%    {C1, C2, C3, U1, U2, U3}.
%
%
%
%gen_users(0) ->
%    ok;
%
%gen_users(N) when N > 0 ->
%    {Name1, Name2, Name3} = nkdomain_sample:get_name(),
%    % _SurName = <<Name2/binary, " "/utf8, Name3/binary>>,
%    Email = <<(nkdomain_util:name(<<Name1/binary, $_, Name2/binary, $_, Name3/binary>>))/binary, "@test.com">>,
%    {ok, _} = create_user(Name1, Name2, Email, <<"1234">>),
%    gen_users(N-1).


%% ===================================================================
%% OBJECTS
%% ===================================================================


to_list(R) -> nklib_util:to_list(R).

to_bin(R) -> nklib_util:to_binary(R).

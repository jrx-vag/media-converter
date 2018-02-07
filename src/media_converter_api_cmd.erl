-module(media_converter_api_cmd).
-export([cmd/2]).

-include("media_converter.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").

cmd(<<"login">>, #nkreq{srv_id=?SRV, data=#{user_id:=UserId} = Data} = Req) ->
    SessData1 = maps:with([password, domain_id, meta], Data),
    SessData2 = SessData1#{
        domain_id => <<"root">>,
        id => UserId
    },
    case nkservice_api:api(<<"objects/session/start">>, SessData2, Req) of
        {ok, Reply, Req3} ->
            lager:info("Starting session: ~p", [Reply]),
            {ok, Reply, Req3};
        {error, object_not_found, Req3} ->
            lager:info("User not found: ~p", [Req3]),
            {error, user_not_found, Req3};
        Other -> 
            lager:info("Unexpected session start result: ~p", [Other]),
            Other
    end;

cmd(<<"get_file">>, #nkreq{data=#{file_id:=FileId}} = Req) ->
    case nkservice_api:api(<<"objects/file/get">>, #{id=>FileId}, Req) of
        {ok, Reply, Req2} ->
            {File, Reply2} = maps:take(?DOMAIN_FILE, Reply),
            Reply3 = maps:merge(Reply2, File),
            {ok, Reply3, Req2};
        Other ->
            Other
    end;

cmd(<<"update_file">>, #nkreq{data=#{file_id:=FileId} = Data} = Req) ->
    Obj1 = maps:with([name, description, icon_id], Data),
    Obj2 = Obj1#{id=>FileId},
    nkservice_api:api(<<"objects/file/update">>, Obj2, Req);


cmd(<<"latest_files">>, Req) ->
    case media_converter_util:recent_items( 40, <<"file">>, <<"files">>, fun media_converter_util:file_view/1 ) of
        {ok, Items } ->
            {ok, Items, Req};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"transcoding_jobs">>, Req) ->
    case media_converter_util:recent_items( 40, <<"transcoder.job">>, <<"transcodings">>, fun media_converter_util:transcoding_view/1 ) of
        {ok, Items } ->
            {ok, Items, Req};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"image_jobs">>, Req) ->
    case media_converter_util:recent_items( 10, <<"image.job">>, <<"image_jobs">>, fun media_converter_util:image_job_view/1 ) of
        {ok, Items } ->
            {ok, Items, Req};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"convert_video">>, #nkreq{user_id=UserId, data=Data}=Req) ->
    case nkdomain_transcoder_job_obj:create(?NKROOT, ?BASE_DOMAIN, UserId, Data) of
        {ok, #{ obj_id := JobId }=Job, Transcoder, File} ->
            {ok, Req2} = nkdomain_transcoder_job_obj:subscribe(Job, Req),
            nkdomain_transcoder_job_obj:start(?NKROOT, File, Transcoder, Job),
            {ok, #{ type => <<"transcoding">>,
                    obj_id => JobId,
                    status => <<"processing">> }, Req2};
        {error, Error} ->
            {error, Error}
    end;


cmd(<<"convert_audio">>, #nkreq{user_id=UserId, data=Data}=Req) ->
    case nkdomain_transcoder_job_obj:create(?NKROOT, ?BASE_DOMAIN, UserId, Data) of
        {ok, #{ obj_id := JobId }=Job, Transcoder, File} ->
            {ok, Req2} = nkdomain_transcoder_job_obj:subscribe(Job, Req),
            nkdomain_transcoder_job_obj:start(?NKROOT, File, Transcoder, Job),
            {ok, #{ type => <<"transcoding">>,
                    obj_id => JobId,
                    status => <<"processing">> }, Req2};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"resize_image">>, #nkreq{user_id=UserId, data=Data}=Req) ->
    case nkdomain_image_job_obj:create(?NKROOT, ?BASE_DOMAIN, UserId, Data#{action => resize}) of
        {ok, #{ obj_id := JobId }=Job, Processor, File, Store} ->
            {ok, Req2} = nkdomain_image_job_obj:subscribe(Job, Req),
            nkdomain_image_job_obj:start(?NKROOT, ?BASE_DOMAIN, UserId, File, Store, Processor, Job),
            {ok, #{ type => <<"image.job">>,
                    obj_id => JobId,
                    status => <<"processing">> }, Req2};
        {error, no_thumbnail_needed} ->
            {error, no_thumbnail_needed};
        {error, thumbnail_bigger} ->
            {error, thumbnail_bigger};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"convert_image">>, #nkreq{user_id=UserId, data=Data}=Req) ->
    case nkdomain_image_job_obj:create(?NKROOT, ?BASE_DOMAIN, UserId, Data#{action => convert}) of
        {ok, #{ obj_id := JobId }=Job, Processor, File, Store} ->
            {ok, Req2} = nkdomain_image_job_obj:subscribe(Job, Req),
            nkdomain_image_job_obj:start(?NKROOT, ?BASE_DOMAIN, UserId, File, Store, Processor, Job),
            {ok, #{ type => <<"image.job">>,
                    obj_id => JobId,
                    status => <<"processing">> }, Req2};
        {error, Error} ->
            {error, Error}
    end;


cmd(<<"image_info">>, #nkreq{user_id=UserId, data=Data}=Req) ->
    case nkdomain_image_job_obj:info(?NKROOT, ?BASE_DOMAIN, UserId, Data#{action => info}) of 
        {ok, ImageInfo} ->
            {ok, ImageInfo, Req}; 
        {error, Error} -> 
            {error, Error}
    end;

cmd(_Cmd, Req) ->
    lager:error("NKLOG MEDIA CONVERTER Not Implemented ~p", [_Cmd, Req]),
    {error, not_implemented, Req}.

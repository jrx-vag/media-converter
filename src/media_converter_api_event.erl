-module(media_converter_api_event).
-export([event/5]).

-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nktranscoder/include/nktranscoder.hrl").
-include_lib("nkimage/include/nkimage.hrl").


event(?TRANSCODER_JOB, ?TRANSCODER_JOB, _, #{ updated_time := Updated,
                                              created_time := Created,
                                              obj_id := JobId,
                                              path := JobPath,
                                              <<"transcoder.job">> := #{
                                                  progress := Progress,
                                                 status := Status,
                                                 input := Input,
                                                 output := Output,
                                                 callback_url := CallbackUrl
                                                 }}, Req) ->

    EventData = #{ id => JobId,
                   path => JobPath,
                   status => Status,
                   progress => Progress,
                   updated => Updated,
                   created => Created,
                   input => Input,
                   output => Output },

    case CallbackUrl of
        <<"">> ->
            lager:info("Notifying transcoding event via websocket only", []),
            send_event(<<"transcoder.job">>, EventData, Req);
        _ ->
            lager:info("Notifying transcoding event via websocket and callback to ~p", [CallbackUrl]),
            media_converter_util:post(CallbackUrl, EventData),
            send_event(<<"transcoder.job">>, EventData, Req),
            ok
    end;

event(?IMAGE_JOB, ?IMAGE_JOB, _, #{ updated_time := Updated,
                                              created_time := Created,
                                              obj_id := JobId,
                                              path := JobPath,
                                              ?IMAGE_JOB := #{
                                                  progress := Progress,
                                                 status := Status,
                                                 input := Input,
                                                 output := Output,
                                                 callback_url := CallbackUrl
                                                 }}, Req) ->

    EventData = #{ id => JobId,
                   path => JobPath,
                   status => Status,
                   progress => Progress,
                   updated => Updated,
                   created => Created,
                   input => Input,
                   output => Output },

    case CallbackUrl of
        <<"">> ->
            ager:info("Notifying image processing event via websocket only", []),
            send_event(<<"image.job">>, EventData, Req);
        _ ->
            lager:info("Notifying image processing  event via websocket and callback to ~p", [CallbackUrl]),
            media_converter_util:post(CallbackUrl, EventData),
            send_event(<<"image.job">>, EventData, Req),
            ok
    end;


event(_ObjType, _Type, _SessId, _Body, _Req) ->
    lager:warning("NKLOG MEDIA CONVERTER EV ~s ~s ~p", [_ObjType, _Type, _Body]),
    ok.



send_event(Type, Body, Req) ->
    Event2 = #nkevent{
        class = <<"media_converter">>,
        subclass = Type,
        type = Type,
        body = Body
    },
    #nkreq{session_pid=Pid} = Req,
    nkapi_server:send_event2(Pid, Event2),
    ok.






-module(media_converter_util).
-export([recent_items/4, 
         file_view/1,
         transcoding_view/1,
         image_job_view/1,
         post/2]).

recent_items(Count, ItemType, ColType, ViewFun ) ->
    {ok, Opts} = nkdomain_store_es_util:get_opts(),
    Query = #{
        <<"size">> => Count,
        <<"sort">> => #{ <<"created_time">> => #{ order => desc } },
        <<"query">> => #{
            <<"bool">> => #{
                <<"filter">> => [#{
                    <<"term">> => #{
                        <<"type">> => ItemType
                    }
                }]
            }
        }
    },
    case nkelastic:search(Query, Opts) of
        {ok, Hits, Data, _, _} ->
            {ok, #{ size => Hits,
                ColType => lists:map( ViewFun, Data),
                type => ColType }};
        Other ->
            {error,Other }
    end.


file_view(#{ <<"_source">> := #{ <<"path">> := Path,
                                 <<"obj_id">> := Id,
                                 <<"created_time">> := Created,
                                 <<"file">> := #{
                                     <<"content_type">> := Mime,
                                     <<"size">> := Size,
                                     <<"links">> := Links
                                    }
                               }}) ->
    #{ path => Path,
       obj_id => Id,
       content_type => Mime,
       size => Size,
       created => Created,
       links => Links
     };

file_view(#{ <<"_source">> := #{ <<"path">> := Path,
                                 <<"obj_id">> := Id,
                                 <<"created_time">> := Created,
                                 <<"file">> := #{
                                     <<"content_type">> := Mime,
                                     <<"size">> := Size
                                    }
                               }}) ->
    #{ path => Path,
       obj_id => Id,
       content_type => Mime,
       size => Size,
       created => Created,
       links => []
     }.


transcoding_view(#{ <<"_source">> := #{ <<"path">> := Path,
                                 <<"obj_id">> := Id,
                                 <<"created_time">> := Created,
                                 <<"updated_time">> := Updated,
                                 <<"transcoder.job">> := #{
                                     <<"progress">> := Progress,
                                     <<"status">> := Status,
                                     <<"input">> := InputFile,
                                     <<"output">> := OutputFile
                                    }
                               }}) ->

    #{ path => Path,
       obj_id => Id,
       status => Status,
       progress => Progress,
       created => Created,
       updated => Updated,
       input => InputFile,
       output => OutputFile

     }.

image_job_view(#{ <<"_source">> := #{ <<"path">> := Path,
                                   <<"obj_id">> := Id,
                                   <<"created_time">> := Created,
                                   <<"updated_time">> := Updated,
                                   <<"image.job">> := #{
                                     <<"progress">> := Progress,
                                     <<"status">> := Status,
                                     <<"input">> := InputFile,
                                     <<"output">> := OutputFile
                                    }
                                 }}) ->

    #{ path => Path,
       obj_id => Id,
       status => Status,
       progress => Progress,
       created => Created,
       updated => Updated,
       input => InputFile,
       output => OutputFile
     }.

post(Url, Data) ->
    Mime = "application/json",
    Json = nklib_json:encode(Data),
    Headers = [{"content-type", "application/json"}],
    Result = httpc:request(post,{erlang:binary_to_list(Url), Headers, Mime, Json},[],[]),
    lager:error("~p", [Result]),
    ok.



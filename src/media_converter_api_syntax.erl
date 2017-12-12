-module(media_converter_api_syntax).
-export([syntax/2]).

syntax(<<"media_converter/login">>, Syntax) ->
    Syntax#{
        user_id => binary,
        password => binary,
        domain_id => binary,
        '__mandatory' => [user_id]
    };

syntax(<<"media_converter/get_file">>, Syntax) ->
    Syntax#{
        file_id => binary,
        '__mandatory' => [file_id]
    };

syntax(<<"media_converter/update_file">>, Syntax) ->
    Syntax#{
        file_id => binary,
        name => binary,
        description => binary,
        icon_id => binary,
        '__mandatory' => [file_id]
    };

syntax(<<"media_converter/latest_files">>, Syntax) ->
    Syntax;

syntax(<<"media_converter/transcoding_jobs">>, Syntax) ->
    Syntax;

syntax(<<"media_converter/image_jobs">>, Syntax) ->
    Syntax;


syntax(<<"media_converter/convert_video">>, Syntax) ->
    Syntax#{
        file_id => binary,
        format => binary,
        callback_url => binary,
        options => map,
        '__mandatory' => [ file_id ],
        '__defaults' => #{ format => <<"video/mp4">>,
                           callback_url => <<>>,
                           options => #{} }
    };

syntax(<<"media_converter/convert_audio">>, Syntax) ->
    Syntax#{
        file_id => binary,
        format => binary,
        callback_url => binary,
        options => map,
        '__mandatory' => [ file_id ],
        '__defaults' => #{ format => <<"audio/mp3">>,
                           callback_url => <<>>,
                           options => #{} }
    };


syntax(<<"media_converter/convert_image">>, Syntax) ->
    Syntax#{
        file_id => binary,
        format => binary,
        callback_url => binary,
        options => map,
        '__mandatory' => [file_id],
        '__defaults' => #{ format  => <<"image/jpeg">>
                           , options => #{} }

    };

syntax(<<"media_converter/resize_image">>, Syntax) ->
    Syntax#{
        file_id => binary,
        format => binary,
        width => integer,
        height => integer,
        callback_url => binary,
        '__mandatory' => [file_id],
        '__defaults' => #{ format => <<"image/jpeg">> }
    };

syntax(_Cmd, Syntax) ->
    Syntax.


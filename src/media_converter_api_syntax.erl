-module(media_converter_api_syntax).
-export([syntax/2]).

syntax(<<"login">>, Syntax) ->
    Syntax#{
        user_id => binary,
        password => binary,
        domain_id => binary,
        '__mandatory' => [user_id]
    };

syntax(<<"get_file">>, Syntax) ->
    Syntax#{
        file_id => binary,
        '__mandatory' => [file_id]
    };

syntax(<<"update_file">>, Syntax) ->
    Syntax#{
        file_id => binary,
        name => binary,
        description => binary,
        icon_id => binary,
        '__mandatory' => [file_id]
    };

syntax(<<"latest_files">>, Syntax) ->
    Syntax;

syntax(<<"transcoding_jobs">>, Syntax) ->
    Syntax;

syntax(<<"image_jobs">>, Syntax) ->
    Syntax;


syntax(<<"convert_video">>, Syntax) ->
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

syntax(<<"convert_audio">>, Syntax) ->
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


syntax(<<"convert_image">>, Syntax) ->
    Syntax#{
        file_id => binary,
        format => binary,
        callback_url => binary,
        options => map,
        '__mandatory' => [file_id],
        '__defaults' => #{ format  => <<"image/jpeg">>
                           , options => #{} }

    };

syntax(<<"resize_image">>, Syntax) ->
    Syntax#{
        file_id => binary,
        format => binary,
        width => integer,
        height => integer,
        callback_url => binary,
        options => map,
        '__mandatory' => [file_id],
        '__defaults' => #{ format => <<"image/jpeg">>,
                           options => #{} }
    };

syntax(<<"image_info">>, Syntax) ->
    Syntax#{
      file_id => binary,
      options => map,
      '__mandatory' => [file_id],
      '__defaults' => #{ options => #{} }
     };

syntax(_Cmd, Syntax) ->
    Syntax.


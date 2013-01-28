%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2010, Tristan Sloughter
%%% @doc
%%% Thanks couchbeam! http://github.com/benoitc/couchbeam
%%% From which most of this was taken :)
%%%
%%% @end
%%% Created : 14 Feb 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(erls_resource).

-export([get/5, get/6, head/5, delete/5, post/6, put/6]).

-include_lib("erlastic_search/include/erlastic_search.hrl").

-record(response, {
          method,
          status,
          reason,
          headers,
          body
         }).

get(State, Path, Headers, Params, Opts) ->
    request(State, "GET", Path, Headers, Params, [], Opts).

get(State, Path, Headers, Params, Body, Opts) ->
    request(State, "GET", Path, Headers, Params, Body, Opts).

head(State, Path, Headers, Params, Opts) ->
    request(State, "HEAD", Path, Headers, Params, [], Opts).

delete(State, Path, Headers, Params, Opts) ->
    request(State, "DELETE", Path, Headers, Params, [], Opts).

post(State, Path, Headers, Params, Body, Opts) ->
    request(State, "POST", Path, Headers, Params, Body, Opts).

put(State, Path, Headers, Params, Body, Opts) ->
    request(State, "PUT", Path, Headers, Params, Body, Opts).


request(State, Method, Path, Headers, Params, Body, Options) ->
    Path1 = lists:append([Path,
                          case Params of
                              [] -> [];
                              Props -> "?" ++ encode_query(Props)
                          end]),
    %Headers1 = make_auth(State,
    %                     default_header("Content-Type", "application/json", Headers)),
    Headers1 = Headers,
     case has_body(Method) of
         true ->
             case make_body(Body, Headers1, Options) of
                 {Headers2, Options1, InitialBody, BodyFun} ->
                     do_request(State, Method, Path1, Headers2, {BodyFun, InitialBody}, Options1);
                 Error ->
                     Error
                end;
         false ->
             do_request(State, Method, Path1, Headers1, {nil, <<>>}, Options)
     end.


do_request(#erls_params{host=Host, port=Port, ssl=Ssl, timeout=Timeout},
        Method, Path, Headers, {BodyFun, InitialBody}, Options) ->
    case lhttpc:request(Host, Port, Ssl, Path, Method, Headers, InitialBody, Timeout, Options) of
        {ok, {{StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody}} ->
            State = #response{method    = Method,
                              status    = StatusCode,
                              reason    = ReasonPhrase,
                              headers   = ResponseHeaders,
                              body      = ResponseBody},

            make_response(State);
        {ok, UploadState} -> %% we stream
            case stream_body(BodyFun, UploadState) of
                {ok, {{StatusCode, ReasonPhrase}, ResponseHeaders, ResponseBody}} ->
                    State = #response{method    = Method,
                                      status    = StatusCode,
                                      reason    = ReasonPhrase,
                                      headers   = ResponseHeaders,
                                      body      = ResponseBody},

                    make_response(State);
                Error -> Error
            end;
        Error -> Error
    end.

make_response(#response{method=Method, status=Status, reason=Reason, body=Body}) ->
    if
        Status >= 400, Status == 404 ->
            {error, not_found};
        Status >= 400, Status == 409 ->
             {error, conflict};
        Status >= 400, Status == 412 ->
             {error, precondition_failed};
        Status >= 400 ->
             {error, {unknown_error, Status}};
        true ->
            if
                Method == "HEAD" ->
                    {ok, {Status, Reason}};
                true ->
                    case is_pid(Body) of
                        true ->
                            {ok, Body};
                        false ->
                            try mochijson2:decode(binary_to_list(Body)) of
                                Resp1 ->
                                    case Resp1 of
                                        {[{<<"ok">>, true}]} -> ok;
                                        {[{<<"ok">>, true}|Res]} -> {ok, {Res}};
                                        Obj -> {ok, Obj}
                                    end
                            catch
                                _:_ -> {ok, Body}
                            end
                    end
            end
    end.

encode_query(Props) ->
    P = fun({A,B}, AccIn) -> io_lib:format("~s=~s&", [A,B]) ++ AccIn end,
    lists:flatten(lists:foldr(P, [], Props)).

default_header(K, V, H) ->
    case proplists:is_defined(K, H) of
        true -> H;
        false -> [{K, V}|H]
    end.

has_body("HEAD") ->
    false;
has_body("GET") ->
    true;
has_body("DELETE") ->
    false;
has_body(_) ->
    true.

default_content_length(B, H) ->
    default_header("Content-Length", integer_to_list(erlang:iolist_size(B)), H).

body_length(H) ->
    case proplists:get_value("Content-Length", H) of
        undefined -> false;
        _ -> true
    end.

make_body(Body, Headers, Options) when is_list(Body) ->
    {default_content_length(Body, Headers), Options, Body, nil};
make_body(Body, Headers, Options) when is_binary(Body) ->
    {default_content_length(Body, Headers), Options, Body, nil};
make_body(Fun, Headers, Options) when is_function(Fun) ->
    case body_length(Headers) of
        true ->
            {ok, InitialState} = Fun(),
            Options1 = [{partial_upload, infinity}|Options],
            {Headers, Options1, InitialState, Fun};
        false ->
            {error,  "Content-Length undefined"}
    end;
make_body({Fun, State}, Headers, Options) when is_function(Fun) ->
    case body_length(Headers) of
        true ->
            Options1 = [{partial_upload, infinity}|Options],
            {ok, InitialState, NextState} = Fun(State),

            {Headers, Options1, InitialState, {Fun, NextState}};
        false ->
            {error,  "Content-Length undefined"}
    end;
make_body(_, _, _) ->
    {error, "body invalid"}.

stream_body({Source, State}, CurrentState) ->
    do_stream_body(Source, Source(State), CurrentState);
stream_body(Source, CurrentState) ->
    do_stream_body(Source, Source(), CurrentState).

do_stream_body(Source, Resp, CurrentState) ->
    case Resp of
        {ok, Data} ->
            {ok, NextState} = lhttpc:send_body_part(CurrentState, Data),
            stream_body(Source, NextState);
        {ok, Data, NewSourceState} ->
            {ok, NextState} = lhttpc:send_body_part(CurrentState, Data),
            stream_body({Source, NewSourceState}, NextState);
        eof ->
            lhttpc:send_body_part(CurrentState, http_eob)
    end.


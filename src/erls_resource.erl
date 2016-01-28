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

-export([get/5
        ,get/6
        ,head/5
        ,delete/5
        ,delete/6
        ,post/6
        ,put/6]).

-include("erlastic_search.hrl").

get(State, Path, Headers, Params, Opts) ->
    request(State, get, Path, Headers, Params, [], Opts).

get(State, Path, Headers, Params, Body, Opts) ->
    request(State, get, Path, Headers, Params, Body, Opts).

head(State, Path, Headers, Params, Opts) ->
    request(State, head, Path, Headers, Params, [], Opts).

delete(State, Path, Headers, Params, Opts) ->
    request(State, delete, Path, Headers, Params, [], Opts).

delete(State, Path, Headers, Params, Body, Opts) ->
    request(State, delete, Path, Headers, Params, Body, Opts).

post(State, Path, Headers, Params, Body, Opts) ->
    request(State, post, Path, Headers, Params, Body, Opts).

put(State, Path, Headers, Params, Body, Opts) ->
    request(State, put, Path, Headers, Params, Body, Opts).

request(State, Method, Path, Headers, Params, Body, Options) ->
    Path1 = <<Path/binary,
              (case Params of
                  [] -> <<>>;
                  Props -> <<"?", (encode_query(Props))/binary>>
              end)/binary>>,
    {Headers2, Options1, Body} = make_body(Body, Headers, Options),
    do_request(State, Method, Path1, Headers2, Body, Options1).

do_request(#erls_params{host=Host, port=Port, timeout=Timeout, ctimeout=CTimeout},
           Method, Path, Headers, Body, Options) ->
    % Ugly, but to keep backwards compatibility: add recv_timeout and
    % connect_timeout when *not* present in Options.
    NewOptions = lists:foldl(
        fun({BCOpt, Value}, Acc) ->
            case proplists:get_value(BCOpt, Acc) of
                undefined -> [{BCOpt, Value}|Acc];
                _ -> Acc
            end
        end,
        Options,
        [{recv_timeout, Timeout}, {connect_timeout, CTimeout}]
    ),
    case hackney:request(Method, <<Host/binary, ":", (list_to_binary(integer_to_list(Port)))/binary,
                                   "/", Path/binary>>, Headers, Body,
                         NewOptions) of
        {ok, Status, _Headers, Client} when Status =:= 200
                                          ; Status =:= 201 ->
            case hackney:body(Client) of
                {ok, RespBody} ->
                    {ok, ?ERLASTIC_SEARCH_JSON_MODULE:decode(RespBody)};
                {error, _Reason} = Error ->
                    Error
            end;
        {ok, Status, _Headers, Client} ->
            case hackney:body(Client) of
                {ok, RespBody} -> {error, {Status, ?ERLASTIC_SEARCH_JSON_MODULE:decode(RespBody)}};
                {error, _Reason} -> {error, Status}
            end;
        {error, R} ->
            {error, R}
    end.

encode_query(Props) ->
    P = fun({A,B}, AccIn) -> io_lib:format("~s=~s&", [A,B]) ++ AccIn end,
    iolist_to_binary((lists:foldr(P, [], Props))).

default_header(K, V, H) ->
    case proplists:is_defined(K, H) of
        true -> H;
        false -> [{K, V}|H]
    end.

default_content_length(B, H) ->
    default_header(<<"Content-Length">>, list_to_binary(integer_to_list(erlang:iolist_size(B))), H).

make_body(Body, Headers, Options) ->
    {default_content_length(Body, Headers), Options, Body}.

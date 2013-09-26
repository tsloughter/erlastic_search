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
     case has_body(Method) of
         true ->
             case make_body(Body, Headers, Options) of
                 {Headers2, Options1, Body} ->
                     do_request(State, Method, Path1, Headers2, Body, Options1);
                 Error ->
                     Error
                end;
         false ->
             do_request(State, Method, Path1, Headers, <<>>, Options)
     end.

do_request(#erls_params{host=Host, port=Port, timeout=Timeout, ctimeout=CTimeout},
           Method, Path, Headers, Body, Options) ->
    case hackney:request(Method, <<Host/binary, ":", (list_to_binary(integer_to_list(Port)))/binary,
                                   "/", Path/binary>>, Headers, Body,
                         [{recv_timeout, Timeout}, {connect_timeout, CTimeout} | Options]) of
        {ok, Status, _Headers, Client} when Status =:= 200
                                          ; Status =:= 201 ->
            case hackney:body(Client) of
                {ok, RespBody, _Client1} ->
                    {ok, jsx:decode(RespBody)};
                {error, _Reason} = Error ->
                    Error
            end;
        {ok, Status, _Headers, _Client} ->
            {error, Status}
    end.

encode_query(Props) ->
    P = fun({A,B}, AccIn) -> io_lib:format("~s=~s&", [A,B]) ++ AccIn end,
    iolist_to_binary((lists:foldr(P, [], Props))).

default_header(K, V, H) ->
    case proplists:is_defined(K, H) of
        true -> H;
        false -> [{K, V}|H]
    end.

has_body(head) ->
    false;
has_body(delete) ->
    false;
has_body(_) ->
    true.

default_content_length(B, H) ->
    default_header(<<"Content-Length">>, list_to_binary(integer_to_list(erlang:iolist_size(B))), H).

make_body(Body, Headers, Options) when is_list(Body) ->
    {default_content_length(Body, Headers), Options, Body};
make_body(Body, Headers, Options) when is_binary(Body) ->
    {default_content_length(Body, Headers), Options, Body};
make_body(_, _, _) ->
    {error, <<"body invalid">>}.


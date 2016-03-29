%%%-------------------------------------------------------------------
%%% @author Brujo Benavides <>
%%% @copyright (C) 2010, 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2016 by Brujo Benavides <>
%%%-------------------------------------------------------------------
-module(erls_config).
-export([get_host/0, get_port/0]).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the default host.
%% If nothing is defined in the app env for the key 'host', it's
%% <<"localhost">>.
%% @end
%%--------------------------------------------------------------------
get_host() ->
    case application:get_env(erlastic_search, host) of
        undefined ->
            <<"localhost">>;
        {ok, Host}->
            Host
    end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves the default port.
%% If nothing is defined in the app env for the key 'port', it's 9200.
%% @end
%%--------------------------------------------------------------------
get_port() ->
    case application:get_env(erlastic_search, port) of
        undefined ->
            9200;
        {ok, Port}->
            Port
    end.

%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2010, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(erls_utils).
-export([
	comma_separate/1,
	json_encode/1
    ]).


comma_separate([H]) ->
    H;
comma_separate(List) ->
    lists:foldl(fun(String, Acc) ->
                        io_lib:format("~s,~s", [String, Acc])
                end, "", List).


%% To encode utf8-json WITHOUT converting multi-byte utf8-chars into ASCII '\uXXXX'.
json_encode(Data) ->
    (mochijson2:encoder([{utf8, true}]))(Data).


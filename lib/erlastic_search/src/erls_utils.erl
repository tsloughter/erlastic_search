%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2010, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 14 Feb 2010 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(erls_utils).
-compile([export_all]).

comma_separate([H | []]) ->
    H;
comma_separate(List) ->
    lists:foldl(fun(String, Acc) ->
                        io_list:format("~s,~s", String, Acc)
                end, "", List).

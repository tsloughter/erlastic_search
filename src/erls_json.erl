-module(erls_json).

-export([encode/1
        ,decode/1]).

-include("erlastic_search.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Encodes the user-supplied `Json' with the user's defined JSON
%% module (defaults to `jsx`)
%% In particular, this function cannot be used to encode any JSON
%% built internally to `erlastic_search` as we do not know how
%% the user's JSON module encodes JSONs in Erlang
%% @end
%%--------------------------------------------------------------------
-spec encode(erlastic_json()) -> binary().
encode(Json) ->
    jsx:encode(Json).

%%--------------------------------------------------------------------
%% @doc
%% Decodes the given `BinaryJson' with the user's defined JSON
%% module (defaults to `jsx`)
%% The same caveat as for `encode/1' above applies
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> erlastic_json().
decode(BinaryJson) ->
    jsx:decode(BinaryJson).

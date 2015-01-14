%%% See http://github.com/mojombo/bert.erl for documentation.
%%% MIT License - Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>

-module(bert).
-version('1.2.0').
-author("Tom Preston-Werner").
-author("Edward Tate").

-export([encode_message/1, decode_message/1]).

-ifdef(TEST).
-include("test/bert_test.erl").
-endif.

%%---------------------------------------------------------------------------
%% Public API
%%---------------------------------------------------------------------------

-spec encode_message(term()) -> binary().

encode_message(Term) ->
  Bin = term_to_binary(encode_term(Term)),
  Size = byte_size(Bin),
  list_to_binary(binary:bin_to_list(<<Size:32>>) ++ binary:bin_to_list(Bin)).

-spec decode_message(binary()) -> term().

decode_message(<<_Hdr:32,Bin/binary>>) ->
  io:format("[bert] Decoding ~p~n", [Bin]),
  decode_term(binary_to_term(Bin)).

%%---------------------------------------------------------------------------
%% Encode
%%---------------------------------------------------------------------------

-spec encode_term(term()) -> term().

encode_term(Term) ->
  case Term of
    [] -> {bert, nil};
    true -> {bert, true};
    false -> {bert, false};
    Dict when is_record(Term, dict, 8) ->
      {bert, dict, dict:to_list(Dict)};
    List when is_list(Term) ->
      lists:map((fun encode_term/1), List);
    Tuple when is_tuple(Term) ->
      TList = tuple_to_list(Tuple),
      TList2 = lists:map((fun encode_term/1), TList),
      list_to_tuple(TList2);
    _Else -> Term
  end.

%%---------------------------------------------------------------------------
%% Decode
%%---------------------------------------------------------------------------

-spec decode_term(term()) -> term().

decode_term(Term) ->
  case Term of
    {bert, nil} -> [];
    {bert, true} -> true;
    {bert, false} -> false;
    {bert, dict, Dict} ->
      dict:from_list(Dict);
    {bert, Other} ->
      {bert, Other};
    List when is_list(Term) ->
      lists:map((fun decode_term/1), List);
    Tuple when is_tuple(Term) ->
      TList = tuple_to_list(Tuple),
      TList2 = lists:map((fun decode_term/1), TList),
      list_to_tuple(TList2);
    _Else -> Term
  end.

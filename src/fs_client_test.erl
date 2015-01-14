%%-----------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%-----------------------------------------------------------------------------
-module(fs_client_test).

-export([test_call/1, test_cast/1, test_error/1]).
-export([test_stream_binary_request/1, test_stream_binary_response/0]).
-export([test_stream_binary_io/1, test_callback/1]).

-export([all/0, composite_tests/0]).

%%==============================================================================
%% External Functions
%%==============================================================================

test_call(X) ->
  S = fs_client:connect(),
  fs_client:cli_call(S, 'FsExtern', test_f, [X]).

test_cast(X) ->
  S = fs_client:connect(),
  fs_client:cli_cast(S, 'FsExtern', test_f, [X]).

test_error(Err) ->
  S = fs_client:connect(),
  fs_client:cli_error(S, Err).

test_stream_binary_request(Xs) when is_binary(Xs) ->
  S = fs_client:connect(),
  fs_client:cli_info(S, stream, []),
  fs_client:cli_call_stream(S, 'FsExtern', test_stream_i_f, [], Xs).

test_stream_binary_response() ->
  S = fs_client:connect(),
  fs_client:cli_call(S, 'FsExtern', test_stream_o_f, []).

test_stream_binary_io(Xs) when is_binary(Xs) ->
  S = fs_client:connect(),
  fs_client:cli_info(S, stream, []),
  fs_client:cli_call_stream(S, 'FsExtern', test_stream_io_f, [], Xs).

test_callback(X) ->
  S = fs_client:connect(),
  fs_client:cli_info(S, callback, [{m, 'FsExtern'}, {f, 'callback_f'}]),
  fs_client:cli_cast(S, 'FsExtern', test_f, [X]).

%%==============================================================================
%% Internal Functions
%%==============================================================================

numeric_tests() ->
  SmallInteger = 255,
  LargeInteger1 = 256,
  LargeInteger2 = 1024,
  Float1 = 1.0,
  Float2 = 1562.015,
  Float3 = 1231314.0231,
  SmallBigInteger = 256256256256,

  [ SmallInteger, 
    LargeInteger1, LargeInteger2, 
    Float1, Float2, Float3, 
    SmallBigInteger ].

composite_tests() ->
  Atom = atom,
  SmallTuple = {Atom},
  LargeTuple = list_to_tuple(lists:seq(1,1024)),
  Nil = [],
  String = "a string",
  List = [SmallTuple],
  NestedList = [[1,2,3], [[1,2], [3,4]]],
  Binary = list_to_binary(lists:seq(1,255)),
  
  [ Atom,
    SmallTuple, LargeTuple,
    Nil, List, NestedList,
    String, 
    Binary ].

all() ->
  Tests = numeric_tests() ++ composite_tests(),
  lists:foreach(fun (X) -> test_call(X) end, Tests).

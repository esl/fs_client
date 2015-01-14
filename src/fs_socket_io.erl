%%-----------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%-----------------------------------------------------------------------------
-module(fs_socket_io).

-export([read_message/1, write_message/2]).
-export([read_binary_stream/1]).

%%==============================================================================
%% External Functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Read a message
%%------------------------------------------------------------------------------

read_message(Sock) ->
  {ok, Hdr} = recv(Sock, [], 4),
  <<Lim:32>> = Hdr,
  {ok, Msg} = recv(Sock, [], Lim),
  list_to_binary([Hdr,Msg]).

%%------------------------------------------------------------------------------
%% @doc Read a binary only stream terminated by a 4-byte 0 length header
%%------------------------------------------------------------------------------

read_binary_stream(Sock) ->
  read_binary_stream(Sock, []).

read_binary_stream(Sock, Acc) ->
  {ok, <<Lim:32>>} = recv(Sock, [], 4),
  case (Lim > 0) of
    true ->
      {ok, Bin} = recv(Sock, [], Lim),
      read_binary_stream(Sock, [Bin|Acc]);
    false ->
      {ok, list_to_binary(Acc)}
  end.

%%------------------------------------------------------------------------------
%% @doc Write a series of bytes to a socket
%%------------------------------------------------------------------------------

write_message(Sock, Xs) ->
  gen_tcp:send(Sock, Xs).

%%==============================================================================
%% Internal Functions
%%==============================================================================

recv(Sock, Acc, Lim) when Lim > 0 ->
  case gen_tcp:recv(Sock, Lim) of
    {ok, Bytes} ->
      recv(Sock, [Bytes|Acc], Lim - byte_size(Bytes));
    {error, closed} ->
      {ok, list_to_binary(Acc)}
  end;
recv(_Sock, Acc, 0) ->
  {ok, list_to_binary(Acc)}.



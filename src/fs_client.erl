%%-----------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%-----------------------------------------------------------------------------
-module(fs_client).

-export([connect/0]).
-export([cli_call/4, cli_cast/4, cli_info/3, cli_error/2]).
-export([cli_call_stream/5]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 2004).
-define(DEFAULT_OPTS, [binary, {packet, raw}, {active, false}, {nodelay, true}]).

%%==============================================================================
%% External Functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Connect to the default backend service
%%------------------------------------------------------------------------------

connect() ->
  {ok, Sock} = gen_tcp:connect(?DEFAULT_HOST, ?DEFAULT_PORT, ?DEFAULT_OPTS),
  Sock.

%%------------------------------------------------------------------------------
%% @doc Make a call request to a non Erlang gen_server
%%------------------------------------------------------------------------------

cli_call(Sock, M, F, Args) ->
  send_message_oneshot(Sock, {call, M, F, Args}).

%%------------------------------------------------------------------------------
%% @doc Make a cast request to a non Erlang gen_server
%%------------------------------------------------------------------------------

cli_cast(Sock, M, F, Args) ->
  send_message_oneshot(Sock, {cast, M, F, Args}).

%%------------------------------------------------------------------------------
%% @doc Make an info request to a non Erlang gen_server
%%------------------------------------------------------------------------------

cli_info(Sock, Cmd, Opts) ->
  send_message_oneshot(Sock, {info, Cmd, Opts}).

%%------------------------------------------------------------------------------
%% @doc Signal an error to a non Erlang gen_server
%%------------------------------------------------------------------------------

cli_error(Sock, Err) ->
  send_message_oneshot(Sock, {error, Err}).

%%------------------------------------------------------------------------------
%% @doc Streaming request (sends bytes)
%%------------------------------------------------------------------------------

cli_call_stream(Sock, M, F, Args, Xs) when is_binary(Xs) ->
  send_message_stream(Sock, {call, M, F, Args}, Xs).

%%==============================================================================
%% Internal Functions
%%==============================================================================

send_message(Sock, Msg) ->
  Bin = bert:encode_message(Msg),
  ok = fs_socket_io:write_message(Sock, Bin),
  Rsp = fs_socket_io:read_message(Sock),
  handle_response(Sock, bert:decode_message(Rsp)).

send_message_oneshot(Sock, Msg) ->
  Val = send_message(Sock, Msg),
  gen_tcp:close(Sock),
  Val.

send_message_stream(Sock, Msg, Xs) when is_binary(Xs) ->
  Bin = bert:encode_message(Msg),
  ok = fs_socket_io:write_message(Sock, Bin),
  ok = fs_socket_io:write_binary_stream(Sock, Xs),
  Rsp = fs_socket_io:read_message(Sock),
  Res = handle_response(Sock, bert:decode_message(Rsp)),
  gen_tcp:close(Sock),
  Res;
send_message_stream(_Sock, _Msg, _Data) ->
  {error, binary_stream_required}.

handle_response(_Sock, {reply, _} = Reply) ->
  Reply;
handle_response(Sock, {info, stream, _}) ->
  io:format("[fs_client] Received {info, stream, ...}, handling streaming response~n"),
  Xs = fs_socket_io:read_binary_stream(Sock),
  io:format("[fs_client] Received binary data ~p~n", [Xs]),
  {reply, Xs}.

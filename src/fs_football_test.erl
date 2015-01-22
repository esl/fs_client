%%-----------------------------------------------------------------------------
%% Author: Edward Tate <edward.tate@erlang-solutions.com>
%%-----------------------------------------------------------------------------
-module(fs_football_test).

-export([simulate_seq/1]).

%%==============================================================================
%% External Functions
%%==============================================================================

simulate_seq(NIter) ->
  S = fs_client:connect(),
  fs_client:cli_call(S, 'FsFootball', simulate_seq, [NIter]).

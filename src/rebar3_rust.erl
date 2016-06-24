-module(rebar3_rust).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Providers = [
    rebar3_prv_rust_compile
  ],

  lists:foldl(
    fun(Provider, {ok, NewState}) -> Provider:init(NewState) end,
    {ok, State},
    Providers
  ).

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

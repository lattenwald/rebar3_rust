-module(rebar3_prv_rust_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER,  compile).
-define(NAMESPACE, rust).
-define(DEPS,      [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name,       ?PROVIDER},
    {namespace,  ?NAMESPACE},
    {module,     ?MODULE},
    {bare,       true},
    {deps,       ?DEPS},
    {example,    "rebar3 rust compile"},
    {opts,       []},
    {short_desc, "A rebar plugin"},
    {desc,       "A rebar plugin"}
  ]),

  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  NewState = rebar3_rust_utils:compile_nifs(State),

  {ok, NewState}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

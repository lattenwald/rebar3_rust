-module(rebar3_rust_utils).

-export([
  compile_crates/1
]).

%% ===================================================================
%% Public API
%% ===================================================================

compile_crates(State) ->
  App     = rebar_state:current_app(State),
  AppName = binary_to_list(rebar_app_info:name(App)),
  AppDir  = rebar_app_info:dir(App),
  PrivDir = code:priv_dir(AppName),

  CratesDir  = filename:join(AppDir, "crates"),
  CratesGlob = filename:join(CratesDir, "*"),
  Crates     = filelib:wildcard(CratesGlob),

  lists:foreach(
    fun(CrateDir) -> compile_crate(CrateDir, PrivDir) end,
    Crates
  ),

  State.

%% ===================================================================
%% Internal functions
%% ===================================================================

compile_crate(CrateDir, PrivDir) ->
  Command = "cargo build --release -j$(nproc)",

  {ok, _} = rebar_utils:sh(Command, [{cd, CrateDir}, {use_stdout, true}]),

  SOGlob  = filename:join([CrateDir, "target", "release", "*.so"]),
  SOFiles = filelib:wildcard(SOGlob),

  CratesPrivDir = filename:join(PrivDir, "crates"),

  file:make_dir(CratesPrivDir),

  CrateName     = filename:basename(CrateDir),
  Destination   = filename:join(CratesPrivDir, CrateName),
  OldFilesGlob  = filename:join(Destination, "*.so"),
  OldFiles      = filelib:wildcard(OldFilesGlob),

  file:make_dir(Destination),

  lists:foreach(
    fun(File) -> remove_file(File) end,
    OldFiles
  ),

  lists:foreach(
    fun(File) -> copy_file(File, Destination) end,
    SOFiles
  ).

copy_file(SourceFile, DestinationDir) ->
  Extension = filename:extension(SourceFile),

  case Extension of
    ".so" ->
      DestinationFile = filename:join(
        DestinationDir,
        filename:basename(SourceFile)
      ),

      file:copy(SourceFile, DestinationFile);
    _ -> ok
  end.

remove_file(File) ->
  case filename:extension(File) of
    ".so" -> file:delete(File);
    _     -> ok
  end.

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
  Command = case os:type() of
    {unix,darwin} ->  "cargo rustc --release -j$(sysctl -n hw.ncpu) -- -C link-args='-flat_namespace -undefined suppress'";
    {unix,linux}  ->  "cargo build --release -j$(nproc)"
  end,

  {ok, _} = rebar_utils:sh(Command, [{cd, CrateDir}, {use_stdout, true}]),

  NewFilesGlob = filename:join([CrateDir, "target", "release", "*"]),
  NewFiles     = lists:filter(
    fun(Path) ->
        filelib:is_regular(Path) andalso filename:basename(Path) /= ".cargo-lock"
    end,
    filelib:wildcard(NewFilesGlob)
  ),

  CratesPrivDir = filename:join(PrivDir, "crates"),

  file:make_dir(CratesPrivDir),

  CrateName    = filename:basename(CrateDir),
  Destination  = filename:join(CratesPrivDir, CrateName),
  OldFilesGlob = filename:join(Destination, "*"),
  OldFiles     = filelib:wildcard(OldFilesGlob),

  file:make_dir(Destination),

  lists:foreach(fun file:delete/1, OldFiles),

  lists:foreach(
    fun(File) -> copy_file(File, Destination) end,
    NewFiles
  ).

copy_file(SourceFile, DestinationDir) ->
  FileName = case filename:extension(SourceFile) of
    ".dylib" -> filename:rootname(filename:basename(SourceFile)) ++ ".so";
    _        -> filename:basename(SourceFile)
  end,

  DestinationFile = filename:join(DestinationDir, FileName),

  file:copy(SourceFile, DestinationFile).

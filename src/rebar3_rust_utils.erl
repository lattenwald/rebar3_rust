-module(rebar3_rust_utils).

-export([
  compile_nifs/1
]).

%% ===================================================================
%% Public API
%% ===================================================================

compile_nifs(State) ->
  App        = rebar_state:current_app(State),
  AppDir     = rebar_app_info:dir(App),
  RustNifDir = filename:join(AppDir, "rust_nif"),

  Command = "cargo build --release -j$(nproc)",
  {ok, _} = rebar_utils:sh(Command, [{cd, rust_nif}, {use_stdout, true}]),

  SOGlob  = filename:join([RustNifDir, "target", "release", "*.so"]),
  SOFiles = filelib:wildcard(SOGlob),

  AppName      = binary_to_list(rebar_app_info:name(App)),
  PrivDir      = code:priv_dir(AppName),
  Destination  = filename:join(PrivDir, "rust_nif"),
  OldFilesGlob = filename:join(Destination, "*.so"),
  OldFiles     = filelib:wildcard(OldFilesGlob),

  file:make_dir(Destination),

  lists:foreach(
    fun(File) -> remove_file(File) end,
    OldFiles
  ),
  lists:foreach(
    fun(File) -> copy_file(File, Destination) end,
    SOFiles
  ),

  State.

%% ===================================================================
%% Internal functions
%% ===================================================================

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

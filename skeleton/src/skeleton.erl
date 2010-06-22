-module(skeleton).
-export([skeleton/1]).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(skeleton) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, skeleton]);
                _ ->
                    filename:join([priv, skeleton])
            end;
        Dir ->
            filename:join(Dir, skeleton)
    end,
    erlang:load_nif(SoName, 0).

skeleton(_) ->
    not_loaded(?LINE).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

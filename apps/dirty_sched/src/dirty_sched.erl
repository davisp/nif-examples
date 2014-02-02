-module(dirty_sched).
-on_load(init/0).

-export([
    double/1,
    triple_defer/1,
    triple_direct/1
]).


double(_N) ->
    not_loaded(?LINE).


triple_defer(_N) ->
    not_loaded(?LINE).


triple_direct(_N) ->
    not_loaded(?LINE).


init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).


not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

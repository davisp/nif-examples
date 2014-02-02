-module(resources).
-export([count/0, create/1, read/1]).
-on_load(init/0).

-define(APPNAME, resources).
-define(LIBNAME, resources).

count() ->
    not_loaded(?LINE).

create(_) ->
    not_loaded(?LINE).

read(_) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

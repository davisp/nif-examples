#include "erl_nif.h"

int
on_load(ErlNifenv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_TERM
skeleton(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = {
    {"skeleton", 1, skeleton}
};

ERL_NIF_INIT(skeleton, nif_funcs, &on_load, NULL, NULL, NULL);


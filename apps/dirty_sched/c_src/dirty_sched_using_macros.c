#include "erl_nif.h"

#include "nif_util.h"


static ERL_NIF_TERM
double_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM r;
    int v;

    enif_get_int(env, argv[0], &v);
    r = enif_make_int(env, v * 2);

    ERL_NIF_RETURN(r);
}


static ERL_NIF_TERM
triple_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    ERL_NIF_TERM r;
    int v;

    enif_get_int(env, argv[0], &v);
    r = enif_make_int(env, v * 3);

    ERL_NIF_RETURN(r);
}


ERL_NIF_WRAP_DIRTY_CPU(double_run)
ERL_NIF_WRAP_DIRTY_CPU(triple_run)


static ErlNifFunc nif_funcs[] = {
    {"double", 1, ERL_NIF_WRAPPED(double_run)},
    {"triple_defer", 1, ERL_NIF_WRAPPED(triple_run)},
    {"triple_direct", 1, triple_run}
};


ERL_NIF_INIT(dirty_sched, nif_funcs, NULL, NULL, NULL, NULL);

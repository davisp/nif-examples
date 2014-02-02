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
double_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_SCHEDULE_CPU(double_run, argc, argv);
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


static ERL_NIF_TERM
triple_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_SCHEDULE_CPU(triple_run, argc, argv);
}


static ErlNifFunc nif_funcs[] = {
    {"double", 1, double_nif},
    {"triple_defer", 1, triple_nif},
    {"triple_direct", 1, triple_run}
};


ERL_NIF_INIT(dirty_sched, nif_funcs, NULL, NULL, NULL, NULL);

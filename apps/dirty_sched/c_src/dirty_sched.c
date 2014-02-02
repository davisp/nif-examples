#include "erl_nif.h"

#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT

static ERL_NIF_TERM
double_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM r;
    int v;
    enif_get_int(env, argv[0], &v);
    r = enif_make_int(env, v * 2);
    return enif_schedule_dirty_nif_finalizer(env, r, enif_dirty_nif_finalizer);
}


static ERL_NIF_TERM
double_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int flags = ERL_NIF_DIRTY_JOB_CPU_BOUND;
    return enif_schedule_dirty_nif(env, flags, double_run, argc, argv);
}


static ERL_NIF_TERM
triple_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM r;
    int v;
    
    enif_get_int(env, argv[0], &v);
    r = enif_make_int(env, v * 3);
    
    if(enif_is_on_dirty_scheduler(env)) {
        return enif_schedule_dirty_nif_finalizer(
                env,
                r,
                enif_dirty_nif_finalizer
            );
    } else {
        return r;
    }
}


static ERL_NIF_TERM
triple_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int flags = ERL_NIF_DIRTY_JOB_CPU_BOUND;
    return enif_schedule_dirty_nif(env, flags, triple_run, argc, argv);
}


static ErlNifFunc nif_funcs[] = {
    {"double", 1, double_nif},
    {"triple_defer", 1, triple_nif},
    {"triple_direct", 1, triple_run}
};


#else // No dirty schedulers


static ERL_NIF_TERM
no_dirty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_tuple2(
            enif_make_atom(env, "error"),
            enif_make_atom(env, "no_dirty_schedulers")
        );
}


static ErlNifFunc nif_funcs[] = {
    {"double", 1, no_dirty},
    {"triple_defer", 1, no_dirty},
    {"triple_direct", 1, no_dirty}
};


#endif


ERL_NIF_INIT(dirty_sched, nif_funcs, NULL, NULL, NULL, NULL);

#ifndef NIF_UTIL_H
#define NIF_UTIL_H

#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT

#define ERL_NIF_WRAP_DIRTY(name, flags)                             \
static ERL_NIF_TERM                                                 \
nif_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])     \
{                                                                   \
    return enif_schedule_dirty_nif(env, (flags), name, argc, argv); \
}


#define ERL_NIF_WRAPPED(name) nif_##name


#define ERL_NIF_RETURN(value)                                       \
do {                                                                \
    if(enif_is_on_dirty_scheduler(env)) {                           \
        return enif_schedule_dirty_nif_finalizer(                   \
                env,                                                \
                value,                                              \
                enif_dirty_nif_finalizer                            \
            );                                                      \
    } else {                                                        \
        return value;                                               \
    }                                                               \
} while(0)


#else


#define ERL_NIF_WRAP_DIRTY(name, flags)
#define ERL_NIF_WRAPPED(name) name
#define ERL_NIF_RETURN(value) return (value)


#endif // ERL_NIF_DIRTY_SCHEDULER_SUPPORT


#define ERL_NIF_WRAP_DIRTY_CPU(name)                                \
    ERL_NIF_WRAP_DIRTY(name, ERL_NIF_DIRTY_JOB_CPU_BOUND)


#define ERL_NIF_WRAP_DIRTY_IO(name)                                 \
    ERL_NIF_WRAP_DIRTY(name, ERL_NIF_DIRTY_JOB_IO_BOUND)


#endif // Included nif_util.h

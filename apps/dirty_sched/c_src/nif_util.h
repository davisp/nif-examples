#ifndef NIF_UTIL_H
#define NIF_UTIL_H

#ifdef ERL_NIF_DIRTY_SCHEDULER_SUPPORT

#define ERL_NIF_DIRTY_SCHEDULE(flags, func, argc, argv) \
do {                                                    \
    return enif_schedule_dirty_nif(                     \
            env,                                        \
            (flags),                                    \
            (func),                                     \
            (argc),                                     \
            (argv)                                      \
        );                                              \
} while(0)


#define ERL_NIF_SCHEDULE_CPU(func, argc, argv)          \
    ERL_NIF_DIRTY_SCHEDULE(ERL_NIF_DIRTY_JOB_CPU_BOUND, (func), (argc), (argv))


#define ERL_NIF_SCHEDULE_IO(func, argc, argv)           \
    ERL_NIF_DIRTY_SCHEDULE(ERL_NIF_DIRTY_JOB_IO_BOUND, (func), (argc), (argv))


#define ERL_NIF_RETURN(value)                           \
do {                                                    \
    if(enif_is_on_dirty_scheduler(env)) {               \
        return enif_schedule_dirty_nif_finalizer(       \
                env,                                    \
                value,                                  \
                enif_dirty_nif_finalizer                \
            );                                          \
    } else {                                            \
        return value;                                   \
    }                                                   \
} while(0)


#else


#define ERL_NIF_SCHEDULE_DIRTY(flags, func, argc, argv) \
do {                                                    \
    return func(env, (argc), (argv))                    \
} while(0)


#define ERL_NIF_DIRTY_SCHEDULE_CPU(func, argc, argv)    \
    ERL_NIF_DIRTY_SCHEDULE(ERL_NIF_DIRTY_JOB_CPU_BOUND, (func), (argc), (argv))


#define ERL_NIF_SCHEDULE_IO(func, argc, argv)           \
    ERL_NIF_DIRTY_SCHEDULE(ERL_NIF_DIRTY_JOB_IO_BOUND, (func), (argc), (argv))


#define ERL_NIF_RETURN(value)                           \
do {                                                    \
    return value;                                       \
} while(0)


#endif // ERL_NIF_DIRTY_SCHEDULER_SUPPORT

#endif // Included nif_util.h

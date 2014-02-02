#include "erl_nif.h"

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM
repeat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* msg_env;
    ErlNifPid pid;
    ERL_NIF_TERM copy;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    if(!enif_is_pid(env, argv[0]))
    {
        return mk_error(env, "not_a_pid");
    }

    if(!enif_get_local_pid(env, argv[0], &pid))
    {
        return mk_error(env, "not_a_local_pid");
    }

    msg_env = enif_alloc_env();
    if(msg_env == NULL)
    {
        return mk_error(env, "environ_alloc_error");
    }

    copy = enif_make_copy(msg_env, argv[1]);

    if(!enif_send(env, &pid, msg_env, copy))
    {
        enif_free(msg_env);
        return mk_error(env, "error_sending_term");
    }

    enif_free_env(msg_env);
    return mk_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"repeat", 2, repeat}
};

ERL_NIF_INIT(termsend, nif_funcs, NULL, NULL, NULL, NULL);


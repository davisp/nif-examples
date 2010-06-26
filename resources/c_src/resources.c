#include "erl_nif.h"

ErlNifResourceType* RES_TYPE;
ERL_NIF_TERM atom_ok;

typedef struct
{
  int count;
} Tracker;

typedef struct
{
  int id;
} Example;

void
free_res(ErlNifEnv* env, void* obj)
{
    Tracker* tracker = (Tracker*) enif_priv_data(env);
    tracker->count -= 1;
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "resources";
    const char* name = "Example";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    Tracker* tracker;

    RES_TYPE = enif_open_resource_type(env, mod, name, free_res, flags, NULL);
    if(RES_TYPE == NULL) return -1;
    
    atom_ok = enif_make_atom(env, "ok");

    tracker = (Tracker*) enif_alloc(sizeof(Tracker));
    tracker->count = 0;
    *priv = (void*) tracker;

    return 0;
}

static ERL_NIF_TERM
count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Tracker* tracker;

    if(argc != 0)
    {
        return enif_make_badarg(env);
    }

    tracker = (Tracker*) enif_priv_data(env);
    return enif_make_int(env, tracker->count);
}

static ERL_NIF_TERM
create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Example* res;
    ERL_NIF_TERM ret;
    unsigned int id;
    Tracker* tracker;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[0], &id))
    {
        return enif_make_badarg(env);
    }

    res = enif_alloc_resource(RES_TYPE, sizeof(Example));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->id = id;

    tracker = (Tracker*) enif_priv_data(env);
    tracker->count += 1;

    return enif_make_tuple2(env, atom_ok, ret);
}

static ERL_NIF_TERM
read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Example* res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void**) &res))
    {
	return enif_make_badarg(env);
    }

    return enif_make_int(env, res->id);
}

static ErlNifFunc nif_funcs[] = {
    {"count", 0, count},
    {"create", 1, create},
    {"read", 1, read}
};

ERL_NIF_INIT(resources, nif_funcs, &load, NULL, NULL, NULL);


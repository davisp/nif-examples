#include <assert.h>
#include "erl_nif.h"

typedef struct _qitem_t
{
    struct _qitem_t*    next;
    ErlNifPid           pid;
} qitem_t;

typedef struct
{
    ErlNifMutex*        lock;
    ErlNifCond*         cond;
    qitem_t*            head;
    qitem_t*            tail;
} queue_t;

typedef struct
{
    ErlNifThreadOpts*   opts;
    ErlNifTid           qthread;
    queue_t*            queue;
    ERL_NIF_TERM        atom_ok;
} state_t;

queue_t*
queue_create()
{
    queue_t* ret;

    ret = (queue_t*) enif_alloc(sizeof(queue_t));
    if(ret == NULL) return NULL;

    ret->lock = NULL;
    ret->cond = NULL;
    ret->head = NULL;
    ret->tail = NULL;

    ret->lock = enif_mutex_create("queue_lock");
    if(ret->lock == NULL) goto error;

    ret->cond = enif_cond_create("queue_cond");
    if(ret->cond == NULL) goto error;

    return ret;

error:
    if(ret->lock != NULL) enif_mutex_destroy(ret->lock);
    if(ret->cond != NULL) enif_cond_destroy(ret->cond);
    if(ret != NULL) enif_free(ret);
    return NULL;
}

void
queue_destroy(queue_t* queue)
{
    ErlNifMutex* lock;
    ErlNifCond* cond;

    enif_mutex_lock(queue->lock);
    assert(queue->head == NULL && "Destroying a non-empty queue.");
    assert(queue->tail == NULL && "Destroying queue in invalid state.");

    lock = queue->lock;
    cond = queue->cond;

    queue->lock = NULL;
    queue->cond = NULL;

    enif_mutex_unlock(lock);

    enif_cond_destroy(cond);
    enif_mutex_destroy(lock);
    enif_free(queue);
}

int
queue_push(queue_t* queue, ErlNifPid pid)
{
    qitem_t* item = (qitem_t*) enif_alloc(sizeof(qitem_t));
    if(item == NULL) return 0;

    item->pid = pid;
    item->next = NULL;

    enif_mutex_lock(queue->lock);

    if(queue->tail != NULL)
    {
        queue->tail->next = item;
    }

    queue->tail = item;

    if(queue->head == NULL)
    {
        queue->head = queue->tail;
    }

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return 1;
}

ErlNifPid
queue_pop(queue_t* queue)
{
    qitem_t* item;
    ErlNifPid ret = (ErlNifPid) -1;

    enif_mutex_lock(queue->lock);

    while(queue->head == NULL)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }

    item = queue->head;
    queue->head = item->next;
    item->next = NULL;

    if(queue->head == NULL)
    {
        queue->tail = NULL;
    }

    enif_mutex_unlock(queue->lock);

    ret = item->pid;
    enif_free(item);

    return ret;
}

static void*
thr_main(void* obj)
{
    state_t* state = (state_t*) obj;
    ErlNifEnv* env = enif_alloc_env();
    ErlNifPid pid;
    ERL_NIF_TERM msg;

    while((pid = queue_pop(state->queue)) != (ErlNifPid) -1)
    {
        msg = enif_make_int64(env, random());
        enif_send(NULL, pid, env, msg);
        enif_clear_env(env);
    }

    return NULL;
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    state_t* state = (state_t*) enif_alloc(sizeof(state_t));
    if(state == NULL) return -1;

    state->queue = queue_create();
    if(state->queue == NULL) goto error;

    state->opts = enif_thread_opts_create("thread_opts");
    if(enif_thread_create(
            "", &(state->qthread), thr_main, state, state->opts
        ) != 0)
    {
        goto error;
    }

    state->atom_ok = enif_make_atom("ok");

    *priv = (void*) state;

    return 0;

error:
    if(state->queue != NULL) queue_destroy(state->queue);
    enif_free(state->queue);
    return NULL;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    state_t* state = (state_t*) priv;
    void* resp;
    
    queue_push(state->queue, NULL);
    enif_thread_join(state->tid, &resp);
    queue_destroy(state->queue);

    enif_thread_opts_destroy(state->opts);
    enif_free(state);
}

static ERL_NIF_TERM
send_to_pid(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    ErlNifPid pid;

    if(!enif_get_local_pid(env, argv[0], &pid))
    {
        return enif_make_badarg(env);
    }

    queue_push(state->queue, pid);

    return state->atom_ok;
}

static ERL_NIF_TERM
send_to_self(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state_t* state = (state_t*) enif_priv_data(env);
    ErlNifPid pid;

    enif_self(env, &pid);

    queue_push(state->queue, pid);

    return state->atom_ok;
}

static ErlNifFunc nif_funcs[] = {
    {"send_to_pid", 1, send_to_pid},
    {"send_to_self", 0, send_to_self}
};

ERL_NIF_INIT(termsend_threaded, nif_funcs, load, unload, NULL, NULL);


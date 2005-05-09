#include <caml/mlvalues.h>

typedef struct thread_s *thread_t;

typedef value sem_t, mutex_t;

void sched_yield();
void __attribute__((__noreturn__)) thread_exit();
void thread_sleep();
void thread_wake(thread_t);
thread_t create_thread(value *fun, value *arg);
void sem_post(sem_t s);
void sem_wait(sem_t s);
int sem_trywait(sem_t s);
void mutex_lock(mutex_t m);
#define mutex_lock(m) sem_wait(m)
void mutex_unlock(mutex_t m);
#define mutex_unlock(m) sem_post(m)
void thread_init(void);

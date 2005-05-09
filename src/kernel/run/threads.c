#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/custom.h>

#include "libc-dummy.h"
#include "multiboot.h"
#include "asm-utils.h"
#include "threads.h"

extern struct custom_operations caml_nativeint_ops;

#define SEM_NUM(sem) Field(sem,0)
#define SEM_WAITING(sem) Field(sem,1)
#define LIST_HEAD(l) Field(l,0)
#define LIST_TAIL(l) Field(l,1)

#define GET_SEM_NUM(sem) Int_val(SEM_NUM(sem))
#define SET_SEM_NUM(sem,value) do { SEM_NUM(sem) = Val_int(value); } while (0)

/* The idle stack. */
extern void stack;

int interrupt_disabled;

typedef struct thread_s
{
  void *stack; /* Stack */
  void *esp; /* Saved stack pointer */
  int id;
  struct thread_s *next_thread; /* Pointer to the next thread in the queue. */
  struct thread_s *prev_thread; /* Pointer to the prev thread in the queue. */
  struct thread_s *next;	/* Pointer to the next thread. */
  struct thread_s *prev;	/* Pointer to the prev thread. */
  int launched; /* Has the thread already been lauched? */
  value *fun; /* Function to execute. */
  value *arg; /* Argument. */

  /* caml saves */
  char *bottom_of_stack;	/* caml_bottom_of_stack */
  unsigned long last_retaddr;	/* caml_last_return_address */
  value *gc_regs;		/* caml_gc_regs */
  char *exception_pointer;	/* caml_exception_pointer */
  struct caml__roots_block *local_roots; /* local_roots */
} thread;

extern char * caml_bottom_of_stack;
extern unsigned long caml_last_return_address;
extern value * caml_gc_regs;
extern char * caml_exception_pointer;

static thread idle_thread =
{
  .stack = &stack,
  .esp = NULL,
  .id = 0,
  .next_thread = &idle_thread,
  .prev_thread = &idle_thread,
  .next = &idle_thread,
  .prev = &idle_thread,
  .launched = 1,
  .fun = NULL,
  .arg = NULL
};

static thread *queue = &idle_thread;
static thread *current = &idle_thread;
static thread *todelete;

typedef void (*scanning_action) (value, value *);

static void (*prev_scan_roots_hook) (scanning_action);
extern void (*caml_scan_roots_hook) (scanning_action);
extern void caml_do_local_roots(scanning_action f, char * bottom_of_stack,
	unsigned long last_retaddr, value * gc_regs,
	struct caml__roots_block * local_roots);

static void caml_thread_scan_roots(scanning_action action)
{
  thread *th;
  th = queue;
  do {
    if (th != current) {
      if (th->bottom_of_stack != NULL)
	caml_do_local_roots(action, th->bottom_of_stack, th->last_retaddr,
	    th->gc_regs, th->local_roots);
    }
    th = th->next;
  } while (th!=queue);
  if (prev_scan_roots_hook != NULL) (*prev_scan_roots_hook)(action);
}

extern void (*caml_enter_blocking_section_hook) ();
extern void (*caml_leave_blocking_section_hook) ();
static void (*prev_enter_blocking_section_hook) ();
static void (*prev_leave_blocking_section_hook) ();

static void caml_thread_enter_blocking_section(void)
{
  if (prev_enter_blocking_section_hook != NULL)
    (*prev_enter_blocking_section_hook)();
  current->bottom_of_stack = caml_bottom_of_stack;
  current->last_retaddr = caml_last_return_address;
  current->gc_regs = caml_gc_regs;
  current->exception_pointer = caml_exception_pointer;
  current->local_roots = caml_local_roots;
}

static void caml_thread_leave_blocking_section(void)
{
  caml_bottom_of_stack = current->bottom_of_stack;
  caml_last_return_address = current->last_retaddr;
  caml_gc_regs = current->gc_regs;
  caml_exception_pointer = current->exception_pointer;
  caml_local_roots = current->local_roots;
  if (prev_leave_blocking_section_hook != NULL)
    (*prev_leave_blocking_section_hook)();
}

static __inline__ void queue_add_thread(thread *th)
{
  th->next_thread = queue;
  th->prev_thread = queue->prev_thread;
  queue->prev_thread->next_thread = th;
  queue->prev_thread = th;
}

static __inline__ void queue_remove_thread(thread *th)
{
  th->next_thread->prev_thread = th->prev_thread;
  th->prev_thread->next_thread = th->next_thread;
}

static int thread_counter = 1;

CAMLprim value caml_funk_kthread_self(value vunit)
{
  CAMLparam1(vunit);
  CAMLreturn(caml_copy_nativeint((int)current));
}

CAMLprim value caml_funk_kthread_id(value vthread)
{
  CAMLparam1(vthread);
  CAMLreturn(Val_int(((thread_t)Nativeint_val(vthread))->id));
}

void thread_exit();

static void __attribute__((__noreturn__)) thread_wrapper()
{
  caml_callback(*current->fun, *current->arg);
  thread_exit();
}

void sched_switch(thread *prev, thread *next)
{
#ifdef DEBUG
  char buf[9];
  int i;
#endif
  if (next == prev) return;

  caml_enter_blocking_section();

  current = next;
#ifdef DEBUG
  snprintf(buf,9,"%p",current);
  for (i=0;i<8;i++)
    ((unsigned char *)(0xb8000))[160+2*i]=buf[i];
  ((unsigned char *)(0xb8000))[180]++;
#endif

  /* Switch to next thread. */
    __asm__ __volatile__ (" \
        movl %%esp,%0 ; \
        movl %1,%%esp ; \
	" : : "rm" (prev->esp), "rm" (next->esp) :
	"ebx", "ecx", "edx", "esi", "edi", "ebp", "cc", "memory");

  if (todelete) {
    free(todelete->stack);
    free(todelete);
    todelete = NULL;
  }
  caml_leave_blocking_section();
  if (!current->launched) {
    current->launched = 1;
    thread_wrapper();
  }
}

void sched_yield()
{
  sched_switch(current, current->next_thread);
}

CAMLprim value caml_funk_kthread_yield(value vunit)
{
  CAMLparam1(vunit);
  sched_yield();
  CAMLreturn(Val_unit);
}

void __attribute__((__noreturn__)) thread_exit()
{
  if (!current->id) {
    c_printf("Aiee, attempted to kill idle!");
    hang();
  }
  caml_remove_global_root(current->fun);
  caml_remove_global_root(current->arg);
  free(current->fun);
  free(current->arg);
  current->fun = NULL;
  current->arg = NULL;
  queue_remove_thread(current);
  current->prev->next = current->next;
  todelete = current;
  while (1) {
    sched_yield();
    c_printf("Uh ? returning back from our grave ?!");
  }
}

CAMLprim value caml_funk_kthread_exit(value vunit)
{
  CAMLparam1(vunit);
  thread_exit();
  CAMLreturn(Val_unit);
}

void thread_sleep()
{
  thread_t next = current->next_thread;
  queue_remove_thread(current);
  sched_switch(current,next);
}

CAMLprim value caml_funk_kthread_sleep(value vunit)
{
  CAMLparam1(vunit);
  thread_sleep();
  CAMLreturn(Val_unit);
}

void thread_wake(thread_t t)
{
  queue_add_thread(t);
}

CAMLprim value caml_funk_kthread_wake(value vt)
{
  CAMLparam1(vt);
  thread_wake((thread_t)Nativeint_val(vt));
  CAMLreturn(Val_unit);
}

void sem_post(sem_t s)
{
  thread *towake;
  cli();
  SET_SEM_NUM(s,GET_SEM_NUM(s)+1);
  if (SEM_WAITING(s) != Val_emptylist) {
    towake = (thread *) Nativeint_val(LIST_HEAD(SEM_WAITING(s)));
    SEM_WAITING(s) = LIST_TAIL(SEM_WAITING(s));
    sti();
    thread_wake(towake);
    if (!interrupt_disabled)
      sched_yield();
  } else
    sti();
}

CAMLprim value caml_funk_sem_post(value vsem)
{
  CAMLparam1(vsem);
  sem_post(vsem);
  CAMLreturn(Val_unit);
}

void sem_wait(sem_t s)
{
  cli();
  while (!GET_SEM_NUM(s)) {
    value cons, entry;
    thread_t next = current->next_thread;
    queue_remove_thread(current);
    cons = caml_alloc_small(2,Tag_cons); /* Warn: this might call malloc() and be some trouble some day... It won't for now */
    LIST_TAIL(cons) = SEM_WAITING(s);
    entry = caml_copy_nativeint((long) current);
    LIST_HEAD(cons) = entry;
    SEM_WAITING(s) = cons;
    sti();
    sched_switch(current,next);
    cli();
  }
  SET_SEM_NUM(s,GET_SEM_NUM(s)-1);
  sti();
}

CAMLprim value caml_funk_sem_wait(value vsem)
{
  CAMLparam1(vsem);
  sem_wait(vsem);
  CAMLreturn(Val_unit);
}

int sem_trywait(sem_t s)
{
  int res;
  cli();
  if (!(res = GET_SEM_NUM(s))) {
    sti();
    return -EAGAIN;
  }
  SET_SEM_NUM(s,res-1);
  sti();
  return 0;
}

CAMLprim value caml_funk_sem_trywait(value vsem)
{
  CAMLparam1(vsem);
  sem_trywait(vsem);
  CAMLreturn(Val_unit);
}

thread_t create_thread(value *fun, value *arg)
{
  thread *new_thread;
  new_thread = malloc(sizeof (*new_thread));
  new_thread->stack = malloc(STACK_SIZE);
  new_thread->esp = new_thread->stack + (STACK_SIZE-64);
  new_thread->id = thread_counter++;
  new_thread->launched = 0,
  new_thread->fun = fun;
  new_thread->arg = arg;
  new_thread->bottom_of_stack = NULL;
  new_thread->exception_pointer = NULL;
  new_thread->local_roots = NULL;
  new_thread->next = idle_thread.next;
  new_thread->prev = &idle_thread;
  idle_thread.next->prev = new_thread;
  idle_thread.next = new_thread;
  queue_add_thread(new_thread);
  return(new_thread);
}

CAMLprim value caml_funk_create_kthread(value vfun, value varg)
{
  CAMLparam2(vfun,varg);
  value* f = malloc(sizeof(value));
  value* a = malloc(sizeof(value));
  *f = vfun;
  *a = varg;
  caml_register_global_root(f);
  caml_register_global_root(a);
  thread_t t = create_thread(f, a);
  CAMLreturn(caml_copy_nativeint((int)t));
}

void thread_init(void)
{
  prev_scan_roots_hook = caml_scan_roots_hook;
  caml_scan_roots_hook = caml_thread_scan_roots;
  prev_enter_blocking_section_hook = caml_enter_blocking_section_hook;
  caml_enter_blocking_section_hook = caml_thread_enter_blocking_section;
  prev_leave_blocking_section_hook = caml_leave_blocking_section_hook;
  caml_leave_blocking_section_hook = caml_thread_leave_blocking_section;
}

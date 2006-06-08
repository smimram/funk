(** The type of thread handles. *)
type t = nativeint

type kthread = t

(** Return the thread currently executing. *)
external self : unit -> t = "caml_funk_kthread_self"

external id : t -> int = "caml_funk_kthread_id"

external create : ('a -> unit) -> 'a -> nativeint = "caml_funk_create_kthread"

external _yield : unit -> unit = "caml_funk_kthread_yield"

let yield () =
  (* Funk.kprintf "Thread" "Yielding from %d.\n%!" (id (self ())); *)
  _yield ()

external exit : unit -> unit = "caml_funk_kthread_exit"

external sleep : unit -> unit = "caml_funk_kthread_sleep"

external wake : nativeint -> unit = "caml_funk_kthread_wake"

module Semaphore =
struct
  type t = { mutable num: int; mutable waiting: kthread list }

  let create num = { num = num; waiting = [] }

  external post : t -> unit = "caml_funk_sem_post"

  external wait : t -> unit = "caml_funk_sem_wait"

  external trywait : t -> unit = "caml_funk_sem_trywait"
end

module Mutex =
struct
  type t = Semaphore.t

  let create () =
    Semaphore.create 1

  (* external lock : t -> unit = "caml_funk_kthread_mutex_lock" *)
  let lock = Semaphore.wait

  (* external unlock : t -> unit = "caml_funk_kthread_mutex_unlock" *)
  let unlock = Semaphore.post
end

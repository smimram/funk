(**
  * Kernel threads.
  *
  * @author Samuel Thibault, Samuel Mimram
  *)

(** The type of thread handles. *)
type t

(** Thread creation and termination. *)

(** Return the thread currently executing. *)
val self : unit -> t

(** Return the identifier of the given thread. A thread identifier is an integer
  * that identifies uniquely the thread. It can be used to build data structures
  * indexed by threads. *)
val id : t -> int

(** [create funct arg] creates a new thread of control, in which the function
  * application [funct arg] is executed concurrently with the other threads of
  * the program. *)
val create : ('a -> unit) -> 'a -> t

val exit : unit -> unit

val sleep : unit -> unit

val wake : t -> unit
                                     
(** Re-schedule the calling thread without suspending it. This function can be
  * used to give scheduling hints, telling the scheduler that now is a good
  * time to switch to other threads.
  **)
val yield : unit -> unit

module Semaphore :
sig
  type t

  val create : int -> t

  val post : t -> unit

  val wait : t -> unit

  val trywait : t -> unit
end

module Mutex :
sig
  type t

  val create : unit -> t

  val lock : t -> unit

  val unlock : t -> unit
end

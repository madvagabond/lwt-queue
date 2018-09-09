exception Queue_Closed
  
type state = Polling | Offering | Idle | Closed

type 'a t = {
  mutable state : state;
  writers : 'a Queue.t;
  readers : 'a Lwt.u Queue.t;
  budget : int;
}


(** Popping the queue, throws Queue_Closed*)
val poll: 'a t -> 'a Lwt.t

(** Asynchronous push,  *)
val offer: 'a t -> 'a ->  bool Lwt.t

(** Retries until write is complete throws Queue_Closed *)
val put: 'a t -> 'a -> unit Lwt.t


(** Shutdowns queue *)
val close: 'a t -> unit Lwt.t


(** Creates bounded queue is Pervasives.max_int by default *)
val create: ?max:int -> unit -> 'a t

(** Empties out queue elements to list*)
val drain: 'a t -> 'a list Lwt.t

val size: 'a t -> int

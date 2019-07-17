exception Queue_Closed

type 'a t


(** Popping the queue, throws Queue_Closed*)
val poll: 'a t -> 'a Lwt.t

(** Asynchronous insertion*)
val offer: 'a t -> 'a ->  bool 

(** Blocks until offer succeeds, may change to be fully nonblocking*)
val put: 'a t -> 'a -> unit Lwt.t





(** Shutdowns queue *)
val close: 'a t -> unit 


(** Empties out queue elements to list*)
val drain: 'a t -> 'a list Lwt.t

val size: 'a t -> int


val bounded: int -> 'a t

val unbounded: unit -> 'a t

val is_closed: 'a t -> bool 

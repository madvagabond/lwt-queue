open Lwt.Infix

exception Queue_Closed
  
  
type state = Polling | Offering | Idle | Closed

type 'a t = {
  mutable state: state;
  
  writers: 'a Queue.t; 
  readers: 'a Lwt.u Queue.t;
  budget: int
}



let poll t =

  match t.state with
  | Idle ->
    let (p, r) = Lwt.wait () in
    t.state <- Polling; 
    Queue.push r t.readers;
    p

  | Polling ->
    let (p, r) = Lwt.wait () in
    Queue.push r t.readers;
    p

  | Offering ->
    let p = Queue.pop t.writers in 
    if (Queue.is_empty t.writers) then (t.state <- Idle);
    Lwt.return p

  | Closed when (Queue.is_empty t.writers)->
    Lwt.fail Queue_Closed

  | Closed ->
    Queue.pop t.writers |> Lwt.return






let offer t a =

  match t.state with

  | Idle ->
    Queue.push a t.writers;
    t.state <- Offering; 
    Lwt.return true

  | Offering when (Queue.length t.writers) >= t.budget ->
    Lwt.return false

  | Offering ->
    Queue.push a t.writers;
    Lwt.return true

  | Polling ->
    let waiter = Queue.pop t.readers in
    Lwt.wakeup waiter a;
    Lwt.return true

  | Closed ->
    Lwt.return false 






let rec put t a =

  match t.state with

  | Idle ->
    Queue.push a t.writers;
    t.state <- Offering; 
    Lwt.return_unit

  | Offering when (Queue.length t.writers) >= t.budget ->
    put t a

  | Offering ->
    Queue.push a t.writers;
    Lwt.return_unit

  | Polling ->
    let waiter = Queue.pop t.readers in
    Lwt.wakeup waiter a;
    Lwt.return_unit

  | Closed ->
    Lwt.fail Queue_Closed




let is_closed t =
  if (t.state = Closed) then true
  else false


let close t =

  let rec kill_polls () = 
    let w = Queue.pop t.readers in
    Lwt.wakeup_exn w Queue_Closed;

    if (Queue.is_empty t.readers) then
      Lwt.return_unit

    else
      kill_polls ()

  in

  match t.state with
  | Idle ->
    t.state <- Closed;
    Lwt.return_unit

  | Polling ->
    kill_polls () 

  | Offering ->
    t.state <- Closed;
    Lwt.return_unit 

  | Closed -> Lwt.return_unit




  
let create ?max () =
  
  match max with
  | Some budget ->
    let readers = Queue.create () in 
    let writers = Queue.create () in

    let state = Idle in
    {budget; writers; readers; state;}

  | None ->

    let readers = Queue.create () in 
    let writers = Queue.create () in

    let state = Idle in
    let budget = max_int in

    {budget; state; writers; readers}



        
let drain t =
  
  let rec aux l =
    let e = Queue.pop t.writers in
    let l1 = l @ [e] in 
    if (Queue.length t.writers) > 0 then
      aux l1

    else Lwt.return l1
  in
  
  aux [] 


let size t = Queue.length t.writers

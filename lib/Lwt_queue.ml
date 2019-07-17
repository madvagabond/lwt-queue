

exception Queue_Closed
  
  
type state = Polling | Offering | Idle | Closed

type 'a t = {
  mutable state: state;
  
  writers: 'a Queue.t; 
  readers: 'a Lwt.u Queue.t;
  budget: int option;

}




let is_closed t =
  match t.state with
  | Closed -> true
  | _ -> false 





let check_bounds t =

  let check i =
    (Queue.length t.writers) <= i
  in

  
  match t.budget with
  | Some b -> check b
  | _ -> true


        


let poll t =
  match t.state with
  | Idle ->
    let (p, r) = Lwt.task () in
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

  | Closed when (Queue.is_empty t.writers) ->
    
    Lwt.fail Queue_Closed

  | Closed ->
    Queue.pop t.writers |> Lwt.return








let offer t a =

  match t.state with

  | Idle ->
    Queue.push a t.writers;
    t.state <- Offering; 
    true


  | Offering when check_bounds t ->
    Queue.push a t.writers;
    true

  | Offering ->
    false


  | Polling ->
    let waiter = Queue.pop t.readers in
    Lwt.wakeup waiter a;
    true

  | Closed ->
    false 







let rec put t a =

  match t.state with

  | Idle ->
    Queue.push a t.writers;
    t.state <- Offering; 
    Lwt.return_unit


  | Offering when check_bounds t ->
    Queue.push a t.writers;
    Lwt.return_unit

  | Offering ->
    put t a


  | Polling ->
    let waiter = Queue.pop t.readers in
    Lwt.wakeup_later waiter a;
    Lwt.return_unit

  | Closed ->
    Lwt.fail Queue_Closed






let close t =

  let rec kill_polls () = 
    let w = Queue.pop t.readers in
    Lwt.wakeup_later_exn w Queue_Closed;

    if (Queue.is_empty t.readers) then
      ()

    else
      kill_polls ()
  in
  
  match t.state with
  | Idle ->
    t.state <- Closed;
    ()

  | Polling ->
    kill_polls () 

  | Offering ->
    t.state <- Closed;
    ()

  | Closed -> ()
                





let bounded size =

  let readers = Queue.create () in 
  let writers = Queue.create () in

  let state = Idle in
  let budget = Some size in 
  {budget; state; writers; readers}




let unbounded () =
  
  let readers = Queue.create () in 
  let writers = Queue.create () in
  
  let state = Idle in
  let budget = None in

  {
    budget;
    state;
    writers;
    readers
  }





  
let drain t =

  let _ = close t in
  
  let rec aux l =
    let e = Queue.pop t.writers in
    let l1 = l @ [e] in 
    if (Queue.length t.writers) > 0 then
      aux l1

    else Lwt.return l1
  in
  
  aux [] 


let size t = Queue.length t.writers

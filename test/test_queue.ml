open Lwt.Infix

open Alcotest



let poll_queue s () =
  let queue = Lwt_queue.unbounded () in

  let p1 = Lwt_queue.poll queue in
  let p2 = Lwt_queue.poll queue in


  let _ = Lwt_queue.offer queue 1 in
  p1 >>= fun i -> 
  Alcotest.(check int) "ints are the same" i 1;

  let _ = Lwt_queue.offer queue 2 in
  p2 >>= fun i ->
  Alcotest.(check int) "ints are the same" i 2 |> Lwt.return



let test_close () =
  let f () = Lwt_main.run (
    let queue = Lwt_queue.unbounded () in 
    Lwt_list.map_s (fun x -> Lwt_queue.offer queue x |> Lwt.return ) [1; 2] >>= fun _ ->
    let _ = Lwt_queue.close queue in
    Lwt_list.map_s (fun _x -> Lwt_queue.poll queue) [1; 2] >>= fun _ ->

    Lwt_queue.poll queue >>= fun _ -> Lwt.return_unit
  )

  in


  Alcotest.check_raises
      "Returns Queue Closed exception after all writes were seen"
      Lwt_queue.Queue_Closed
      f
  



let test_parallel s () =
  let queue = Lwt_queue.unbounded () in
  let items = [1; 2; 3; 4; 5;] in
  Lwt_list.map_p (fun x -> Lwt_queue.offer queue x |> Lwt.return) items >>= fun _ ->

  Lwt_queue.drain queue >|= fun list ->

  let sorted = List.sort (fun l r -> compare l r) list in 

  Alcotest.(check (list int) )
    "drain returns all the elements put in"
    items
    list
  ;

  let got = Lwt_queue.size queue in
  Alcotest.(check int) "size is zero" got 0
  
    

let tests = [
  Alcotest_lwt.test_case "Test gets and sets" `Quick poll_queue;
  "Test closed", `Quick, test_close;
  Alcotest_lwt.test_case "Test Parallel Ops and drain" `Quick test_parallel
]


let _ = Alcotest.run "testing lwt-queue"  ["Lwt_queue", tests]

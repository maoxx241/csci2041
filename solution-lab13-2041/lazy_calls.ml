open Printf;;

(* standard recursive fibonacci function *)
let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (fib (n-1)) + (fib (n-2))
;;

(* main code for program: time execution of fibonacci based on command
   line argument. Compare normal fib to memoized version. *)
let _ =
  if Array.length Sys.argv < 2 then                      (* handle command line arguments *)
    begin
      printf "usage: %s {integer}\n" Sys.argv.(0);
      Pervasives.exit 1;
    end;
  let n = int_of_string (Sys.argv.(1)) in                (* argument is int to use in fib calls *)


  let flags = [|(false,false); (true,false);             (* array of flags *)
                (false,true);  (true,true)|]
  in

  printf "=====(1) either_args=====\n";

  (* Print first/second arg based on flags *)
  let either_args (flag1,flag2) arg1 arg2 =
    if flag1 then
      printf "First:  %d\n" arg1;
    if flag2 then
      printf "Second: %d\n" arg2;
  in

  for i=0 to 3 do
    let (flag1,flag2) = flags.(i) in
    let start_time = Sys.time () in

    either_args (flag1,flag2) (fib n) (fib n);           (* compute fib in arguments *)

    let stop_time = Sys.time () in
    let tot_time = stop_time -. start_time in
    printf "(%5b,%5b) time: %.4f secs\n" flag1 flag2 tot_time;
  done;

  printf "=====(2) either_susp, new suspension each iter=====\n";

  (* Print result of calling first/second arg based on flags *)
  let either_susp (flag1,flag2) arg1 arg2 =
    if flag1 then
      printf "First:  %d\n" (arg1 ());
    if flag2 then
      printf "Second: %d\n" (arg2 ());
  in

  for i=0 to 3 do
    let (flag1,flag2) = flags.(i) in
    let start_time = Sys.time () in

    (* pass "suspension" as both arguments *)
    either_susp (flag1,flag2) (fun ()-> fib n) (fun ()->fib n); 

    let stop_time = Sys.time () in
    let tot_time = stop_time -. start_time in
    printf "(%5b,%5b) time: %.4f secs\n" flag1 flag2 tot_time;
  done;

  printf "=====(3) either_lazy, new lazy expr each iter======\n";

  (* Print result of calling first/second arg based on flags *)
  let either_lazy (flag1,flag2) arg1 arg2 =
    if flag1 then
      printf "First:  %d\n" (Lazy.force arg1);           (* force the lazy expression to evaluate *)
    if flag2 then
      printf "Second: %d\n" (Lazy.force arg2);
  in

  for i=0 to 3 do
    let (flag1,flag2) = flags.(i) in
    let start_time = Sys.time () in

    (* pass in two lazy expressions as args *)
    either_lazy (flag1,flag2) (lazy (fib n)) (lazy (fib n)); 

    let stop_time = Sys.time () in
    let tot_time = stop_time -. start_time in
    printf "(%5b,%5b) time: %.4f secs\n" flag1 flag2 tot_time;
  done;

  printf "=====(4) either_susp, single suspension before loop=====\n";

  let fib_susp = (fun ()->fib n) in                      (* suspension before loop *)
  for i=0 to 3 do
    let (flag1,flag2) = flags.(i) in
    let start_time = Sys.time () in

    (* pass "suspension" as both arguments *)
    either_susp (flag1,flag2) fib_susp fib_susp;

    let stop_time = Sys.time () in
    let tot_time = stop_time -. start_time in
    printf "(%5b,%5b) time: %.4f secs\n" flag1 flag2 tot_time;
  done;

  printf "=====(5) either_lazy, new lazy expr each iter======\n";

  let fib_lazy = lazy (fib n) in                         (* lazy expression before loop *)
  for i=0 to 3 do
    let (flag1,flag2) = flags.(i) in
    let start_time = Sys.time () in

    (* pass in two lazy expressions as args *)
    either_lazy (flag1,flag2) fib_lazy fib_lazy;

    let stop_time = Sys.time () in
    let tot_time = stop_time -. start_time in
    printf "(%5b,%5b) time: %.4f secs\n" flag1 flag2 tot_time;
  done;


;;

(* fib.ml: demo program which creates a fibonnacci function and uses
   Memfunc1.Memoize to create a memoized version of it.  The run times
   of the two versions are compared. *)

(* standard recursive fibonacci function *)
let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (fib (n-1)) + (fib (n-2))
;;

(* interface module to memoize fib function *)
module FibFunc = struct
  type arg_t = int;;
  type ret_t = int;;
  let func = fib;;
  let compare_arg = (-);;
end;;

(* module with memoized fibonacci *)
module MemoFib = Memfunc1.Memoize(FibFunc);;

open Printf;;

(* main code for program: time execution of fibonacci based on command
   line argument. Compare normal fib to memoized version. *)
let _ =
  if Array.length Sys.argv < 2 then                      (* handle command line arguments *)
    begin
      printf "usage: %s {integer}\n" Sys.argv.(0);
      Pervasives.exit 1;
    end;
  let n = int_of_string (Sys.argv.(1)) in                (* argument is int to use in fib calls *)
  printf "n = %d\n" n;                                   (* print the integer *)

  printf "---------------------------------\n";
  printf "Running loops on fib/mem functions\n";

  let farr = Array.make n (-1) in
  let start_time = Sys.time () in                        (* what time does the loop start running  *)
  for i=1 to n do
    farr.(i-1) <- fib i;                                 (* repeatedly call normal fib *)
  done;
  let stop_time = Sys.time () in                         (* what time does the loop complete *)
  let tot_time = stop_time -. start_time in              (* calculate elapsed time *)
  printf "fib time: %.4f secs\n" tot_time;               (* report time *)

  let marr = Array.make n (-1) in
  let start_time = Sys.time () in                        (* what time does the loop start running  *)
  for i=1 to n do
    marr.(i-1) <- MemoFib.call i;                        (* repeatedly call memoized fib *)
  done;
  let stop_time = Sys.time () in                         (* what time does the loop complete *)
  let tot_time = stop_time -. start_time in              (* calculate elapsed time *)
  printf "mem time: %.4f secs\n" tot_time;               (* report time *)

  for i=1 to n do                                        (* print results table *)
    printf "fib(%2d) = %8d  | MemoFib.call(%2d) = %8d\n" 
      i farr.(i-1) i marr.(i-1);
  done;

  printf "---------------------------------\n";
  printf "Re-running loops on fib/mem again\n";

  let farr = Array.make n (-1) in
  let start_time = Sys.time () in                        (* what time does the loop start running  *)
  for i=1 to n do
    farr.(i-1) <- fib i;                                 (* repeatedly call normal fib *)
  done;
  let stop_time = Sys.time () in                         (* what time does the loop complete *)
  let tot_time = stop_time -. start_time in              (* calculate elapsed time *)
  printf "fib time: %.4f secs\n" tot_time;               (* report time *)

  let start_time = Sys.time () in                        (* what time does the loop start running  *)
  let marr = Array.make n (-1) in
  for i=1 to n do
    marr.(i-1) <- MemoFib.call i;                        (* repeatedly call memoized fib *)
  done;
  let stop_time = Sys.time () in                         (* what time does the loop complete *)
  let tot_time = stop_time -. start_time in              (* calculate elapsed time *)
  printf "mem time: %.4f secs\n" tot_time;               (* report time *)

  for i=1 to n do                                        (* print results table *)
    printf "fib(%2d) = %8d  | MemoFib.call(%2d) = %8d\n" 
      i farr.(i-1) i marr.(i-1);
  done;

;;
      
               

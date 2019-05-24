(* curry_trouble.ml: Debug the following code which has a compile time
   error due to a partial application. *)

open Printf;;

(* raise base to given exp *)
let pow base exp =
  let ans = ref 1 in
  for i=1 to exp do
    ans := !ans * base;
  done;
  !ans
;;

(* print successive powers *)
let print_powers base start stop =
  for i=start to stop do
    let x = pow base in
    printf "%d^%d is %d\n" base i x;
  done;
;;
      
(* main function *)
let _ =
  if Array.length Sys.argv < 4 then
    begin
      printf "usage: %s base start stop\n" Sys.argv.(0);
      exit 1;
    end;
  let base  = int_of_string Sys.argv.(1) in
  let start = int_of_string Sys.argv.(2) in
  let stop  = int_of_string Sys.argv.(3) in
  print_powers base start stop;
;;
  

(**************************************************************)
(* open any modules with needed functions to make them direcly usable *)
open Printf;;
open Defs_only;;

(**************************************************************)
(* MAIN CODE to execute when running as a program *)
let _ =
  greet_user user;
  let a = 4.0 in
  let b = 3.0 in
  printf "A right triangle has sides length %.1f and %.1f\n" a b;
  let hyp = hypotenuse a b in
  printf "Its hypotenuse has length %.1f\n" hyp;
  printf "Au revoir!\n";
;;

(**************************************************************)
(* open any modules with needed functions to make them direcly usable *)
open Printf;;

(**************************************************************)
(* DEFINITIONS FOR VALUES AND FUNCTIONS *)

(* name of user of the program *)
let user = "Triangle Lover";;

(* function to print messages *)
let greet_user name =
  let msg = "Bonjour!" in
  print_endline msg;
  let greet = "Welcome to OCaml, "^name in
  print_endline greet;
;;


(* compute length of the hypotenuse of a right triangle *)
let hypotenuse a b =
  let c2 = a*.a +. b*.b in
  let c = sqrt c2 in
  c
;;

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

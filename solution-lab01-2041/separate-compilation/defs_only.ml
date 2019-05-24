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


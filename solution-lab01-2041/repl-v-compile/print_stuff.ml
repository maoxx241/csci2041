(* function to print messages *)
let print_hello name =
  let msg = "Hello there!" in
  print_endline msg;
  let greet = "Welcome to OCaml, "^name in
  print_endline greet;
;;


let student = "Meatbag #43281";;        (* replace this with your name *)

(* call the printing function *)
print_hello student;;

print_endline "Remember, there is no spoon.";;


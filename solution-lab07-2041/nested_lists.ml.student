(* nested_lists.ml: Define some functions on nested lists (e.g. int
   list list and string list list) using higher-order functions. *)

open Printf;;

(* sample string list list *)
let strll = [
    ["Korra";"Mako";"Bolin";"Asami";];
    ["Tenzin";"Pema"];
    ["Meelo";"Jinora";"Iki"];
    ["Amon";"Kuvira";"Zaheer"];
];;

(* sample int list list *)
let intll = [
    [1;2;3];
    [4;5;6];
    [7;8;9;10];
    [11];
];;

(* val flatten : 'a list list -> 'a list

   Converts a list of lists to a single "flat" list. Each list is
   appended onto the last.  Since this function is polymorphic, no
   special versions are needed for different types of list.  Makes use
   of List.fold_left. This function is equivalent to the standard
   List.flatten function built into ocaml. 

   # flatten intll;;
   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11]
   # flatten strll;;
   - : string list =
   ["Korra"; "Mako"; "Bolin"; "Asami"; "Tenzin"; "Pema"; "Meelo"; "Jinora";
    "Iki"; "Amon"; "Kuvira"; "Zaheer"]
*)
let flatten list_list =
  (* YOUR CODE HERE *)
  []
;;

(* val totlen : 'a list list -> int

   Calculates the total length of all combined lists using fold_left.
   Does not use flatten but does use List.length.

   # totlen intll;;
   - : int = 11
   # totlen strll;;
   - : int = 12
*)
let totlen list_list =
  (* YOUR CODE HERE *)
  -1
;;


(* val print_list_list : ('a -> unit) -> 'a list list -> unit

   Print all lists in a list. argument print_elem is a function that
   prints the a single element of any list.  Each list is printed on
   its own line starting with an open square brace [ and ending with a
   clsoe square brace ].  See specific output for print_str_list_list
   and print int_list_list below which both use this function. 
*)
let print_list_list print_elem listlist =
  (* YOUR CODE HERE *)
  ()
;;

(* val print_str_list_list : string list list -> unit

   Print all string lists in a list. Each string is printed preceded
   by a space. Otherwise the conventions of print_list_list are used.

   # print_str_list_list strll;;
   [ Korra Mako Bolin Asami]
   [ Tenzin Pema]
   [ Meelo Jinora Iki]
   [ Amon Kuvira Zaheer]
   - : unit = ()
*)
let print_str_list_list =
  (* YOUR CODE HERE *)
  ()
;;

(* val print_int_list_list : int list list -> unit

   Print all int lists in a list. Each integer is printed preceded by
   a space. Otherwise the conventions of print_list_list are used.

   # print_int_list_list intll;;
   [ 1 2 3]
   [ 4 5 6]
   [ 7 8 9 10]
   [ 11]
   - : unit = ()
 *)
let print_int_list_list =
  (* YOUR CODE HERE *)
  ()
;;


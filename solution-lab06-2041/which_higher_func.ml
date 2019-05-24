(* which_higher_func.ml: fill in short definitions of each function
   below using a higher-order List function like filter, iter, map, or
   fold_left. *)

(* val totlen : string list -> int
   Given a list of strings, compute the sum of their lengths 

   # totlen ["abba"; "queen"; "def leopard"];;
   - : int = 20
   # totlen ["miles"; "cannonball"; "coltrane"; "ella"];;
   - : int = 27
*)
let totlen list =
  List.fold_left (fun tot str -> tot + (String.length str)) 0 list
;;


(* val run_thunks : (unit -> unit) list -> unit

   Given a list of functions which take unit and return unit
   ("thunks") call each function with unit input. 

   # run_thunks [(fun ()->for i=1 to 4 do printf "Thunk!\n"; done); 
                 (fun ()->printf "What was that?\n")];;
   Thunk!
   Thunk!
   Thunk!
   Thunk!
   What was that?
   - : unit = ()
   # let xr = ref 5;;
   val xr : int ref = {contents = 5}
   # run_thunks [(fun ()-> xr := 7); 
                 (fun ()-> xr := !xr * 2); 
                 (fun ()-> printf "done\n")];;
   done
   - : unit = ()
   # xr;;
   - : int ref = {contents = 14}
*)
let run_thunks list =
  List.iter (fun f-> f ()) list
;;

(* val func_results : (unit -> 'a) list -> 'a list

   Given a list of functions which take unit and return something,
   create list with the return value given by calling each function
   with unit parameter . 

   # func_results [(fun ()-> 7);
                   (fun ()-> 16);
                   (fun ()-> 8*5+2);];;
     - : int list = [7; 16; 42]

   # func_results [(fun ()-> "hello there");
                   (fun ()-> sprintf "%s %s" "goodbye" "now");];;
     - : string list = ["hello there"; "goodbye now"]
*)
let func_results list =
  List.map (fun f -> f ()) list
;;

(* val keepers : ('a * string * 'b) list -> ('a * string * 'b) list

   Given a list of triples (3-tuples), create a list with elements
   that have string "keep" as the 2nd element of the triple 

   # keepers [(1,"nope",4); (7,"keep",2); (3,"nada",12); (11,"keep",11)];;
   - : (int * string * int) list = [(7, "keep", 2); (11, "keep", 11)]

   # keepers [("a","keep","b"); ("keep","nope",""); ("b","keep","c"); ("d","keep","ee")];;
   - : (string * string * string) list =
   [("a", "keep", "b"); ("b", "keep", "c"); ("d", "keep", "ee")]
*)
let keepers list =
  List.filter (fun (a,b,c)-> b = "keep") list
;;

(* val strlens : string list -> int list

   Given a list of strings, create a list of the lengths of those
   strings 

   # strlens ["abba"; "queen"; "def leopard"];;
   - : int list = [4; 5; 11]
   # strlens ["miles"; "cannonball"; "coltrane"; "ella"];;
   - : int list = [5; 10; 8; 4]
*)
let strlens list =
  List.map String.length list
;;

(* val div57 : int list -> int list

   Given a list of integers, create a list with eleemnts that are
   divisible by 5 or 7 

   # div57 [5;7;11];;
   - : int list = [5; 7]

   # div57 [1;15;21;14;35;36];;
   - : int list = [15; 21; 14; 35]
*)
let div57 list =
  List.filter (fun n-> n mod 5 = 0 || n mod 7 = 0) list
;;

(* val set_sum : float ref -> float list -> unit

   Given a reference to a float and a float list, initialize the
   reference to 0.0 then set the sum of floats in the list 

   # let mysum = ref 2.0;;
   val mysum : float ref = {contents = 2.}

   # set_sum mysum [1.0; 6.0; 9.5];;
   - : unit = ()
   # mysum;;
   - : float ref = {contents = 16.5}

   # set_sum mysum [];;
   - : unit = ()
   # mysum;;
   - : float ref = {contents = 0.}

   # set_sum mysum [6.2; 3.1];;
   - : unit = ()
   # mysum;;
   - : float ref = {contents = 9.3}
*)
let set_sum fref list =
  fref := 0.0;
  List.iter (fun f-> fref := !fref +. f) list;
;;

(* val first_elems : 'a list list -> 'a list

   Given a list of lists, create a list of the first elements. If any
   of the lists are empty, some kind of exception will result.

   # first_elems [[1;2]; [3]; [4;5;6]];;
   - : int list = [1; 3; 4]

   # first_elems [["a"]; ["b";"c";"d"]; ["f";"g"]; ["h";"i"]];;
   - : string list = ["a"; "b"; "f"; "h"]

   # first_elems [];;
   first_elems [];;
   - : 'a list = []

   # first_elems [[1]; []; [3]];;
   Exception: Failure "hd".
*)
let first_elems list =
  List.map List.hd list
;;

(* val find_min : 'a list -> 'a -> 'a
   Given a list of any kind of value and an absolute maximum value,
   return the minimum value in the list or the absolute max if the
   list is empty 

   # max_float;;
   - : float = 1.79769313486231571e+308
   # find_min [7.5; 9.5; 6.3; 2.7; 8.1] max_float;;
   - : float = 2.7
   # find_min [7.5] max_float;;
   - : float = 7.5
   # find_min [] max_float;;
   - : float = 1.79769313486231571e+308
   
   # max_int;;
   - : int = 4611686018427387903
   # find_min [7; 2; 4; 8; 11; 5] max_int;;
   - : int = 2
   # find_min [] max_int;;
   - : int = 4611686018427387903
*)
let find_min list absmax =
  List.fold_left min absmax list
;;

(* Alternate version *)
(* let find_min list absmax =
 *   List.fold_left (fun mn x->if x<mn then x else mn) absmax list
 * ;; *)

(* val transforms : ('a -> 'b) list -> 'a -> 'b list

   Given a list of functions and a data element, apply each function
   to the data and create a list of the results. 


   # let int_funcs = [(fun x-> 2*x);
                      (fun x-> x+7);
                      (fun x-> 0);];;
   val int_funcs : (int -> int) list = [<fun>; <fun>; <fun>]

   # transforms int_funcs 8;;
   - : int list = [16; 15; 0]
   
   # transforms int_funcs 20;;
   - : int list = [40; 27; 0]
   
   # let string_funcs = [(fun x-> x="indeed");
                         (fun x-> (String.length x) > 4)];;
   val string_funcs : (string -> bool) list = [<fun>; <fun>]
   
   # transforms string_funcs "no";;
   - : bool list = [false; false]

   # transforms string_funcs "indeed";;
   - : bool list = [true; true]

   # transforms string_funcs "indubitably";;
   - : bool list = [false; true]
*)
let transforms transf_list x =
  List.map (fun transf -> transf x) transf_list
;;


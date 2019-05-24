(* closure_objects.ml: Demonstrates another way in which closures can
   create data and associated functions to operate on it. This version
   uses records to give objects enable familiar object syntax like 

   x.to_string();
   x.name_change "Moniker Nuevo";
   
   This is proper OCaml as the record fields are bound to functions
*)

open Printf;;

type person = {                        (* type of a person *)
    mutable name : string;             (* data *)
    mutable age  : int;
    birthday     : unit -> unit;       (* "methods": functions operating  *)
    name_change  : string -> unit;     (* on the data above *)
    to_string    : unit -> string;
  };;

(* create a person *)
let make_person name age =
  let rec this = {                     (* need binding for name 'this' *)
      name        = name;              (* define record with data fields *)
      age         = age;               (* and "methods" *)
      birthday    = birthday_func;     (* forward reference to 'birthday_func' *)
      name_change = name_change_func;  (* forward reference to 'name_change_func' *)
      to_string   = to_string_func;
  }
  and birthday_func () =               (* 'and' used for mutual recursion/forward refs *)
    this.age <- this.age + 1;          (* function for incrementing age *)
  and name_change_func name =          (* 'and' again *)
    this.name <- name;                 (* function for changing name *)
  and to_string_func () =
    sprintf "{name=\"%s\"; age=%d; ...}" this.name this.age    
  in
  this                                 (* return the constructed "object" *)
;;

(* main function to demonstrate use of person instances *)
let _ =
  let bob = make_person "Bob Belcher" 42 in
  printf "%s = %s\n" "bob" (bob.to_string ());
  bob.birthday ();
  bob.birthday ();
  printf "%s = %s\n" "bob" (bob.to_string ());
  bob.name_change "Sterling Archer";
  printf "%s = %s\n" "bob" (bob.to_string ());

  let gene = make_person "Eugene Belcher" 11 in
  gene.birthday ();
  printf "%s = %s\n" "gene" (gene.to_string ());
;;
  

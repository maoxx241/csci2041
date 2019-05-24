(* map_modules.ml: provides two modules for maps.
   1. StringStringMap which maps strings to strings
   2. IntpairBoolMap which maps pairs of ints to bools.
   Both modules are created by creating a short module adhering to the
   Treemap.KEYVAL_SIG signature and then invoking the Treemap.Make
   functor. *)

open Printf;;

(* Interface module for maps of string to string *)
module StringStringKV = struct
  type key_t = string;;
  type value_t = string;;
  let compare_keys = String.compare ;;
  let keyval_string = sprintf "{%s -> %s}" ;;
  end ;;

(* A map module from string keys to string values. *)
module StringStringMap = Treemap.Make(StringStringKV);;

(* Interface module for maps of int pairs to bool *)
module IntpairBoolKV = struct
  type key_t = int * int;;
  type value_t = bool;;
  let compare_keys (key11,key12) (key21,key22)= if (key11 - key21) <>0 then key11 - key21 else key12 - key22;;
  let keyval_string (key1,key2) bol= sprintf "{%i > %i : %b}" key1 key2 bol;;
  end;;

(* A map module from int pair keys to bool values. *)
module IntpairBoolMap = Treemap.Make(IntpairBoolKV);;
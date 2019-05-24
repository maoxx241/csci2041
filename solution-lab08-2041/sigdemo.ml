(* sigdemo.ml: demonstrate use of signatures which specify which
   bindings in modules are publicly available *)

module All = struct                   (* a module with no explicit signature *)
  let val1 = 42;;                     (* data *)
  let val2 = "Ta-da!";;               (* data *)
  let func1 x y =                     (* function *)
    let z = x*val1 + y in
    val2 ^ " " ^ (string_of_int z)
  ;;
  let func2 (x,y) = (y,x);;           (* function *)
end;;                                 (* end of module structure *)


module AllSig : sig                   (* a similar module with an explicit signature *)
  val val1 : int;;                    (* 3 values present: int *)
  val val2 : string;;                 (* string *)
  val func1 : int -> int -> string;;  (* function *)
  val func2 : 'a * 'b -> 'b * 'a      (* function *)
end = struct                          (* end of signature, beginning of module structure  *)
  let val1 = 42;;                     (* data *)
  let val2 = "Ta-da!";;               (* data  *)
  let func1 x y =                     (* function *)
    let z = x*val1 + y in
    val2 ^ " " ^ (string_of_int z)
  ;;
  let func2 (x,y) = (y,x);;           (* function *)
end;;                                 (* end of module structure *)

module Func1Only : sig                (* a module with a more restrictive signature *)
  val func1 : int -> int -> string;;  (* only a function is present *)
end = struct                          (* end of signature, beginning of module structure  *)
  let val1 = 42;;                     (* data *)
  let val2 = "Ta-da!";;               (* data *)
  let func1 x y =                     (* function *)
    let z = x*val1 + y in
    val2 ^ " " ^ (string_of_int z)
  ;;
  let func2 (x,y) = (y,x);;           (* function *)
end;;                                 (* end of module structure *)

module type ONLY2S = sig              (* named signature for a module *)
  val val2 : string;;                 (* string *)
  val func2 : 'a * 'b -> 'b * 'a      (* function *)
end;;

module A2SModule : ONLY2S =           (* module using named signature ONLY2S *)
struct
  let func2 (x,y) = (y,x);;           (* accessible *)
  let z = "zip";;                     (* hidden *)
  let w = 86;;                        (* hidden *)
  let val2 = z;;                      (* accessible *)
end;;

module Another2S : ONLY2S =           (* module using named signature ONLY2S *)
struct
  let x = 17;;                        (* hidden *)
  let y = "hi";;                      (* hidden *)
  let val2 = y;;                      (* accessible *)
  let f () = true;;                   (* hidden *)
  let func2 (x,y) = (y,x);;           (* accessible *)
end;;

module RestrictAll : ONLY2S =         (* module using named signature ONLY2S *)
  All                                 (* alias for existing module, new signature *)
;;


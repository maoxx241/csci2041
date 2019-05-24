(* memfunc1.ml: provides a functor to memoize functions of one
   argument. *)

(* interface signature for Memoize functor *)
module type FUNC1_SIG = sig
  type arg_t;;                                           (* function argument type *)
  type ret_t;;                                           (* function return type *)
  val func : arg_t -> ret_t;;                            (* the function itself *)
  val compare_arg : arg_t -> arg_t -> int;;              (* comparison function argument types *)
end;;

module Memoize(FuncMod : FUNC1_SIG) : sig                (* functor to memoize 1-arg functions *)
  val call : FuncMod.arg_t -> FuncMod.ret_t              (* restrict signature of output module so only call is visible *)
end =                                                    (* end of signature *)
struct                                                   (* begining of functor output sturcture *)

  module MapInterface = struct                           (* interface module to to adapt to Make.Make *)
    type t = FuncMod.arg_t;;
    let compare = FuncMod.compare_arg;;
  end

  module MemoMap = Map.Make(MapInterface);;              (* create a map module for mapping arg_t to ret_t *)

  let arg_ret_map = ref MemoMap.empty;;                  (* a mutable reference to a map, initially empty *)
                                                         (* used to track correspondence of arg to ret value *)

  let call arg =                                         (* publicly visible binding: call the memoized function *)
    let ret_opt = MemoMap.find_opt arg !arg_ret_map in   (* look up arg in the map *)
    match ret_opt with                                   (* match the returned option *)
    | Some ret -> ret                                    (* previous call used arg, return associate ret value *)
    | None ->                                            (* no previous call used arg *)
       let ret = FuncMod.func arg in                     (* call the function to generate the return value *)
       arg_ret_map := MemoMap.add arg ret !arg_ret_map;  (* store it in the map for later retrieval *)
       ret                                               (* return the return value *)
  ;;

end

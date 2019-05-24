(* custom_lazy.ml: optional problem exploring possible implementations
   of lazy expressions. *)


type 'a lazy_expr = {               (* type for lazy expressions *)
    expr           : unit -> 'a;    (* expression to evaluate *)
    mutable result : 'a option;     (* saved results, None if uneval'd yet *)
};;

let my_lazy expr =                  (* create a lazy version of expr *)
  {expr   = (fun () -> expr);
   result = None}
;;

let my_force lazy_expr =            (* force a value out of the lazy_expr *)
  match lazy_expr.result with
  | Some a -> a                     (* already evaluated *)
  | None ->                         (* not eval'd yet *)
     let a = lazy_expr.expr () in   (* eval *)
     lazy_expr.result <- Some a;    (* save result *)
     a                              (* return result *)
;;

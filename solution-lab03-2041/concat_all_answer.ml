(* pattern matching version: concatenate several string *)
let rec concat_all strlist =
  match strlist with
  | [] -> ""
  | head :: []   -> head
  | head :: tail -> head ^ " " ^ (concat_all tail)
;;

(* # concat_all [];;
   - : string = ""
   # concat_all ["Fold-em"];;
   - : string = "Fold-em"
   # concat_all ["Muh";"muh"];;
   - : string = "Muh muh"
   # concat_all ["P"; "p"; "p"; "poker"; "face"];;
   - : string = "P p p poker face"
*)

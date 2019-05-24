(* sample implementation of fold_left *)
let fold_left func init list =
  let rec help cur lst =
    match lst with
    | []         -> cur
    | head::tail -> let next = func cur head in
                    help next tail
  in
  help init list
;;

(* sample implementation of reduce_left *)
let reduce_left func list =
  let rec helper cur lst =
    match lst with
    | [] -> cur
    | head::tail -> helper (func cur head) tail
  in
  match list with
  | [] -> failwith "Empty list"
  | head::tail -> helper head tail
;;
    

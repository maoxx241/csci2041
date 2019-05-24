let rec last_elem list =
  if list = [] then
    raise (Failure "No last element in an empty list")
  else
    let elem = List.hd list in
    let rest = List.tl list in
    if rest = [] then
      elem
    else
      last_elem rest
;;

let elems_outside start stop list =
  let rec helper pos lst =
    if lst=[] then
      []
    else if start<=pos && pos<=stop then
      helper (pos+1) (List.tl lst)
    else
      let elem = List.hd lst in
      let rest = List.tl lst in
      let result =  helper (pos+1) rest in
      elem :: result
  in
  helper 0 list
;;

(* REPL EXAMPLES
   # elems_outside 3 5 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 1; 2; 6; 7]
   # elems_outside 1 5 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 6; 7]
   # elems_outside 2 4 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 1; 5; 6; 7]
   # elems_outside 2 4 [];;
   - : 'a list = []
*)

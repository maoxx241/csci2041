(* Returns the last element in a list. Raises an exception if the list
   is empty. *)
let rec last_elem list =
  if list = [] then             (* error case of empty list, raise exception *)
    raise (Failure "No last element in an empty list")
  else                          (* non-empty list *)
    let elem = List.hd list in  (* peel off the first element *)
    let rest = List.tl list in  (* get the rest of the list *)
    if rest = [] then           (* if the rest is empty *)
      elem                      (* the elem is the last, return it *)
    else
      last_elem rest            (* otherwise recurse on remaining list *)
;;

(* Returns elements in a list outside of the given indices
   start/stop. Elements at start/stop are not included in the
   result. Uses an internal recursive helper method. *)
let elems_outside start stop list =
  let rec helper pos lst =                   (* recursive helper *)
    if lst=[] then                           (* check for end of list/empty *)
      []                                     (* return empty *)
    else if start<=pos && pos<=stop then     (* between start/stop *)
      helper (pos+1) (List.tl lst)           (* recurse but don't cons on any parts *)
    else                                     (* outside start/stop *)
      let elem = List.hd lst in              (* peel off first element *)
      let rest = List.tl lst in              (* get rest of list *)
      let result =  helper (pos+1) rest in   (* recurse on remainder of list *)
      elem :: result                         (* cons on current element *)
  in                                         (* end helper *)
  helper 0 list                              (* call helper at beginning of list *)
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

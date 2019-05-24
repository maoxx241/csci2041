(* Produce a new list which has the give elt inserted into it in
   sorted order. Presumes that the list is already sorted. *)

let rec sorted_insert elt list =
  if list=[] then
    [elt]
  else
    let head = List.hd list in
    let tail = List.tl list in
    if elt <= head then
      elt :: list
    else
      head :: (sorted_insert elt tail)
;;

(* Sort a list through repeated insertions. *)
let rec sort list =
  if list=[] then
    []
  else
    let head = List.hd list in
    let tail = List.tl list in
    let sorted_tail = sort tail in
    sorted_insert head sorted_tail
;;

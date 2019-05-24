let rec concat_all_crap strlist =
  if strlist=[] then
    ""
  else
    let head = List.hd strlist in
    let tail = List.tl strlist in
    let rest = concat_all_crap tail in
    head ^ " " ^ rest
;;  

let rec concat_all_good strlist =
  if strlist=[] then
    ""
  else
    let head = List.hd strlist in
    let tail = List.tl strlist in
    if tail=[] then
      head
    else
      let rest = concat_all_good tail in
      head ^ " " ^ rest
;;


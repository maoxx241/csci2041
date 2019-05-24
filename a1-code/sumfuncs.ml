let array_sum arr =
    let result= ref 0 in
    for i=0 to (Array.length arr)-1 do
        let next= !result+ arr.(i) in
        result:=next;
        done;
        !result;;


(* let rec list_sum lst =
    match lst with
    | x1::n->x1+list_sum n
    |[]-> 0 *)

    let rec list_sum lst =

    if lst=[] then 0
        else
        let first_element= List.hd lst in
        let remain=List.tl lst in
        first_element+list_sum remain

let array_above thresh arr = 
    let func thresh arr =
    let n= ref 0 in
    for i=0 to (Array.length arr)-1 do
        if arr.(i)>thresh 
        then 
        let next= !n+1 in
        n:=next;
        done;
   
    let result = Array.make !n arr.(0) in 
    let index= ref 0 in
    for i=0 to (Array.length arr)-1 do
        if arr.(i)>thresh 
        then result.(!index)<-arr.(i);
        if arr.(i)>thresh 
        then let next= !index+1 in
        index:=next;
        done;
        result;
        in
    if Array.length arr >0 then func thresh arr else arr;;


(* let rec list_above thresh lst = 
    match lst with
    | x::n-> if x>thresh then x:: list_above thresh n else list_above thresh n
    |[]-> [] *)

let rec list_above thresh lst = 
    if lst = [] then
        [] else 
        let first_element= List.hd lst in
        let remain=List.tl lst in
        if first_element > thresh then first_element:: list_above thresh remain else list_above thresh remain;;

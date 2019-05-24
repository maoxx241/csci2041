let array_rev arr =
if Array.length arr >1 then
let temp =Array.make 1 arr.(0) in
let x=  ((Array.length arr)-1)/2 in
let index=ref x in
let i= ref 0 in
let quit_loop = ref false in
while not !quit_loop do
    temp.(0)<-arr.(!i);
    let n=Array.length arr - 1- !i in
        arr.(!i)<-arr.(n);
        arr.(n)<- temp.(0);
    let next= !i+1 in
        i:=next;
    if !i> !index then quit_loop :=true
    done;;



(* let list_rev lst =
    let rec func result lst=
    match lst with
    | x::n -> func (x::result) n
    | []->result in
    func [] lst *)

    let list_rev lst =
        let rec func result lst=
        if lst=[] then result else
            let first_element= List.hd lst in
            let remain=List.tl lst in
        func (first_element::result) remain
        in
        func [] lst

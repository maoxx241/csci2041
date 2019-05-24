let rec insert list elem =
    match list with
    | x::xs -> if elem=x then list else if elem <x then elem::x::xs else x::insert xs elem
    | []-> [elem]

let rec remove list elem=
    match list with
    | x::xs -> if elem = x then xs else x::remove xs elem
    | []->[]

let rec print strlist=
    match strlist with
    (* Printf.fprintf stdout "%s" x; *)
    | x::xs ->  Pervasives.print_endline x; print xs;
    | [] -> ()

let rec merge lista listb =
    match lista,listb with
    | x::xs,y::ys -> if x=y then x::merge xs ys else if x<y then x::merge xs listb else y::merge lista ys
    | _,[] -> lista
    | [],_ -> listb

(* let rec merge lista listb =
    match lista with
    | x::xs -> insert listb x 
    | [] -> listb *)
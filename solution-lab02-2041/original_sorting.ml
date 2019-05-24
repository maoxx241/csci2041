(* Original version of sort/insert from Ocaml System Manual *)
let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
;;

(* Version with pattern matching and guards rather than if/else *)
let rec sort lst =
  match lst with
    [] -> []
  | head::tail -> insert head (sort tail)
and insert elt lst =
  match lst with
  | [] ->
     [elt]
  | head::tail when elt<=head ->
     elt :: lst
  | head::tail ->
     head :: (insert elt tail)
;;

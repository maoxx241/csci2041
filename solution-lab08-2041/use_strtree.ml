(* use_strtree.ml: demonstrate use of the strtree type defined in
   strtree.ml. *)

open Printf;;
module ST = Strtree;;                      (* shorter module alias *)
      
let data1 = [                              (* small amount of data for the tree *)
    "Mario"; "Donkey Kong"; "Luigi";
    "Princess"; "Toad"; 
];;

let data2 = [                              (* larger amount of data *)
    "Bowser"; "Buzzy Beetle"; "Wario"; 
    "Goomba"; "Koopa"; "Bullet Bill"; 
    "Bob-omb";  "Chain Chomp"; "Pokey";
    "Thwomp";
];;

let _ =
  let tree1 = ST.Empty in
  let tree2 = ST.add tree1 "Mario" in
  let tree3 = ST.add tree2 "Luigi" in
  let tree4 = ST.add tree3 "Princess" in
  printf "tree1:\n";
  printf "%s\n" (ST.tree_string tree1);    (* print tree1 *)
  printf "tree2:\n";
  printf "%s\n" (ST.tree_string tree2);    (* print tree2 *)
  printf "tree3:\n";
  printf "%s\n" (ST.tree_string tree3);    (* print tree3 *)
  printf "tree4:\n";
  printf "%s\n" (ST.tree_string tree4);    (* print tree4 *)

  let liltree =                            (* tree of data1 through folding *)
    List.fold_left ST.add ST.Empty data1
  in
  printf "liltree:\n";
  printf "%s\n" (ST.tree_string liltree);  (* print liltree *)

  let bigtree =                            (* second tree with data1 and data2 *)
    List.fold_left ST.add liltree data2
  in
  printf "bigtree:\n";
  printf "%s\n" (ST.tree_string bigtree);  (* print bigtree *)
  printf "liltree:\n";
  printf "%s\n" (ST.tree_string liltree);  (* print liltree again *)
;;


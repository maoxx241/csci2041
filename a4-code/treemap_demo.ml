(* treemap_demo.ml: demonstrate Treemap.Make and make use of the
   modules defined in map_modules.ml *)

open Printf;;

(* The below code will compile/run after completing Treemap *)
module IntIntKV = struct
  type key_t = int;;
  type value_t = int;;
  let compare_keys x y = x - y;;
  let keyval_string k v = sprintf "{%d -> %d}" k v;;
end;;

module IntIntMap = Treemap.Make(IntIntKV);;

let _ =
  let map1 = IntIntMap.add IntIntMap.empty 10 20 in
  let map2 = IntIntMap.add map1 15 30 in
  let map3 = IntIntMap.add map2 25 50 in
  printf "map3:\n%s\n" (IntIntMap.to_string map3);
  let map4 = IntIntMap.add map3 5 10 in
  let map5 = IntIntMap.add map4 7 14 in
  printf "map5:\n%s\n" (IntIntMap.tree_string map5);
  let addvals tot _ v = tot+v in
  let valsum = IntIntMap.fold addvals 0 map5 in
  printf "valsum: %d\n" valsum;
;;


(* The below code will compile/run *)
let _ =
  let module SSM = Map_modules.StringStringMap in
  let map1 = SSM.empty in
  let map2 = SSM.add map1 "17" "Seventeen" in
  let map3 = SSM.add map2 "99" "Ninety-nine" in
  printf "map3:\n%s\n" (SSM.tree_string map3);

  let module IPBM = Map_modules.IntpairBoolMap in
  let map = IPBM.empty in
  let map = IPBM.add map (5,10) true  in
  let map = IPBM.add map (10,5) false in
  let map = IPBM.add map (12,7) false in
  let map = IPBM.add map (7,12) true in
  let map = IPBM.add map (3,5) false in
  let map = IPBM.add map (3,2) false in
  printf "map:\n%s\n" (IPBM.tree_string map);
;;

  

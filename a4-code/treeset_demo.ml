(* treeset_samples.ml: Demonstrate use of the Treeset.Make functor to
   create Treeset modules specialized to various types. *)

open Printf;;

module IntType =         (* defines interface for ints *)
struct                                         (* to be elements of a Treeset *)
  type element = int;;
  let compare x y = x - y;;
  let elem_string = string_of_int;;
end;;

module IntTreeset = Treeset.Make(IntType);;    (* Treeset specialized for int elements *)

module StrType  =                              (* defines interface for strings *)
struct                                         (* to be elements of a Treeset *)
  type element = string;;
  let compare = String.compare;;
  let elem_string x = x;;
end;;

module StrTreeset = Treeset.Make(StrType);;   (* Treeset specialized for string elements *)

type city = {                                  (* a record describing cities *)
    name     : string;                         (* name of city *)
    country  : string;                         (* country location *)
    pop_mill : float;                          (* population in millions *)
  };;

module CityType =                              (* defines interface for cities *)
struct
  type element = city;;                        (* to be elements of a Treeset *)
  let compare c d =
    String.compare c.name d.name
  ;;
  let elem_string c =
    Printf.sprintf "%s, %s: %.2f million"
      c.name c.country c.pop_mill
  ;;
end;;

module CityTreeset = Treeset.Make(CityType);;  (* Treeset specialized to cities *)
           
let _ =
  (* Demo the IntTreeset *)
  let inttree1 = IntTreeset.empty in
  let inttree2 = IntTreeset.add inttree1 17 in
  let inttree3 = IntTreeset.add inttree2 99 in
  printf "inttree3:\n%s\n" (IntTreeset.tree_string inttree3);

  let int_list = [50; 25; 42; 11; 86; 61; 99; 14; 75] in
  let inttree4 =
    List.fold_left IntTreeset.add IntTreeset.empty int_list
  in
  printf "inttree4:\n%s\n" (IntTreeset.tree_string inttree4);
  let sum = IntTreeset.fold (+) 0 inttree4 in
  printf "sum: %d\n" sum;

  (* Demo StrTreeset *)
  let str_list = ["K"; "D"; "R"; "M"; "N"; "Q"; "E"; "Z"; "A"; "T"; "W";] in
  let strtree1 =
    List.fold_left StrTreeset.add StrTreeset.empty str_list
  in
  printf "strtree1:\n%s\n" (StrTreeset.tree_string strtree1);

  let strtree2 = StrTreeset.remove strtree1 "M" in
  let strtree3 = StrTreeset.remove strtree2 "K" in
  printf "strtree3:\n%s\n" (StrTreeset.tree_string strtree3);
  
  (* list of most populous cities: https://en.wikipedia.org/wiki/List_of_largest_cities *)
  let cit_list = [
      {name="Chongqing" ; country="China"      ; pop_mill=30.75};
      {name="Shanghai"  ; country="China"      ; pop_mill=24.25};
      {name="Delhi"     ; country="India"      ; pop_mill=11.03};
      {name="Beijing"   ; country="China"      ; pop_mill=21.51};
      {name="Dhaka"     ; country="Bangladesh" ; pop_mill=14.39};
      {name="Mumbai"    ; country="India"      ; pop_mill=12.47};
      {name="Lagos"     ; country="Nigeria"    ; pop_mill=16.06};
      {name="Chengdu"   ; country="China"      ; pop_mill=16.04};
      {name="Karachi"   ; country="Pakistan"   ; pop_mill=14.91};
      {name="Guangzhou" ; country="China"      ; pop_mill=14.04};
      {name="Istanbul"  ; country="Turkey"     ; pop_mill=14.02};
      {name="Tokyo"     ; country="Japan"      ; pop_mill=13.83};
      {name="Tianjin"   ; country="China"      ; pop_mill=12.78};
      {name="Moscow"    ; country="Russia"     ; pop_mill=12.19};
      {name="Sao Paulo" ; country="Brazil"     ; pop_mill=12.03};
    ]
  in

  (* Demo CityTreeset *)
  let citytree1 =
    List.fold_left CityTreeset.add CityTreeset.empty cit_list
  in
  printf "citytree1:\n%s\n" (CityTreeset.tree_string citytree1);
    
  let citytree2 = CityTreeset.remove citytree1 (List.hd cit_list) in
  printf "citytree2:\n%s\n" (CityTreeset.tree_string citytree2);
;;
    
  

open Printf;;
open Mltest;;


(* files for diffing *)
let actual_file = "test-data/actual.tmp";;
let expect_file = "test-data/expect.tmp";;
let diff_file   = "test-data/diff.tmp"  ;;
let msgref = ref "";;

let string_of_stropt o =
  match o with
  | None -> "None"
  | Some s -> sprintf "Some \"%s\"" s
;;

let make_getopt_msg key map expect actual =
  let lines = [
      sprintf "Key:    %s" key;
      sprintf "EXPECT: %s" (string_of_stropt expect);
      sprintf "ACTUAL: %s" (string_of_stropt actual);
      sprintf "Tree String for Map:\n%s" (Ssmap.tree_string map);
    ]
  in
  String.concat "\n" lines
;;

let make_contains_key_msg key map expect actual =
  let lines = [
      sprintf "Key:    %s" key;
      sprintf "EXPECT: %s" (string_of_bool expect);
      sprintf "ACTUAL: %s" (string_of_bool actual);
      sprintf "Tree String for Map:\n%s" (Ssmap.tree_string map);
    ]
  in
  String.concat "\n" lines
;;

let make_iterfold_msg map funcstr expect actual =
  let lines = [
      sprintf "%s" funcstr;
      sprintf "EXPECT: %s" expect;
      sprintf "ACTUAL: %s" actual;
      sprintf "Tree String for Map:\n%s" (Ssmap.tree_string map);
    ]
  in
  String.concat "\n" lines
;;


let make_remove_msg key original =
  let lines = [
      sprintf "REMOVAL PRODUCES WRONG TREE";
      sprintf "- Remove Key: %s" key;
      sprintf "- Original Tree:";
      sprintf "%s\n" (Ssmap.tree_string original);
    ] in
  String.concat "\n" lines
;;

let singleton = Ssmap.add Ssmap.empty "Mario" "plumber";;

let smallmap =
  let map = Ssmap.empty in
  let map = Ssmap.add map "Mario"    "plumber"  in
  let map = Ssmap.add map "Toad"     "retainer" in
  let map = Ssmap.add map "Luigi"    "plumber"  in
  let map = Ssmap.add map "Princess" "royalty"  in
  map
;;

(* map for a larger tests *)
let bigmap =
  let entries = [
      "Mouser"    , "1";
      "Cobrat"    , "2";
      "Tweeter"   , "3";
      "Pokey"     , "4";
      "Phanto"    , "5";
      "Beezo"     , "6";
      "Toad"      , "7";
      "Pidgit"    , "8";
      "Ostro"     , "9";
      "Autobomb"  ,"10";
      "Wart"      ,"11";
      "Hoopster"  ,"12";
      "Spark"     ,"13";
      "Mario"     ,"14";
      "Albatoss"  ,"15";
      "Tryclyde"  ,"16";
      "Flurry"    ,"17";
      "Bob-omb"   ,"18";
      "Hawkmouth" ,"19";
      "Trouter"   ,"20";
      "Snifit"    ,"21";
      "Ninji"     ,"22";
      "ShyGuy"    ,"23";
      "Porcupo"   ,"24";
      "Luigi"     ,"25";
      "Princess"  ,"26";
      "Panser"    ,"27";
      "Clawgrip"  ,"28";
      "Whale"     ,"29";
    ] in
  let addkv map (k,v) = Ssmap.add map k v in
  List.fold_left addkv Ssmap.empty entries
;;

(* map for a larger tests *)
let bigmap2 =
  let entries = [
      "Beezo"     , "1" ;
      "Bob-omb"   , "2" ;
      "Spark"     , "3" ;
      "Mouser"    , "4" ;
      "Cobrat"    , "5" ;
      "Tweeter"   , "6" ;
      "Pokey"     , "7" ;
      "Phanto"    , "8" ;
      "Albatoss"  , "9" ;
      "Toad"      ,"10" ;
      "Pidgit"    ,"11" ;
      "Ostro"     ,"12" ;
      "Autobomb"  ,"13" ;
      "Wart"      ,"14" ;
      "Hoopster"  ,"15" ;
      "Mario"     ,"16" ;
      "Tryclyde"  ,"17" ;
      "Flurry"    ,"18" ;
      "Hawkmouth" ,"19" ;
      "Trouter"   ,"20" ;
      "Snifit"    ,"21" ;
      "Ninji"     ,"22" ;
      "ShyGuy"    ,"23" ;
      "Porcupo"   ,"24" ;
      "Luigi"     ,"25" ;
      "Princess"  ,"26" ;
      "Panser"    ,"27" ;
      "Clawgrip"  ,"28" ;
      "Whale"     ,"29" ;
    ] in
  let addkv map (k,v) = Ssmap.add map k v in
  List.fold_left addkv Ssmap.empty entries
;;


Mltest.main [|
(******************************************)
(* ssmap.ml tests *)

(fun () ->
  (* getopt calls in an empty tree *)
  let map = Ssmap.empty in
  
  (* BEG_TEST *)
  let key = "Mario" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luigi" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* getopt calls in a singleton tree *)
  let map = Ssmap.empty in
  let map = Ssmap.add map "Mario"    "plumber"  in
  
  (* BEG_TEST *)
  let key = "Mario" in
  let actual = Ssmap.getopt map key in
  let expect = Some "plumber" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luigi" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Princess" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* successful getopt calls in a small tree *)
  
  (* BEG_TEST *)
  let key = "Mario" in
  let actual = Ssmap.getopt smallmap key in
  let expect = Some "plumber" in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luigi" in
  let actual = Ssmap.getopt smallmap key in
  let expect = Some "plumber" in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Princess" in
  let actual = Ssmap.getopt smallmap key in
  let expect = Some "royalty" in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Toad" in
  let actual = Ssmap.getopt smallmap key in
  let expect = Some "retainer" in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* failure getopt calls in a small tree *)
  
  (* BEG_TEST *)
  let key = "Bowser" in
  let actual = Ssmap.getopt smallmap key in
  let expect = None in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Morton" in
  let actual = Ssmap.getopt smallmap key in
  let expect = None in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Wario" in
  let actual = Ssmap.getopt smallmap key in
  let expect = None in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luma" in
  let actual = Ssmap.getopt smallmap key in
  let expect = None in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Peach" in
  let actual = Ssmap.getopt smallmap key in
  let expect = None in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "ShyGuy" in
  let actual = Ssmap.getopt smallmap key in
  let expect = None in
  let msg = make_getopt_msg key smallmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (*  getopt calls in a large tree 1 *)
  (* BEG_TEST *)
  let key = "ShyGuy" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "23" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Mario" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "14" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Princess" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "26" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Clawgrip" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "28" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Wart" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "11" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (*  getopt calls in a large tree 2 *)

  (* BEG_TEST *)
  let key = "Porcupo" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "24" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Ninji" in
  let actual = Ssmap.getopt bigmap key in
  let expect = Some "22" in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Wario" in
  let actual = Ssmap.getopt bigmap key in
  let expect = None in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Bowser" in
  let actual = Ssmap.getopt bigmap key in
  let expect = None in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Koopa" in
  let actual = Ssmap.getopt bigmap key in
  let expect = None in
  let msg = make_getopt_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);


(fun () ->
  (* contains_key calls 1 *)
  let map = Ssmap.empty in
  
  (* BEG_TEST *)
  let key = "Mario" in
  let actual = Ssmap.contains_key map key in
  let expect = false in
  let msg = make_contains_key_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luigi" in
  let actual = Ssmap.contains_key map key in
  let expect = false in
  let msg = make_contains_key_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Princess" in
  let actual = Ssmap.contains_key bigmap key in
  let expect = true in
  let msg = make_contains_key_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Clawgrip" in
  let actual = Ssmap.contains_key bigmap key in
  let expect = true in
  let msg = make_contains_key_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* contains_key calls 2 *)

  (* BEG_TEST *)
  let key = "Wart" in
  let actual = Ssmap.contains_key bigmap key in
  let expect = true in
  let msg = make_contains_key_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Wario" in
  let actual = Ssmap.contains_key bigmap key in
  let expect = false in
  let msg = make_contains_key_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Bowser" in
  let actual = Ssmap.contains_key bigmap key in
  let expect = false in
  let msg = make_contains_key_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Koopa" in
  let actual = Ssmap.contains_key bigmap key in
  let expect = false in
  let msg = make_contains_key_msg key bigmap expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* iter calls on empty *)
  let empty = Ssmap.empty in

  (* BEG_TEST *)
  let str = ref "" in
  let concat_keys k v = str := !str ^ k ^ " " in
  let funcstr = "concatenate all keys in a ref" in
  Ssmap.iter concat_keys empty;
  let expect = "" in
  let actual = !str in
  let msg = make_iterfold_msg empty funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let str = ref "" in
  let concat_values k v = str := !str ^ v ^ " " in
  let funcstr = "concatenate all value strings in a ref" in
  Ssmap.iter concat_values empty;
  let expect = "" in
  let actual = !str in
  let msg = make_iterfold_msg empty funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let sum = ref 0 in
  let add_values k v = sum := !sum + (int_of_string v) in
  let funcstr = "add all values in a ref" in
  Ssmap.iter add_values empty;
  let expect = 0 in
  let actual = !sum in
  let msg = make_iterfold_msg empty funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)
);  

(fun () ->
  (* iter calls on small/big map 1 *)

  (* BEG_TEST *)
  let str = ref "" in
  let concat_keys k v = str := !str ^ k ^ " " in
  let funcstr = "concatenate all keys in a ref" in
  Ssmap.iter concat_keys smallmap;
  let expect = "Luigi Mario Princess Toad " in
  let actual = !str in
  let msg = make_iterfold_msg smallmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let str = ref "" in
  let concat_values k v = str := !str ^ v ^ " " in
  let funcstr = "concatenate all vlaues in a ref" in
  Ssmap.iter concat_values smallmap;
  let expect = "plumber plumber royalty retainer " in
  let actual = !str in
  let msg = make_iterfold_msg smallmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let str = ref "" in
  let concat_keys k v = str := !str ^ k ^ " " in
  let funcstr = "concatenate all keys in a ref" in
  Ssmap.iter concat_keys bigmap;
  let expect = "Albatoss Autobomb Beezo Bob-omb Clawgrip Cobrat Flurry Hawkmouth Hoopster Luigi Mario Mouser Ninji Ostro Panser Phanto Pidgit Pokey Porcupo Princess ShyGuy Snifit Spark Toad Trouter Tryclyde Tweeter Wart Whale " in
  let actual = !str in
  let msg = make_iterfold_msg bigmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let str = ref "" in
  let concat_values k v = str := !str ^ v ^ " " in
  let funcstr = "concatenate all vlaues in a ref" in
  Ssmap.iter concat_values bigmap;
  let expect = "15 10 6 18 28 2 17 19 12 25 14 1 22 9 27 5 8 4 24 26 23 21 13 7 20 16 3 11 29 " in
  let actual = !str in
  let msg = make_iterfold_msg bigmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* iter calls on small/big map 2 *)

  (* BEG_TEST *)
  let sum = ref 0 in
  let add_vallengths k v = sum := !sum + (String.length v) in
  let funcstr = "add lengths of values in a ref" in
  Ssmap.iter add_vallengths smallmap;
  let expect = 29 in
  let actual = !sum in
  let msg = make_iterfold_msg smallmap funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let sum = ref 0 in
  let add_valints k v = sum := !sum + (int_of_string v) in
  let funcstr = "add values as ints in a ref" in
  Ssmap.iter add_valints bigmap;
  let expect = 435 in
  let actual = !sum in
  let msg = make_iterfold_msg bigmap funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

);

(fun () ->
  (* fold calls on empty map *)
  let empty = Ssmap.empty in

  (* BEG_TEST *)
  let concat_keys cur k v = cur ^ k ^ " " in
  let funcstr = "concatenate all keys in a fold" in
  let actual = Ssmap.fold concat_keys "" empty in
  let expect = "" in
  let msg = make_iterfold_msg empty funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let concat_values cur k v = cur ^ v ^ " " in
  let funcstr = "concatenate all vlaues in a fold" in
  let actual = Ssmap.fold concat_values "" empty in
  let expect = "" in
  let msg = make_iterfold_msg empty funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let add_vallengths sum k v = sum + (String.length v) in
  let funcstr = "add lengths of values in a fold" in
  let actual = Ssmap.fold add_vallengths 0 empty in
  let expect = 0 in
  let msg = make_iterfold_msg empty funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* fold calls on small/big map 1 *)

  (* BEG_TEST *)
  let concat_keys cur k v = cur ^ k ^ " " in
  let funcstr = "concatenate all keys in a fold" in
  let actual = Ssmap.fold concat_keys "" smallmap in
  let expect = "Luigi Mario Princess Toad " in
  let msg = make_iterfold_msg smallmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let concat_values cur k v = cur ^ v ^ " " in
  let funcstr = "concatenate all vlaues in a fold" in
  let actual = Ssmap.fold concat_values "" smallmap in
  let expect = "plumber plumber royalty retainer " in
  let msg = make_iterfold_msg smallmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let concat_keys str k v = str ^ k ^ " " in
  let funcstr = "concatenate all keys in a fold" in
  let actual = Ssmap.fold concat_keys "" bigmap in
  let expect = "Albatoss Autobomb Beezo Bob-omb Clawgrip Cobrat Flurry Hawkmouth Hoopster Luigi Mario Mouser Ninji Ostro Panser Phanto Pidgit Pokey Porcupo Princess ShyGuy Snifit Spark Toad Trouter Tryclyde Tweeter Wart Whale " in
  let msg = make_iterfold_msg bigmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let concat_values str k v = str ^ v ^ " " in
  let funcstr = "concatenate all vlaues in a fold" in
  let actual = Ssmap.fold concat_values "" bigmap in
  let expect = "15 10 6 18 28 2 17 19 12 25 14 1 22 9 27 5 8 4 24 26 23 21 13 7 20 16 3 11 29 " in
  let msg = make_iterfold_msg bigmap funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* fold calls on small/big map *)

  (* BEG_TEST *)
  let add_vallengths sum k v = sum + (String.length v) in
  let funcstr = "add lengths of values in a fold" in
  let actual = Ssmap.fold add_vallengths 0 smallmap in
  let expect = 29 in
  let msg = make_iterfold_msg smallmap funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let add_valints sum k v = sum + (int_of_string v) in
  let funcstr = "add values as ints in a fold" in
  let actual = Ssmap.fold add_valints 0 bigmap in
  let expect = 435 in
  let msg = make_iterfold_msg bigmap funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

);

(fun () ->
  (* to_string on empty map *)
  (* BEG_TEST *)
  let actual = Ssmap.to_string Ssmap.empty in
  let expect = "[]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* to_string on smallmap *)
  (* BEG_TEST *)
  let actual = Ssmap.to_string smallmap in
  let expect = "[{Luigi -> plumber}, {Mario -> plumber}, {Princess -> royalty}, {Toad -> retainer}]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* to_string on bigmap *)
  (* BEG_TEST *)
  let actual = Ssmap.to_string bigmap in
  let expect = "[{Albatoss -> 15}, {Autobomb -> 10}, {Beezo -> 6}, {Bob-omb -> 18}, {Clawgrip -> 28}, {Cobrat -> 2}, {Flurry -> 17}, {Hawkmouth -> 19}, {Hoopster -> 12}, {Luigi -> 25}, {Mario -> 14}, {Mouser -> 1}, {Ninji -> 22}, {Ostro -> 9}, {Panser -> 27}, {Phanto -> 5}, {Pidgit -> 8}, {Pokey -> 4}, {Porcupo -> 24}, {Princess -> 26}, {ShyGuy -> 23}, {Snifit -> 21}, {Spark -> 13}, {Toad -> 7}, {Trouter -> 20}, {Tryclyde -> 16}, {Tweeter -> 3}, {Wart -> 11}, {Whale -> 29}]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);


(fun () ->
  (* findmin_keyval on empty map, raises exception *)
  (* BEG_TEST *)
  try
    let _ = Ssmap.findmin_keyval Ssmap.empty in
    let msg = "Should have raised an exception" in
    __check__ ( false );
  with Failure(actual) ->
    begin
      let expect = "No minimum in an empty tree" in
      let msg = sprintf "EXPECT message: %s\nACTUAL message: %s\n" expect actual in
      __check__ ( expect = actual );
    end
  (* END_TEST *)
);

(fun () ->
  (* findmin_keyval on singleton map *)
  (* BEG_TEST *)
  let map = Ssmap.add Ssmap.empty "Mario" "plumber" in
  let (actkey,actval) as actual = Ssmap.findmin_keyval map in
  let (expkey,expval) as expect = ("Mario","plumber") in
  let msg = sprintf "EXPECT: (%s,%s)\nACTUAL: (%s,%s)\n" expkey expval actkey actval in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* findmin_keyval on small/big map *)
  (* BEG_TEST *)
  let (actkey,actval) as actual = Ssmap.findmin_keyval smallmap in
  let (expkey,expval) as expect = ("Luigi","plumber") in
  let msg = sprintf "EXPECT: (%s,%s)\nACTUAL: (%s,%s)\n" expkey expval actkey actval in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let (actkey,actval) as actual = Ssmap.findmin_keyval bigmap in
  let (expkey,expval) as expect = ("Albatoss","15") in
  let msg = sprintf "EXPECT: (%s,%s)\nACTUAL: (%s,%s)\n" expkey expval actkey actval in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* remove_key on empty *)
  (* BEG_TEST *)
  let key = "Mario" in
  let original = Ssmap.empty in
  let initmsg = sprintf "REMOVAL PRODUCES WRONG TREE\nRemove Key: %s\nOriginal Tree:\n%s\n" key (Ssmap.tree_string original) in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "" in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Princess" in
  let original = Ssmap.empty in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "" in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)
);

(fun () ->
  (* remove_key on singleton map *)

  (* BEG_TEST *)
  let key = "Princess" in
  let original = singleton in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = " 0: {Mario -> plumber}" in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luigi" in
  let original = singleton in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = " 0: {Mario -> plumber}" in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  (* actually removes only root leaving empty *)
  let key = "Mario" in
  let original = singleton in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "" in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)
);

(fun () ->
  (* remove_key on small map, not present  *)

  (* BEG_TEST *)
  let key = "Wario" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
     2: {Princess -> royalty}
 0: {Mario -> plumber}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Bob-omb" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
     2: {Princess -> royalty}
 0: {Mario -> plumber}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Koopa" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
     2: {Princess -> royalty}
 0: {Mario -> plumber}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "ShyGuy" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
     2: {Princess -> royalty}
 0: {Mario -> plumber}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)
);

(fun () ->
  (* remove_key on small map, 0 or 1 single child *)

  (* BEG_TEST *)
  let key = "Princess" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
 0: {Mario -> plumber}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Luigi" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
     2: {Princess -> royalty}
 0: {Mario -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Toad" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Princess -> royalty}
 0: {Mario -> plumber}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)
);

(fun () ->
  (* remove on smallmap, two-child root  *)
  (* BEG_TEST *)
  let key = "Mario" in
  let original = smallmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
   1: {Toad -> retainer}
 0: {Princess -> royalty}
   1: {Luigi -> plumber}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

);

(fun () ->
  (* remove on bigmap, not present *)

  (* BEG_TEST *)
  let key = "Wario" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Bowser" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Koopa" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Daisy" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

);

(fun () ->
  (* remove on bigmap, 0/1 child *)
  (* BEG_TEST *)
  let key = "Wart" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
     2: {Whale -> 29}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Tryclyde" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Bob-omb" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
       3: {Clawgrip -> 28}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Porcupo" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
               7: {Princess -> 26}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

);

(fun () ->
  (* remove on bigmap, two children *)

  (* BEG_TEST *)
  let key = "Mouser" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
 0: {Ninji -> 22}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Pokey" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
               7: {Princess -> 26}
     2: {Porcupo -> 24}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Toad" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
       3: {Trouter -> 20}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Pokey" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
               7: {Princess -> 26}
     2: {Porcupo -> 24}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
         4: {Clawgrip -> 28}
       3: {Bob-omb -> 18}
     2: {Beezo -> 6}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Beezo" in
  let original = bigmap in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
       3: {Whale -> 29}
     2: {Wart -> 11}
   1: {Tweeter -> 3}
         4: {Tryclyde -> 16}
           5: {Trouter -> 20}
       3: {Toad -> 7}
         4: {Spark -> 13}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
     2: {Pokey -> 4}
         4: {Pidgit -> 8}
       3: {Phanto -> 5}
           5: {Panser -> 27}
         4: {Ostro -> 9}
           5: {Ninji -> 22}
 0: {Mouser -> 1}
       3: {Mario -> 14}
         4: {Luigi -> 25}
     2: {Hoopster -> 12}
         4: {Hawkmouth -> 19}
       3: {Flurry -> 17}
   1: {Cobrat -> 2}
       3: {Clawgrip -> 28}
     2: {Bob-omb -> 18}
       3: {Autobomb -> 10}
         4: {Albatoss -> 15}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

);

(fun () ->
  (* remove on bigmap 2 *)
  (* BEG_TEST *)
  let key = "Bob-omb" in
  let original = bigmap2 in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
         4: {Whale -> 29}
       3: {Wart -> 14}
     2: {Tweeter -> 6}
         4: {Tryclyde -> 17}
           5: {Trouter -> 20}
       3: {Toad -> 10}
   1: {Spark -> 3}
         4: {Snifit -> 21}
           5: {ShyGuy -> 23}
               7: {Princess -> 26}
             6: {Porcupo -> 24}
       3: {Pokey -> 7}
           5: {Pidgit -> 11}
         4: {Phanto -> 8}
             6: {Panser -> 27}
           5: {Ostro -> 12}
             6: {Ninji -> 22}
     2: {Mouser -> 4}
           5: {Mario -> 16}
             6: {Luigi -> 25}
         4: {Hoopster -> 15}
             6: {Hawkmouth -> 19}
           5: {Flurry -> 18}
       3: {Cobrat -> 5}
         4: {Clawgrip -> 28}
 0: {Beezo -> 1}
     2: {Autobomb -> 13}
   1: {Albatoss -> 9}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Flurry" in
  let original = bigmap2 in
  let initmsg = make_remove_msg key original in
  let actual_str = (Ssmap.tree_string (Ssmap.remove_key original key)) in
  let expect_str = "
           5: {Whale -> 29}
         4: {Wart -> 14}
       3: {Tweeter -> 6}
           5: {Tryclyde -> 17}
             6: {Trouter -> 20}
         4: {Toad -> 10}
     2: {Spark -> 3}
           5: {Snifit -> 21}
             6: {ShyGuy -> 23}
                 8: {Princess -> 26}
               7: {Porcupo -> 24}
         4: {Pokey -> 7}
             6: {Pidgit -> 11}
           5: {Phanto -> 8}
               7: {Panser -> 27}
             6: {Ostro -> 12}
               7: {Ninji -> 22}
       3: {Mouser -> 4}
             6: {Mario -> 16}
               7: {Luigi -> 25}
           5: {Hoopster -> 15}
             6: {Hawkmouth -> 19}
         4: {Cobrat -> 5}
           5: {Clawgrip -> 28}
   1: {Bob-omb -> 2}
 0: {Beezo -> 1}
     2: {Autobomb -> 13}
   1: {Albatoss -> 9}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

);

(fun () ->
  (* remove with getopt *)

  (* BEG_TEST *)
  let key = "Princess" in
  let original = smallmap in
  let map = Ssmap.remove_key original key in

  let key = "Princess" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Mario" in
  let actual = Ssmap.getopt map key in
  let expect = Some "plumber" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Luigi" in
  let actual = Ssmap.getopt map key in
  let expect = Some "plumber" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Toad" in
  let actual = Ssmap.getopt map key in
  let expect = Some "retainer" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Wart" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "Mouser" in
  let original = bigmap in
  let map = Ssmap.remove_key original key in

  let key = "Mouser" in
  let actual = Ssmap.getopt map key in
  let expect = None in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Tweeter" in
  let actual = Ssmap.getopt map key in
  let expect = Some "3" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Cobrat" in
  let actual = Ssmap.getopt map key in
  let expect = Some "2" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );

  let key = "Hawkmouth" in
  let actual = Ssmap.getopt map key in
  let expect = Some "19" in
  let msg = make_getopt_msg key map expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

|];;    

open Printf;;
open Mltest;;


(* files for diffing *)
let actual_file = "test-data/actual.tmp";;
let expect_file = "test-data/expect.tmp";;
let diff_file   = "test-data/diff.tmp"  ;;
let msgref = ref "";;

let ident x = x;;

let str_of_opt to_string o =
  match o with
  | None -> "None"
  | Some x -> sprintf "Some %s" (to_string x)
;;

let str_of_boolopt = str_of_opt string_of_bool;;
let str_of_stropt  = str_of_opt ident;;
let str_of_ip (i,j) = sprintf "(%d,%d)" i j;;

let make_getopt_msg key mapstr expect actual =
  let lines = [
      sprintf "Key:    %s" key;
      sprintf "EXPECT: %s" expect;
      sprintf "ACTUAL: %s" actual;
      sprintf "Tree String for Map:\n%s" mapstr;
    ]
  in
  String.concat "\n" lines
;;

let make_iterfold_msg mapstr funcstr expect actual =
  let lines = [
      sprintf "%s" funcstr;
      sprintf "EXPECT: %s" expect;
      sprintf "ACTUAL: %s" actual;
      sprintf "Tree String for Map:\n%s" mapstr;
    ]
  in
  String.concat "\n" lines
;;

let make_remove_msg key original_str =
  let lines = [
      sprintf "REMOVAL PRODUCES WRONG TREE";
      sprintf "- Remove Key: %s" key;
      sprintf "- Original Tree:";
      sprintf "%s\n" original_str;
    ] in
  String.concat "\n" lines
;;

Mltest.main [|
(******************************************)
(* treemap.ml tests *)

(fun () ->
  (* StringStringKV module *)
  let module SSKV = Map_modules.StringStringKV in

  (* BEG_TEST *)
  let (key1,key2) = ("Mario","Luigi") in
  let actual = SSKV.compare_keys key1 key2 in
  let expect = String.compare key1 key2 in
  let msg = sprintf "(compare_keys %s %s) incorrect\nEXPECT: %d\nACTUAL: %d\n" key1 key2 expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

  (* BEG_TEST *)
  let (key1,key2) = ("Bowser","Bob-omb") in
  let actual = SSKV.compare_keys key1 key2 in
  let expect = String.compare key1 key2 in
  let msg = sprintf "(compare_keys %s %s) incorrect\nEXPECT: %d\nACTUAL: %d\n" key1 key2 expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

  (* BEG_TEST *)
  let (key,value) = ("Princess","royalty") in
  let actual = SSKV.keyval_string key value in
  let expect = "{Princess -> royalty}" in
  let msg = sprintf "(keyval_string %s %s) incorrect\nEXPECT: %s\nACTUAL: %s\n" key value expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

  (* BEG_TEST *)
  let (key,value) = ("Toad","retainer") in
  let actual = SSKV.keyval_string key value in
  let expect = "{Toad -> retainer}" in
  let msg = sprintf "(keyval_string %s %s) incorrect\nEXPECT: %s\nACTUAL: %s\n" key value expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

);

(fun () ->
  (* StringStringMap module *)
  let module SSM = Map_modules.StringStringMap in
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
    let addkv map (k,v) = SSM.add map k v in
    List.fold_left addkv SSM.empty entries
  in
  let bigmapstr = SSM.tree_string bigmap in

  (* BEG_TEST *)
  (* StringStringMap getopt  *)
  let key = "ShyGuy" in
  let actual = SSM.getopt bigmap key in
  let expect = Some "23" in
  let msg = make_getopt_msg key bigmapstr (str_of_stropt expect) (str_of_stropt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap getopt  *)
  let key = "Koopa" in
  let actual = SSM.getopt bigmap key in
  let expect = None in
  let msg = make_getopt_msg key bigmapstr (str_of_stropt expect) (str_of_stropt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap contains_key  *)
  let key = "Princess" in
  let actual = SSM.contains_key bigmap key in
  let expect = true in
  let msg = make_getopt_msg key bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap contains_key  *)
  let key = "Wario" in
  let actual = SSM.contains_key bigmap key in
  let expect = false in
  let msg = make_getopt_msg key bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap contains_key *)
  let key = "Wario" in
  let actual = SSM.contains_key bigmap key in
  let expect = false in
  let msg = make_getopt_msg key bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap iter *)
  let str = ref "" in
  let concat_keys k v = str := !str ^ k ^ " " in
  let funcstr = "concatenate all keys in a ref" in
  SSM.iter concat_keys bigmap;
  let expect = "Albatoss Autobomb Beezo Bob-omb Clawgrip Cobrat Flurry Hawkmouth Hoopster Luigi Mario Mouser Ninji Ostro Panser Phanto Pidgit Pokey Porcupo Princess ShyGuy Snifit Spark Toad Trouter Tryclyde Tweeter Wart Whale " in
  let actual = !str in
  let msg = make_iterfold_msg bigmapstr funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap fold *)
  let sum = ref 0 in
  let add_valints k v = sum := !sum + (int_of_string v) in
  let funcstr = "add values as ints in a ref" in
  SSM.iter add_valints bigmap;
  let expect = 435 in
  let actual = !sum in
  let msg = make_iterfold_msg bigmapstr funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap.to_string on empty map *)
  let actual = SSM.to_string SSM.empty in
  let expect = "[]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap.to_string on bigmap *)
  let actual = SSM.to_string bigmap in
  let expect = "[{Albatoss -> 15}, {Autobomb -> 10}, {Beezo -> 6}, {Bob-omb -> 18}, {Clawgrip -> 28}, {Cobrat -> 2}, {Flurry -> 17}, {Hawkmouth -> 19}, {Hoopster -> 12}, {Luigi -> 25}, {Mario -> 14}, {Mouser -> 1}, {Ninji -> 22}, {Ostro -> 9}, {Panser -> 27}, {Phanto -> 5}, {Pidgit -> 8}, {Pokey -> 4}, {Porcupo -> 24}, {Princess -> 26}, {ShyGuy -> 23}, {Snifit -> 21}, {Spark -> 13}, {Toad -> 7}, {Trouter -> 20}, {Tryclyde -> 16}, {Tweeter -> 3}, {Wart -> 11}, {Whale -> 29}]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap.remove_key on bigmap *)
  let key = "Tryclyde" in
  let original = bigmap in
  let initmsg = make_remove_msg key bigmapstr in
  let actual_str = (SSM.tree_string (SSM.remove_key original key)) in
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
);

(fun () ->
  (* IntpairBoolKV module *)
  let module IPBKV = Map_modules.IntpairBoolKV in

  (* BEG_TEST *)
  let (k11,k12) as key1 = (7,2) in
  let (k21,k22) as key2 = (5,8) in
  let actual = IPBKV.compare_keys key1 key2 in
  let expect = 7 - 5 in
  let msg = sprintf "(compare_keys (%d,%d) (%d,%d)) incorrect\nEXPECT: %d\nACTUAL: %d\n" k11 k12 k21 k22 expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

  (* BEG_TEST *)
  let (k11,k12) as key1 = (6,2) in
  let (k21,k22) as key2 = (9,5) in
  let actual = IPBKV.compare_keys key1 key2 in
  let expect = 6 - 9 in
  let msg = sprintf "(compare_keys (%d,%d) (%d,%d)) incorrect\nEXPECT: %d\nACTUAL: %d\n" k11 k12 k21 k22 expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

  (* BEG_TEST *)
  let (k11,k12) as key1 = (3,2) in
  let (k21,k22) as key2 = (3,5) in
  let actual = IPBKV.compare_keys key1 key2 in
  let expect = 2 - 5 in
  let msg = sprintf "(compare_keys (%d,%d) (%d,%d)) incorrect\nEXPECT: %d\nACTUAL: %d\n" k11 k12 k21 k22 expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

  (* BEG_TEST *)
  let (k11,k12) as key1 = (3,8) in
  let (k21,k22) as key2 = (3,5) in
  let actual = IPBKV.compare_keys key1 key2 in
  let expect = 8 - 5 in
  let msg = sprintf "(compare_keys (%d,%d) (%d,%d)) incorrect\nEXPECT: %d\nACTUAL: %d\n" k11 k12 k21 k22 expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)
  
  (* BEG_TEST *)
  let (((k1,k2) as key),value) = ((5,2),true) in
  let actual = IPBKV.keyval_string key value in
  let expect = "{5 > 2 : true}" in
  let msg = sprintf "(keyval_string (%d,%d) %b) incorrect\nEXPECT: %s\nACTUAL: %s\n" k1 k2 value expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)
  
  (* BEG_TEST *)
  let (((k1,k2) as key),value) = ((1,3),false) in
  let actual = IPBKV.keyval_string key value in
  let expect = "{1 > 3 : false}" in
  let msg = sprintf "(keyval_string (%d,%d) %b) incorrect\nEXPECT: %s\nACTUAL: %s\n" k1 k2 value expect actual in
  __check__ ( actual = expect );
  (* END_TEST *)

);

(fun () ->
  (* IntpairBoolMap module *)
  let module IPBM = Map_modules.IntpairBoolMap in
  let bigmap =
    let entries = [
        ( 7, 3),  7 >  3;
        (14,14), 14 > 14;
        (10, 8), 10 >  8;
        ( 3, 1),  3 >  1;
        ( 2, 7),  2 >  7;
        ( 7, 5),  7 >  5;
        (13,11), 13 > 11;
        ( 5,11),  5 > 11;
        ( 4,11),  4 > 11;
        ( 3, 7),  3 >  7;
        ( 5, 6),  5 >  6;
        (11, 1), 11 >  1;
        (13, 0), 13 >  0;
        ( 7, 1),  7 >  1;
        ( 7,11),  7 > 11;
        ( 1, 0),  1 >  0;
        ( 5, 4),  5 >  4;
        ( 9,13),  9 > 13;
        (14, 4), 14 >  4;
        ( 4, 8),  4 >  8;
      ] in
    let addkv map (k,v) = IPBM.add map k v in
    List.fold_left addkv IPBM.empty entries
  in
  let bigmapstr = IPBM.tree_string bigmap in

  (* BEG_TEST *)
  (* IntpairBoolMap tree_string *)
  let actual_str = IPBM.tree_string bigmap in
  let expect_str = "
   1: {14 > 14 : false}
         4: {14 > 4 : true}
       3: {13 > 11 : true}
           5: {13 > 0 : true}
         4: {11 > 1 : true}
     2: {10 > 8 : true}
           5: {9 > 13 : false}
         4: {7 > 11 : false}
       3: {7 > 5 : true}
 0: {7 > 3 : true}
       3: {7 > 1 : true}
     2: {5 > 11 : false}
         4: {5 > 6 : false}
           5: {5 > 4 : true}
       3: {4 > 11 : false}
           5: {4 > 8 : false}
         4: {3 > 7 : false}
   1: {3 > 1 : true}
     2: {2 > 7 : false}
       3: {1 > 0 : true}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file msgref );
  (* END_TEST *)


  (* BEG_TEST *)
  (* IntpairBoolMap getopt  *)
  let (k1,k2) as key = (5,2) in
  let actual = IPBM.getopt bigmap key in
  let expect = None in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap getopt  *)
  let (k1,k2) as key = (7,5) in
  let actual = IPBM.getopt bigmap key in
  let expect = Some true in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap getopt  *)
  let (k1,k2) as key = (5,6) in
  let actual = IPBM.getopt bigmap key in
  let expect = Some false in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap getopt  *)
  let (k1,k2) as key = (3,2) in
  let actual = IPBM.getopt bigmap key in
  let expect = None in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap contains_key  *)
  let (k1,k2) as key = (13,2) in
  let actual = IPBM.contains_key bigmap key in
  let expect = false in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap contains_key  *)
  let (k1,k2) as key = (4,11) in
  let actual = IPBM.contains_key bigmap key in
  let expect = true in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap contains_key  *)
  let (k1,k2) as key = (9,13) in
  let actual = IPBM.contains_key bigmap key in
  let expect = true in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* IntpairBoolMap contains_key  *)
  let (k1,k2) as key = (9,14) in
  let actual = IPBM.contains_key bigmap key in
  let expect = false in
  let msg = make_getopt_msg (str_of_ip key) bigmapstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* IntpairBoolMap module *)
  let module IPBM = Map_modules.IntpairBoolMap in
  let bigmap =
    let entries = [
        ( 7, 3),  7 >  3;
        (14,14), 14 > 14;
        (10, 8), 10 >  8;
        ( 3, 1),  3 >  1;
        ( 2, 7),  2 >  7;
        ( 7, 5),  7 >  5;
        (13,11), 13 > 11;
        ( 5,11),  5 > 11;
        ( 4,11),  4 > 11;
        ( 3, 7),  3 >  7;
        ( 5, 6),  5 >  6;
        (11, 1), 11 >  1;
        (13, 0), 13 >  0;
        ( 7, 1),  7 >  1;
        ( 7,11),  7 > 11;
        ( 1, 0),  1 >  0;
        ( 5, 4),  5 >  4;
        ( 9,13),  9 > 13;
        (14, 4), 14 >  4;
        ( 4, 8),  4 >  8;
      ] in
    let addkv map (k,v) = IPBM.add map k v in
    List.fold_left addkv IPBM.empty entries
  in
  let bigmapstr = IPBM.tree_string bigmap in


  (* BEG_TEST *)
  (* IntpairBoolMap iter *)
  let k1sum = ref 0 in
  let k2sum = ref 0 in
  let sum_keys (k1,k2) v = k1sum := !k1sum+k1; k2sum := !k2sum+k2; in
  let funcstr = "sum all key pairs in a ref" in
  IPBM.iter sum_keys bigmap;
  let expect = 144 in
  let actual = !k1sum in
  let msg = make_iterfold_msg bigmapstr funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  let expect = 126 in
  let actual = !k2sum in
  let msg = make_iterfold_msg bigmapstr funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  (* END_TEST *)


  (* BEG_TEST *)
  (* IntpairBoolMap iter *)
  let concat_tf cur k v = cur ^ (string_of_bool v) ^ " " in
  let funcstr = "concatenate all true/false values as strings" in
  let actual = IPBM.fold concat_tf "" bigmap in
  let expect = "true false true false false false true false false true true true false false true true true true true false " in
  let msg = make_iterfold_msg bigmapstr funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* to_string on empty map *)
  let actual = IPBM.to_string IPBM.empty in
  let expect = "[]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* to_string on bigmap *)
  let actual = IPBM.to_string bigmap in
  let expect = "[{1 > 0 : true}, {2 > 7 : false}, {3 > 1 : true}, {3 > 7 : false}, {4 > 8 : false}, {4 > 11 : false}, {5 > 4 : true}, {5 > 6 : false}, {5 > 11 : false}, {7 > 1 : true}, {7 > 3 : true}, {7 > 5 : true}, {7 > 11 : false}, {9 > 13 : false}, {10 > 8 : true}, {11 > 1 : true}, {13 > 0 : true}, {13 > 11 : true}, {14 > 4 : true}, {14 > 14 : false}]" in
  let msg = sprintf "EXPECT to_string: %s\nACTUAL to_string: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  (* StringStringMap.remove_key on bigmap *)
  let (k1,k2) as key = (3,1) in
  let original = bigmap in
  let initmsg = make_remove_msg (sprintf "(%d,%d)" k1 k2) bigmapstr in
  let actual_str = (IPBM.tree_string (IPBM.remove_key original key)) in
  let expect_str = "
   1: {14 > 14 : false}
         4: {14 > 4 : true}
       3: {13 > 11 : true}
           5: {13 > 0 : true}
         4: {11 > 1 : true}
     2: {10 > 8 : true}
           5: {9 > 13 : false}
         4: {7 > 11 : false}
       3: {7 > 5 : true}
 0: {7 > 3 : true}
       3: {7 > 1 : true}
     2: {5 > 11 : false}
         4: {5 > 6 : false}
           5: {5 > 4 : true}
       3: {4 > 11 : false}
         4: {4 > 8 : false}
   1: {3 > 7 : false}
     2: {2 > 7 : false}
       3: {1 > 0 : true}
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)
);




|];;    

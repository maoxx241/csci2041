open Printf;;
open Mltest;;


(* files for diffing *)
let actual_file = "test-data/actual.tmp";;
let expect_file = "test-data/expect.tmp";;
let diff_file   = "test-data/diff.tmp"  ;;
let msgref = ref "";;

let str_of_opt to_string o =
  match o with
  | None -> "None"
  | Some x -> sprintf "Some %s" (to_string x)
;;

let str_of_boolopt = str_of_opt string_of_bool;;

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
  (* basic functionality, empty + add + tree_string *)
  let module IBKV = struct
      type key_t = int;;
      type value_t = bool;;
      let compare_keys = (-);;
      let keyval_string i b = sprintf "(%d, %b)" i b;;
    end
  in
  let module IBMap = Treemap.Make(IBKV) in

  (* BEG_TEST *)
  let map = IBMap.empty in
  let map = IBMap.add map 5 false in
  let actual_str = IBMap.tree_string map in
  let expect_str = "
 0: (5, false)
" 
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file msgref );
  (* END_TEST *)
);

(fun () ->
  (* basic functionality, empty + add + tree_string *)
  let module IBKV = struct
      type key_t = int;;
      type value_t = bool;;
      let compare_keys = (-);;
      let keyval_string i b = sprintf "(%d, %b)" i b;;
    end
  in
  let module IBMap = Treemap.Make(IBKV) in
  let ibmap =
    let kvs = [5,false; 8,true; 12,true; 9,false; 2,true; 4,true; 7,false; 1,false] in
    let addkv map (k,v) = IBMap.add map k v in
    List.fold_left addkv IBMap.empty kvs
  in

  (* BEG_TEST *)
  let key = 9 in
  let actual = IBMap.getopt ibmap key in
  let expect = Some false in
  let msg = make_getopt_msg (string_of_int key) (IBMap.tree_string ibmap) (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = 4 in
  let actual = IBMap.getopt ibmap key in
  let expect = Some true in
  let msg = make_getopt_msg (string_of_int key) (IBMap.tree_string ibmap) (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = 3 in
  let actual = IBMap.getopt ibmap key in
  let expect = None in
  let msg = make_getopt_msg (string_of_int key) (IBMap.tree_string ibmap) (str_of_boolopt expect) (str_of_boolopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = 8 in
  let actual = IBMap.contains_key ibmap 12 in
  let expect = true in
  let msg = make_getopt_msg (string_of_int key) (IBMap.tree_string ibmap) (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = 3 in
  let actual = IBMap.contains_key ibmap 12 in
  let expect = true in
  let msg = make_getopt_msg (string_of_int key) (IBMap.tree_string ibmap) (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

);

(fun () ->
  (* iter *)
  let module ISKV = struct
      type key_t = int;;
      type value_t = string;;
      let compare_keys = (-);;
      let keyval_string i s = sprintf "(%d, %s)" i s;;
    end
  in
  let module ISMap = Treemap.Make(ISKV) in
  let ismap =
    let kvs = [5,"odd"; 8,"even"; 12,"even"; 9,"odd"; 2,"even"; 4,"even"; 7,"odd"; 1,"odd"] in
    let addkv map (k,v) = ISMap.add map k v in
    List.fold_left addkv ISMap.empty kvs
  in

  (* BEG_TEST *)
  let str = ref "" in
  let concat_values k v = str := !str ^ v ^ " " in
  let funcstr = "concatenate all value strings in a ref" in
  ISMap.iter concat_values ismap;
  let expect = "odd even even odd odd even odd even " in
  let actual = !str in
  let msg = make_iterfold_msg (ISMap.tree_string ismap) funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* fold *)
  let module ISKV = struct
      type key_t = int;;
      type value_t = string;;
      let compare_keys = (-);;
      let keyval_string i s = sprintf "(%d, %s)" i s;;
    end
  in
  let module ISMap = Treemap.Make(ISKV) in
  let ismap =
    let kvs = [5,"odd"; 8,"even"; 12,"even"; 9,"odd"; 2,"even"; 4,"even"; 7,"odd"; 1,"odd"] in
    let addkv map (k,v) = ISMap.add map k v in
    List.fold_left addkv ISMap.empty kvs
  in

  (* BEG_TEST *)
  let accum_kv (ksum,vcat) k v = (ksum+k, (vcat^v^" ")) in
  let funcstr = "sums keys, concatenates value strings" in
  let (ksum,vcat) = ISMap.fold accum_kv (0,"") ismap in
  let mapstr = ISMap.tree_string ismap in
  let actual = ksum in
  let expect = 48 in
  let msg = make_iterfold_msg mapstr funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  let actual = vcat in
  let expect = "odd even even odd odd even odd even " in
  let msg = make_iterfold_msg mapstr funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* to_string *)
  let module ISKV = struct
      type key_t = int;;
      type value_t = string;;
      let compare_keys = (-);;
      let keyval_string i s = sprintf "(%d, %s)" i s;;
    end
  in
  let module ISMap = Treemap.Make(ISKV) in
  let ismap =
    let kvs = [5,"odd"; 8,"even"; 12,"even"; 9,"odd"; 2,"even"; 4,"even"; 7,"odd"; 1,"odd"] in
    let addkv map (k,v) = ISMap.add map k v in
    List.fold_left addkv ISMap.empty kvs
  in

  (* BEG_TEST *)
  let actual = ISMap.to_string ISMap.empty in
  let expect = "[]" in
  let msg = sprintf "to_string incorrect\nEXPECT: %s\nACTUAL: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let actual = ISMap.to_string ismap in
  let expect = "[(1, odd), (2, even), (4, even), (5, odd), (7, odd), (8, even), (9, odd), (12, even)]" in
  let msg = sprintf "to_string incorrect\nEXPECT: %s\nACTUAL: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* remove_key *)
  let module SIKV = struct
      type key_t = string;;
      type value_t = int;;
      let compare_keys = String.compare;;
      let keyval_string s i = sprintf "%s is %d" s i;;
    end
  in
  let module SIMap = Treemap.Make(SIKV) in
  let simap =
    let kvs = ["seven",7; "two",2; "five",5; "one",1; "nine",9; "twelve",12; "eight",8] in
    let addkv map (k,v) = SIMap.add map k v in
    List.fold_left addkv SIMap.empty kvs
  in
  let simap_str = SIMap.tree_string simap in

  (* BEG_TEST *)
  let key = "ten" in
  let initmsg = make_remove_msg key simap_str in
  let actual_str = (SIMap.tree_string (SIMap.remove_key simap key)) in
  let expect_str = "
   1: two is 2
     2: twelve is 12
 0: seven is 7
     2: one is 1
       3: nine is 9
   1: five is 5
     2: eight is 8
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "one" in
  let initmsg = make_remove_msg key simap_str in
  let actual_str = (SIMap.tree_string (SIMap.remove_key simap key)) in
  let expect_str = "
   1: two is 2
     2: twelve is 12
 0: seven is 7
     2: nine is 9
   1: five is 5
     2: eight is 8
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "seven" in
  let initmsg = make_remove_msg key simap_str in
  let actual_str = (SIMap.tree_string (SIMap.remove_key simap key)) in
  let expect_str = "
   1: two is 2
 0: twelve is 12
     2: one is 1
       3: nine is 9
   1: five is 5
     2: eight is 8
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)

  (* BEG_TEST *)
  let key = "twelve" in
  let initmsg = make_remove_msg key simap_str in
  let actual_str = (SIMap.tree_string (SIMap.remove_key simap key)) in
  let expect_str = "
   1: two is 2
 0: seven is 7
     2: one is 1
       3: nine is 9
   1: five is 5
     2: eight is 8
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)


  (* BEG_TEST *)
  let key = "five" in
  let initmsg = make_remove_msg key simap_str in
  let actual_str = (SIMap.tree_string (SIMap.remove_key simap key)) in
  let expect_str = "
   1: two is 2
     2: twelve is 12
 0: seven is 7
     2: one is 1
   1: nine is 9
     2: eight is 8
"
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file ~initmsg msgref );
  (* END_TEST *)
);

|];;    

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

let str_of_fopt = str_of_opt string_of_float;;

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
(* treeset.ml tests *)

(fun () ->
  (* basic functionality, empty + add + tree_string *)
  let module FloatType = struct
      type element = float;;
      let compare = Pervasives.compare;;
      let elem_string f = sprintf "%.4f" f;;
    end
  in
  let module FS = Treeset.Make(FloatType) in

  (* BEG_TEST *)
  let set = FS.empty in
  let set = FS.add set 5.5 in
  let actual_str = FS.tree_string set in
  let expect_str = "
 0: 5.5000
" 
  in
  __check_output__ ( check_diff  ~expect_str expect_file ~actual_str actual_file diff_file msgref );
  (* END_TEST *)
);

(fun () ->
  (* basic functionality, empty + add + tree_string *)
  let module FloatType = struct
      type element = float;;
      let compare = Pervasives.compare;;
      let elem_string = string_of_float;;
    end
  in
  let module FS = Treeset.Make(FloatType) in
  let fset =
    let elems = [5.5; 8.2; 12.6; 9.4; 2.1; 4.5; 7.6; 1.9; 3.3; 3.6; 3.9; 3.1; 4.98] in
    List.fold_left FS.add FS.empty elems
  in
  let fsetstr = FS.tree_string fset in

  (* BEG_TEST *)
  let el = 2.1 in
  let actual = FS.getopt fset el in
  let expect = Some 2.1 in
  let msg = make_getopt_msg (string_of_float el) fsetstr (str_of_fopt expect) (str_of_fopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let el = 6.66 in
  let actual = FS.getopt fset el in
  let expect = None in
  let msg = make_getopt_msg (string_of_float el) fsetstr (str_of_fopt expect) (str_of_fopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

);

(fun () ->
  (* basic functionality, empty + add + tree_string *)
  let module FloatType = struct
      type element = float;;
      let compare = Pervasives.compare;;
      let elem_string = string_of_float;;
    end
  in
  let module FS = Treeset.Make(FloatType) in
  let fset =
    let elems = [5.5; 8.2; 12.6; 9.4; 2.1; 4.5; 7.6; 1.9; 3.3; 3.6; 3.9; 3.1; 4.98] in
    List.fold_left FS.add FS.empty elems
  in
  let fsetstr = FS.tree_string fset in

  (* BEG_TEST *)
  let el = 4.98 in
  let actual = FS.getopt fset el in
  let expect = Some 4.98 in
  let msg = make_getopt_msg (string_of_float el) fsetstr (str_of_fopt expect) (str_of_fopt actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let el = 8.2 in
  let actual = FS.contains fset el in
  let expect = true in
  let msg = make_getopt_msg (string_of_float el) fsetstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let el = 7.64 in
  let actual = FS.contains fset el in
  let expect = false in
  let msg = make_getopt_msg (string_of_float el) fsetstr (string_of_bool expect) (string_of_bool actual) in
  __check__ ( expect = actual );
  (* END_TEST *)

);

(fun () ->
  (* iter *)
  let module ISType = struct
      type element = int * string;;
      let compare = Pervasives.compare;;
      let elem_string (i,s) = sprintf "(%d, %s)" i s;;
    end
  in
  let module ISSet = Treeset.Make(ISType) in
  let isset =
    let els = [5,"odd"; 8,"even"; 12,"even"; 9,"odd"; 2,"even"; 4,"even"; 7,"odd"; 1,"odd"] in
    List.fold_left ISSet.add ISSet.empty els
  in
  let issetstr = ISSet.tree_string isset in

  (* BEG_TEST *)
  let str = ref "" in
  let concat_els (i,s) = str := !str ^ (sprintf "%d is %s " i s) in
  let funcstr = "concatenate all int/strin pairs in a ref" in
  ISSet.iter concat_els isset;
  let expect = "1 is odd 2 is even 4 is even 5 is odd 7 is odd 8 is even 9 is odd 12 is even " in
  let actual = !str in
  let msg = make_iterfold_msg issetstr funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* fold *)
  let module ISType = struct
      type element = int * string;;
      let compare = Pervasives.compare;;
      let elem_string (i,s) = sprintf "(%d, %s)" i s;;
    end
  in
  let module ISSet = Treeset.Make(ISType) in
  let isset =
    let els = [5,"odd"; 8,"even"; 12,"even"; 9,"odd"; 2,"even"; 4,"even"; 7,"odd"; 1,"odd"] in
    List.fold_left ISSet.add ISSet.empty els
  in
  let issetstr = ISSet.tree_string isset in

  (* BEG_TEST *)
  let accum_kv (ksum,vcat) (k,v) = (ksum+k, (vcat^v^" ")) in
  let funcstr = "sums keys, concatenates value strings" in
  let (ksum,vcat) = ISSet.fold accum_kv (0,"") isset in
  let actual = ksum in
  let expect = 48 in
  let msg = make_iterfold_msg issetstr funcstr (string_of_int expect) (string_of_int actual) in
  __check__ ( expect = actual );
  let actual = vcat in
  let expect = "odd even even odd odd even odd even " in
  let msg = make_iterfold_msg issetstr funcstr expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)
);

(fun () ->
  (* to_string *)
  let module ISType = struct
      type element = int * string;;
      let compare = Pervasives.compare;;
      let elem_string (i,s) = sprintf "(%d, %s)" i s;;
    end
  in
  let module ISSet = Treeset.Make(ISType) in
  let isset =
    let els = [5,"odd"; 8,"even"; 12,"even"; 9,"odd"; 2,"even"; 4,"even"; 7,"odd"; 1,"odd"] in
    List.fold_left ISSet.add ISSet.empty els
  in

  (* BEG_TEST *)
  let actual = ISSet.to_string ISSet.empty in
  let expect = "[]" in
  let msg = sprintf "to_string incorrect\nEXPECT: %s\nACTUAL: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

  (* BEG_TEST *)
  let actual = ISSet.to_string isset in
  let expect = "[(1, odd), (2, even), (4, even), (5, odd), (7, odd), (8, even), (9, odd), (12, even)]" in
  let msg = sprintf "to_string incorrect\nEXPECT: %s\nACTUAL: %s\n" expect actual in
  __check__ ( expect = actual );
  (* END_TEST *)

);

(fun () ->
  (* remove *)
  let module SIType = struct
      type element = string * int;;
      let compare = Pervasives.compare;;
      let elem_string (s,i) = sprintf "%s is %d" s i;;
    end
  in
  let module SISet = Treeset.Make(SIType) in
  let siset =
    let els = ["seven",7; "two",2; "five",5; "one",1; "nine",9; "twelve",12; "eight",8] in
    List.fold_left SISet.add SISet.empty els
  in
  let siset_str = SISet.tree_string siset in

  (* BEG_TEST *)
  let (s,i) as el = ("ten",10) in
  let initmsg = make_remove_msg (sprintf "(%s,%d)" s i) siset_str in
  let actual_str = (SISet.tree_string (SISet.remove siset el)) in
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
  let (s,i) as el = ("one",1) in
  let initmsg = make_remove_msg (sprintf "(%s,%d)" s i) siset_str in
  let actual_str = (SISet.tree_string (SISet.remove siset el)) in
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
);

(fun () ->
  (* remove *)
  let module SIType = struct
      type element = string * int;;
      let compare = Pervasives.compare;;
      let elem_string (s,i) = sprintf "%s is %d" s i;;
    end
  in
  let module SISet = Treeset.Make(SIType) in
  let siset =
    let els = ["seven",7; "two",2; "five",5; "one",1; "nine",9; "twelve",12; "eight",8] in
    List.fold_left SISet.add SISet.empty els
  in
  let siset_str = SISet.tree_string siset in

  (* BEG_TEST *)
  let (s,i) as el = ("seven",7) in
  let initmsg = make_remove_msg (sprintf "(%s,%d)" s i) siset_str in
  let actual_str = (SISet.tree_string (SISet.remove siset el)) in
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
);

(fun () ->
  (* remove *)
  let module SIType = struct
      type element = string * int;;
      let compare = Pervasives.compare;;
      let elem_string (s,i) = sprintf "%s is %d" s i;;
    end
  in
  let module SISet = Treeset.Make(SIType) in
  let siset =
    let els = ["seven",7; "two",2; "five",5; "one",1; "nine",9; "twelve",12; "eight",8] in
    List.fold_left SISet.add SISet.empty els
  in
  let siset_str = SISet.tree_string siset in

  (* BEG_TEST *)
  let (s,i) as el = ("twelve",12) in
  let initmsg = make_remove_msg (sprintf "(%s,%d)" s i) siset_str in
  let actual_str = (SISet.tree_string (SISet.remove siset el)) in
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

);

(fun () ->
  (* remove *)
  let module SIType = struct
      type element = string * int;;
      let compare = Pervasives.compare;;
      let elem_string (s,i) = sprintf "%s is %d" s i;;
    end
  in
  let module SISet = Treeset.Make(SIType) in
  let siset =
    let els = ["seven",7; "two",2; "five",5; "one",1; "nine",9; "twelve",12; "eight",8] in
    List.fold_left SISet.add SISet.empty els
  in
  let siset_str = SISet.tree_string siset in

  (* BEG_TEST *)
  let (s,i) as el = ("eighteen",18) in
  let initmsg = make_remove_msg (sprintf "(%s,%d)" s i) siset_str in
  let actual_str = (SISet.tree_string (SISet.remove siset el)) in
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
);
|];;    

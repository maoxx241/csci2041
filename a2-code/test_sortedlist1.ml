open Sortedlist;;               (* insert / remove / print / merge *)
open Printf;;
open Mltest;;

(* files for diffing *)
let actual_file = "test-data/actual.tmp";;
let expect_file = "test-data/expect.tmp";;
let diff_file   = "test-data/diff.tmp"  ;;
let msgref = ref "";;

Mltest.main [|
(******************************************)
(* sortedlist insert tests *)

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.insert base cases *)
  let list = [] in
  let elem = 1 in
  let actual = insert list elem in
  let expect = [1] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert base cases *)
  let list = [] in
  let elem = "a" in
  let actual = insert list elem in
  let expect = ["a"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.insert short lists *)
  let list = [3] in
  let elem = 1 in
  let actual = insert list elem in
  let expect = [1;3] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert short lists *)
  let list = [3] in
  let elem = 5 in
  let actual = insert list elem in
  let expect = [3;5] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert short lists *)
  let list = ["c"] in
  let elem = "a" in
  let actual = insert list elem in
  let expect = ["a";"c"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert short lists *)
  let list = ["c"] in
  let elem = "e" in
  let actual = insert list elem in
  let expect = ["c";"e"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);


(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.insert med lists *)
  let list = [1; 3; 5; 7] in
  let elem = 0 in
  let actual = insert list elem in
  let expect = [0; 1; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert med lists *)
  let list = [1; 3; 5; 7] in
  let elem = 2 in
  let actual = insert list elem in
  let expect = [1; 2; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert med lists *)
  let list = [1; 3; 5; 7] in
  let elem = 4 in
  let actual = insert list elem in
  let expect = [1; 3; 4; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert med lists *)
  let list = [1; 3; 5; 7] in
  let elem = 6 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 6; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert med lists *)
  let list = [1; 3; 5; 7] in
  let elem = 8 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7; 8] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.insert no duplicates *)
  let list = [1; 3; 5; 7] in
  let elem = 1 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert no duplicates *)
  let list = [1; 3; 5; 7] in
  let elem = 3 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert no duplicates *)
  let list = [1; 3; 5; 7] in
  let elem = 5 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert no duplicates *)
  let list = [1; 3; 5; 7] in
  let elem = 5 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert no duplicates *)
  let list = [1; 3; 5; 7] in
  let elem = 7 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.insert long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 4 in
  let actual = insert list elem in
  let expect = [1; 3; 4; 5; 7; 9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 50 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 50; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 100 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; 100] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert long lists, no duplicates *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 11 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.insert long lists, no duplicates *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 99 in
  let actual = insert list elem in
  let expect = [1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(******************************************)
(* sortedlist insert tests *)

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.remove base cases *)
  let list = [] in
  let elem = 1 in
  let actual = remove list elem in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove base cases *)
  let list = [] in
  let elem = "a" in
  let actual = remove list elem in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);


(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.remove short lists *)
  let list = [1] in
  let elem = 1 in
  let actual = remove list elem in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove short lists *)
  let list = [1] in
  let elem = 3 in
  let actual = remove list elem in
  let expect = [1] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove short lists *)
  let list = [1] in
  let elem = 0 in
  let actual = remove list elem in
  let expect = [1] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove short lists *)
  let list = ["a"] in
  let elem = "a" in
  let actual = remove list elem in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove short lists *)
  let list = ["c"] in
  let elem = "a" in
  let actual = remove list elem in
  let expect = ["c"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove short lists *)
  let list = ["c"] in
  let elem = "e" in
  let actual = remove list elem in
  let expect = ["c"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.remove med lists *)
  let list = [1;3;5;7] in
  let elem = 1 in
  let actual = remove list elem in
  let expect = [3;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove med lists *)
  let list = [1;3;5;7] in
  let elem = 3 in
  let actual = remove list elem in
  let expect = [1;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove med lists *)
  let list = [1;3;5;7] in
  let elem = 5 in
  let actual = remove list elem in
  let expect = [1;3;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove med lists *)
  let list = [1;3;5;7] in
  let elem = 7 in
  let actual = remove list elem in
  let expect = [1;3;5] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.remove no element equal *)
  let list = [1;3;5;7] in
  let elem = 0 in
  let actual = remove list elem in
  let expect = [1;3;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove no element equal *)
  let list = [1;3;5;7] in
  let elem = 2 in
  let actual = remove list elem in
  let expect = [1;3;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove no element equal *)
  let list = [1;3;5;7] in
  let elem = 4 in
  let actual = remove list elem in
  let expect = [1;3;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove no element equal *)
  let list = [1;3;5;7] in
  let elem = 6 in
  let actual = remove list elem in
  let expect = [1;3;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove no element equal *)
  let list = [1;3;5;7] in
  let elem = 8 in
  let actual = remove list elem in
  let expect = [1;3;5;7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.remove long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 33 in
  let actual = remove list elem in
  let expect = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ]in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 67 in
  let actual = remove list elem in
  let expect = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ]in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 22 in
  let actual = remove list elem in
  let expect = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ]in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 99 in
  let actual = remove list elem in
  let expect = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; ] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* Sortedlist.remove long lists *)
  let list = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99; ] in
  let elem = 150 in
  let actual = remove list elem in
  let expect = [ 1;  3;  5;  7;  9; 11; 13; 15; 17; 19; 21; 23; 25; 27; 29; 31; 33; 35; 37; 39; 41; 43; 45; 47; 49; 51; 53; 55; 57; 59; 61; 63; 65; 67; 69; 71; 73; 75; 77; 79; 81; 83; 85; 87; 89; 91; 93; 95; 97; 99;] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(* sortedist print tests *)
(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.print short lists *)
  let list = [ "a";  "b";  "c";] in
  let expect_string = "\
a
b
c
" in
  let thunk = fun () -> print list in
  save_stdout actual_file thunk;
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.remove long lists *)
  let list = [ "apple";  "banana";  "orange"; "strawberry" ;] in
  let expect_string = "\
apple
banana
orange
strawberry
" in
  let thunk = fun () -> print list in
  save_stdout actual_file thunk;
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);


(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.print single element *)
  let list = [ "Banana"] in
  let expect_string = "\
Banana
" in
  let thunk = fun () -> print list in
  save_stdout actual_file thunk;
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.print empty list *)
  let list = [] in
  let expect_string = "\
" in
  let thunk = fun () -> print list in
  save_stdout actual_file thunk;
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.print medium lists *)
  let list = [ "practice";"clear";"child";"period";"yard";"yeah";"problem";"your";"cause";"near";"operation";"heart";"difference";"wall";"I";"hard";"energy";"up";"story";"art";"natural";"usually";"debate";"program";"mind";] in
  let expect_string = "\
practice
clear
child
period
yard
yeah
problem
your
cause
near
operation
heart
difference
wall
I
hard
energy
up
story
art
natural
usually
debate
program
mind
" in
  let thunk = fun () -> print list in
  save_stdout actual_file thunk;
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.print medium lists *)
  let list = [ "generation";"state";"carry";"real";"expert";"behavior";"second";"return";"subject";"result";"manager";"president";"painting";"career";"goal";"safe";"artist";"number";"public";"you";"baby";"picture";"center";"player";"culture";"fail";"agree";"space";"rock";"door";"here";"decade";"worker";"draw";"give";"reason";"between";"believe";"brother";"past";"speech";"break";"organization";"yes";"black";"war";"cover";"phone";"feel";"they";"floor";"physical";"cost";"might";"house";"else";"value";"coach";"woman";"take";"play";"set";"go";"court";"some";"place";"performance";"summer";"themselves";"death";"owner";"customer";"senior";"attention";"remember";"exactly";"financial";"weapon";"race";"huge";"road";"collection";"throughout";"season";"act";"day";"could";"same";"sea";"third";"dark";"usually";"become";"evening";"probably";"mention";"trip";"seat";"art";"question";] in
  let expect_string = "\
generation
state
carry
real
expert
behavior
second
return
subject
result
manager
president
painting
career
goal
safe
artist
number
public
you
baby
picture
center
player
culture
fail
agree
space
rock
door
here
decade
worker
draw
give
reason
between
believe
brother
past
speech
break
organization
yes
black
war
cover
phone
feel
they
floor
physical
cost
might
house
else
value
coach
woman
take
play
set
go
court
some
place
performance
summer
themselves
death
owner
customer
senior
attention
remember
exactly
financial
weapon
race
huge
road
collection
throughout
season
act
day
could
same
sea
third
dark
usually
become
evening
probably
mention
trip
seat
art
question
" in
  let thunk = fun () -> print list in
  save_stdout actual_file thunk;
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);

|];;    
    

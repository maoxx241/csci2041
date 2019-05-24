open Sumfuncs;;                           (* array_sum and list_sum *)
open Printf;;
open Mltest;;

Mltest.main [|
(******************************************)
(* array_sum tests *)
(fun () ->
  (* BEG_TEST *)
  (* array_sum on short array *)
  let input = [|1; 2; 3;|] in
  let actual = array_sum input in
  let expect = 6 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on short array *)
  let input = [|8; 6; 7; 5; 7; 0; 9;|] in
  let actual = array_sum input in
  let expect = 42 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on short array *)
  let input = [|-2; 1; 9; -17; 6;|] in
  let actual = array_sum input in
  let expect = -3 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on single element array *)
  let input = [|13|] in
  let actual = array_sum input in
  let expect = 13 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on empty array *)
  let input = [||] in
  let actual = array_sum input in
  let expect = 0 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

(******************************************)
(* list_sum tests *)
(fun () ->
  (* BEG_TEST *)
  (* list_sum on short list *)
  let input = [1; 2; 3;] in
  let actual = list_sum input in
  let expect = 6 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on short list *)
  let input = [8; 6; 7; 5; 7; 0; 9;] in
  let actual = list_sum input in
  let expect = 42 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on short list *)
  let input = [-2; 1; 9; -17; 6;] in
  let actual = list_sum input in
  let expect = -3 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on single element list *)
  let input = [13] in
  let actual = list_sum input in
  let expect = 13 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on empty list *)
  let input = [] in
  let actual = list_sum input in
  let expect = 0 in
  let msg = sprintf "Expect: %d\nActual: %d" expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

|];;    
    

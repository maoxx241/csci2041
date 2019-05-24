open Revfuncs;;                           (* array_rev and list_rev *)
open Printf;;
open Mltest;;

Mltest.main [|
(******************************************)
(* array_rev tests *)
(fun () ->
  (* BEG_TEST *)
  (* array_sum on short odd length arrays *)
  let actual = [|1; 2; 3;|] in
  array_rev actual;
  let expect = [|3; 2; 1;|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_sum on short odd length arrays *)
  let actual = [|"a"; "b"; "c"; "d"; "e"|] in
  array_rev actual;
  let expect = [|"e"; "d"; "c"; "b"; "a"|] in
  let msg = sprintf "Expect: %s\nActual: %s" (strarr2str expect) (strarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on short even length arrays *)
  let actual = [|1; 2; 3; 4|] in
  array_rev actual;
  let expect = [|4; 3; 2; 1;|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_sum on short even length arrays *)
  let actual = [|"a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"|] in
  array_rev actual;
  let expect = [|"j"; "i"; "h"; "g"; "f"; "e"; "d"; "c"; "b"; "a"|] in
  let msg = sprintf "Expect: %s\nActual: %s" (strarr2str expect) (strarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on empty array *)
  let actual = [||] in
  array_rev actual;
  let expect = [||] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on length 1 array *)
  let actual = [|false|] in
  array_rev actual;
  let expect = [|false|] in
  let msg = sprintf "Expect: %s\nActual: %s" (boolarr2str expect) (boolarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_sum on long array *)
  let actual = [|30; 62; 24; 40; 50; 63; 35; 96; 68; 41; 5; 10; 43; 23; 70; 45; 9; 17; 7; 100; 85; 6; 72; 59; 71; 36; 60; 42; 76; 99; 97; 58; 74; 65; 75; 18; 88; 49; 90; 48; 54; 38; 84; 46; 86; 95; 98; 79; 32; 20; 22; 78; 69; 44; 83; 28; 25; 37; 64; 47; 31; 73; 81; 11; 77; 92; 29; 67; 56; 12; 51; 89; 14; 34; 93; 8; 66; 13; 53; 27; 39; 52; 82; 4; 16; 80; 94; 2; 87; 91; 55; 19; 33; 21; 15; 1; 61; 57; 3; 26;|] in
  array_rev actual;
  let expect = [|26; 3; 57; 61; 1; 15; 21; 33; 19; 55; 91; 87; 2; 94; 80; 16; 4; 82; 52; 39; 27; 53; 13; 66; 8; 93; 34; 14; 89; 51; 12; 56; 67; 29; 92; 77; 11; 81; 73; 31; 47; 64; 37; 25; 28; 83; 44; 69; 78; 22; 20; 32; 79; 98; 95; 86; 46; 84; 38; 54; 48; 90; 49; 88; 18; 75; 65; 74; 58; 97; 99; 76; 42; 60; 36; 71; 59; 72; 6; 85; 100; 7; 17; 9; 45; 70; 23; 43; 10; 5; 41; 68; 96; 35; 63; 50; 40; 24; 62; 30|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

(******************************************)
(* list_rev tests *)
(fun () ->
  (* BEG_TEST *)
  (* list_sum on short odd length lists *)
  let actual = list_rev [1; 2; 3;] in
  let expect = [3; 2; 1;] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_sum on short odd length lists *)
  let actual = list_rev ["a"; "b"; "c"; "d"; "e"] in
  let expect = ["e"; "d"; "c"; "b"; "a"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on short even length lists *)
  let actual = list_rev [1; 2; 3; 4] in
  let expect = [4; 3; 2; 1;] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_sum on short even length lists *)
  let actual = list_rev ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] in
  let expect = ["j"; "i"; "h"; "g"; "f"; "e"; "d"; "c"; "b"; "a"] in
  let msg = sprintf "Expect: %s\nActual: %s" (strlist2str expect) (strlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on empty list *)
  let actual = list_rev [] in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on length 1 list *)
  let actual = list_rev [false] in
  let expect = [false] in
  let msg = sprintf "Expect: %s\nActual: %s" (boollist2str expect) (boollist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_sum on long list *)
  let actual = list_rev [30; 62; 24; 40; 50; 63; 35; 96; 68; 41; 5; 10; 43; 23; 70; 45; 9; 17; 7; 100; 85; 6; 72; 59; 71; 36; 60; 42; 76; 99; 97; 58; 74; 65; 75; 18; 88; 49; 90; 48; 54; 38; 84; 46; 86; 95; 98; 79; 32; 20; 22; 78; 69; 44; 83; 28; 25; 37; 64; 47; 31; 73; 81; 11; 77; 92; 29; 67; 56; 12; 51; 89; 14; 34; 93; 8; 66; 13; 53; 27; 39; 52; 82; 4; 16; 80; 94; 2; 87; 91; 55; 19; 33; 21; 15; 1; 61; 57; 3; 26;] in
  let expect = [26; 3; 57; 61; 1; 15; 21; 33; 19; 55; 91; 87; 2; 94; 80; 16; 4; 82; 52; 39; 27; 53; 13; 66; 8; 93; 34; 14; 89; 51; 12; 56; 67; 29; 92; 77; 11; 81; 73; 31; 47; 64; 37; 25; 28; 83; 44; 69; 78; 22; 20; 32; 79; 98; 95; 86; 46; 84; 38; 54; 48; 90; 49; 88; 18; 75; 65; 74; 58; 97; 99; 76; 42; 60; 36; 71; 59; 72; 6; 85; 100; 7; 17; 9; 45; 70; 23; 43; 10; 5; 41; 68; 96; 35; 63; 50; 40; 24; 62; 30] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);

|];;    

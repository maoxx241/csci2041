open Abovefuncs;;               (* array_above and list_above *)
open Printf;;
open Mltest;;

Mltest.main [|
(******************************************)
(* array_above tests *)
(fun () ->
  (* BEG_TEST *)
  (* array_above on short int array *)
  let thresh = 0 in
  let input = [|0; 1; 2; 0|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|1; 2|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (intarr2str original) (intarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_above on short int array *)
  let thresh = 0 in
  let input = [|4; -2; -1; 7; 0; 3|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|4; 7; 3|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (intarr2str original) (intarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_above on short array *)
  let thresh = 3 in
  let input = [|4; -2; -1; 7; 0; 3|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|4; 7|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (intarr2str original) (intarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_above on short float array *)
  let thresh = 1.5 in
  let input = [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|4.2; 7.6; 8.9; 8.5|] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatarr2str expect) (floatarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (floatarr2str original) (floatarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_above on short array *)
  let thresh = 0.0 in
  let input = [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatarr2str expect) (floatarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (floatarr2str original) (floatarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_above on short float array *)
  let thresh = 9.0 in
  let input = [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [||] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatarr2str expect) (floatarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (floatarr2str original) (floatarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_above on short bool array *)
  let thresh = false in
  let input = [|false; true; false; true; true;|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|true; true; true|] in
  let msg = sprintf "Expect: %s\nActual: %s" (boolarr2str expect) (boolarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (boolarr2str original) (boolarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* array_above on empty array *)
  let thresh = 0 in
  let input = [||] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [||] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (intarr2str original) (intarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_above on empty array *)
  let thresh = 100.0 in
  let input = [||] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [||] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatarr2str expect) (floatarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (floatarr2str original) (floatarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
  (* BEG_TEST *)
  (* array_above on long int array *)
  let thresh = 49 in
  let input = [|30; 62; 24; 40; 50; 63; 35; 96; 68; 41; 5; 10; 43; 23; 70; 45; 9; 17; 7; 100; 85; 6; 72; 59; 71; 36; 60; 42; 76; 99; 97; 58; 74; 65; 75; 18; 88; 49; 90; 48; 54; 38; 84; 46; 86; 95; 98; 79; 32; 20; 22; 78; 69; 44; 83; 28; 25; 37; 64; 47; 31; 73; 81; 11; 77; 92; 29; 67; 56; 12; 51; 89; 14; 34; 93; 8; 66; 13; 53; 27; 39; 52; 82; 4; 16; 80; 94; 2; 87; 91; 55; 19; 33; 21; 15; 1; 61; 57; 3; 26;|] in
  let original = Array.copy input in
  let actual = array_above thresh input in
  let expect = [|62; 50; 63; 96; 68; 70; 100; 85; 72; 59; 71; 60; 76; 99; 97; 58; 74; 65; 75; 88; 90; 54; 84; 86; 95; 98; 79; 78; 69; 83; 64; 73; 81; 77; 92; 67; 56; 51; 89; 93; 66; 53; 52; 82; 80; 94; 87; 91; 55; 61; 57|] in
  let msg = sprintf "Expect: %s\nActual: %s" (intarr2str expect) (intarr2str actual) in
  __check__ (actual = expect);
  let msg = sprintf "Input Array Modified\nOriginal: %s\nActual:   %s" (intarr2str original) (intarr2str input) in
  __check__ (input = original);
  (* END_TEST *)
);


(******************************************)
(* list_above tests *)
(fun () ->
  (* BEG_TEST *)
  (* list_above on short int array *)
  let thresh = 0 in
  let input = [0; 1; 2; 0] in
  let actual = list_above thresh input in
  let expect = [1; 2] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_above on short int array *)
  let thresh = 0 in
  let input = [4; -2; -1; 7; 0; 3] in
  let actual = list_above thresh input in
  let expect = [4; 7; 3] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_above on short array *)
  let thresh = 3 in
  let input = [4; -2; -1; 7; 0; 3] in
  let actual = list_above thresh input in
  let expect = [4; 7] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_above on short float array *)
  let thresh = 1.5 in
  let input = [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5] in
  let actual = list_above thresh input in
  let expect = [4.2; 7.6; 8.9; 8.5] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatlist2str expect) (floatlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_above on short array *)
  let thresh = 0.0 in
  let input = [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5] in
  let actual = list_above thresh input in
  let expect = [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatlist2str expect) (floatlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_above on short float array *)
  let thresh = 9.0 in
  let input = [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5] in
  let actual = list_above thresh input in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatlist2str expect) (floatlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_above on short bool array *)
  let thresh = false in
  let input = [false; true; false; true; true;] in
  let actual = list_above thresh input in
  let expect = [true; true; true] in
  let msg = sprintf "Expect: %s\nActual: %s" (boollist2str expect) (boollist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_above on empty array *)
  let thresh = 0 in
  let input = [] in
  let actual = list_above thresh input in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_above on empty array *)
  let thresh = 100.0 in
  let input = [] in
  let actual = list_above thresh input in
  let expect = [] in
  let msg = sprintf "Expect: %s\nActual: %s" (floatlist2str expect) (floatlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* list_above on long int array *)
  let thresh = 49 in
  let input = [30; 62; 24; 40; 50; 63; 35; 96; 68; 41; 5; 10; 43; 23; 70; 45; 9; 17; 7; 100; 85; 6; 72; 59; 71; 36; 60; 42; 76; 99; 97; 58; 74; 65; 75; 18; 88; 49; 90; 48; 54; 38; 84; 46; 86; 95; 98; 79; 32; 20; 22; 78; 69; 44; 83; 28; 25; 37; 64; 47; 31; 73; 81; 11; 77; 92; 29; 67; 56; 12; 51; 89; 14; 34; 93; 8; 66; 13; 53; 27; 39; 52; 82; 4; 16; 80; 94; 2; 87; 91; 55; 19; 33; 21; 15; 1; 61; 57; 3; 26;] in
  let actual = list_above thresh input in
  let expect = [62; 50; 63; 96; 68; 70; 100; 85; 72; 59; 71; 60; 76; 99; 97; 58; 74; 65; 75; 88; 90; 54; 84; 86; 95; 98; 79; 78; 69; 83; 64; 73; 81; 77; 92; 67; 56; 51; 89; 93; 66; 53; 52; 82; 80; 94; 87; 91; 55; 61; 57] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* list_above on long int array *)
  let thresh = 22 in
  let input = [30; 62; 24; 40; 50; 63; 35; 96; 68; 41; 5; 10; 43; 23; 70; 45; 9; 17; 7; 100; 85; 6; 72; 59; 71; 36; 60; 42; 76; 99; 97; 58; 74; 65; 75; 18; 88; 49; 90; 48; 54; 38; 84; 46; 86; 95; 98; 79; 32; 20; 22; 78; 69; 44; 83; 28; 25; 37; 64; 47; 31; 73; 81; 11; 77; 92; 29; 67; 56; 12; 51; 89; 14; 34; 93; 8; 66; 13; 53; 27; 39; 52; 82; 4; 16; 80; 94; 2; 87; 91; 55; 19; 33; 21; 15; 1; 61; 57; 3; 26;] in
  let actual = list_above thresh input in
  let expect = [30; 62; 24; 40; 50; 63; 35; 96; 68; 41; 43; 23; 70; 45; 100; 85; 72; 59; 71; 36; 60; 42; 76; 99; 97; 58; 74; 65; 75; 88; 49; 90; 48; 54; 38; 84; 46; 86; 95; 98; 79; 32; 78; 69; 44; 83; 28; 25; 37; 64; 47; 31; 73; 81; 77; 92; 29; 67; 56; 51; 89; 34; 93; 66; 53; 27; 39; 52; 82; 80; 94; 87; 91; 55; 33; 61; 57; 26] in
  let msg = sprintf "Expect: %s\nActual: %s" (intlist2str expect) (intlist2str actual) in
  __check__ (actual = expect);
  (* END_TEST *)
);
|];;    
    

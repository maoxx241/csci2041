open Sortedlist;;               (* insert / remove / print / merge *)
open Printf;;
open Mltest;;

(* generate error message for an int list *)
let make_intlist_msg listA listB expect actual =
  String.concat "\n" [
      "listA:  "^(intlist2str listA);
      "listB:  "^(intlist2str listB);
      "Expect: "^(intlist2str expect);
      "Actual: "^(intlist2str actual);
    ]
;;

(* generate error message for an string list *)
let make_strlist_msg listA listB expect actual =
  String.concat "\n" [
      "listA:  "^(strlist2str listA);
      "listB:  "^(strlist2str listB);
      "Expect: "^(strlist2str expect);
      "Actual: "^(strlist2str actual);
    ]
;;

Mltest.main [|
(******************************************)
(* sortedlist merge tests *)

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [] in
  let listB = [] in
  let actual = merge listA listB in
  let expect = [] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [1] in
  let listB = [] in
  let actual = merge listA listB in
  let expect = [1] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [] in
  let listB = [2] in
  let actual = merge listA listB in
  let expect = [2] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [1;3;5;7;9;11] in
  let listB = [] in
  let actual = merge listA listB in
  let expect = [1;3;5;7;9;11] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [] in
  let listB = [1;3;5;7;9;11] in
  let actual = merge listA listB in
  let expect = [1;3;5;7;9;11] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.merge short cases *)
  let listA = [1] in
  let listB = [2] in
  let actual = merge listA listB in
  let expect = [1;2] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [2] in
  let listB = [1] in
  let actual = merge listA listB in
  let expect = [1;2] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge base cases *)
  let listA = [1] in
  let listB = [1] in
  let actual = merge listA listB in
  let expect = [1] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.merge 2nd list many positions cases *)
  let listA = [1;3;5] in
  let listB = [2] in
  let actual = merge listA listB in
  let expect = [1;2;3;5] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge 2nd list many positions cases *)
  let listA = [1;3;5] in
  let listB = [6] in
  let actual = merge listA listB in
  let expect = [1;3;5;6] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge 2nd list many positions cases *)
  let listA = [1;3;5] in
  let listB = [4] in
  let actual = merge listA listB in
  let expect = [1;3;4;5] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge 2nd list many positions cases *)
  let listA = [1;3;5] in
  let listB = [0] in
  let actual = merge listA listB in
  let expect = [0;1;3;5] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.merge medium cases *)
  let listA = [1;3;5] in
  let listB = [2;4;6] in
  let actual = merge listA listB in
  let expect = [1;2;3;4;5;6] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge medium cases *)
  let listA = [1;3;5] in
  let listB = [0;2;4;6;8;10] in
  let actual = merge listA listB in
  let expect = [0;1;2;3;4;5;6;8;10] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge medium cases *)
  let listA = [1;3;5;9;11] in
  let listB = [4;6;8] in
  let actual = merge listA listB in
  let expect = [1;3;4;5;6;8;9;11] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge medium cases *)
  let listA = [1;3;5;9;11] in
  let listB = [0;4;6;8;10] in
  let actual = merge listA listB in
  let expect = [0;1;3;4;5;6;8;9;10;11] in
  let msg = make_intlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Sortedlist.merge large string *)
  let listA = ["develop";"population";] in
  let listB = ["according";"close";"director";"four";"guess";"knowledge";"letter";"lose";"nor";"want";] in
  let actual = merge listA listB in
  let expect = ["according"; "close"; "develop"; "director"; "four"; "guess"; "knowledge"; "letter"; "lose"; "nor"; "population"; "want"] in
  let msg = make_strlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge large string *)
  let listA = ["about";"assume";"call";"catch";"develop";"girl";"glass";"patient";"population";"summer";] in
  let listB = ["according";"close";"director";"four";"guess";"knowledge";"letter";"lose";"nor";"want";] in
  let actual = merge listA listB in
  let expect = ["about"; "according"; "assume"; "call"; "catch"; "close"; "develop"; "director"; "four"; "girl"; "glass"; "guess"; "knowledge"; "letter"; "lose"; "nor"; "patient"; "population"; "summer"; "want"] in
  let msg = make_strlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
  (* BEG_TEST *)
  (* Sortedlist.merge large string *)
  let listA = ["ability";"after";"agreement";"better";"born";"call";"central";"create";"detail";"everyone";"factor";"hair";"his";"keep";"record";"safe";"series";"service";"shot";"society";"statement";"them";"upon";"whatever";"window";] in
  let listB = ["ahead";"beautiful";"day";"instead";"method";"my";"particular";"relationship";"win";"wind";] in
  let actual = merge listA listB in
  let expect = ["ability"; "after"; "agreement"; "ahead"; "beautiful"; "better"; "born"; "call"; "central"; "create"; "day"; "detail"; "everyone"; "factor"; "hair"; "his"; "instead"; "keep"; "method"; "my"; "particular"; "record"; "relationship"; "safe"; "series"; "service"; "shot"; "society"; "statement"; "them"; "upon"; "whatever"; "win"; "wind"; "window"] in
  let msg = make_strlist_msg listA listB expect actual in
  __check__ (actual = expect);
  (* END_TEST *)
);

|];;    
    

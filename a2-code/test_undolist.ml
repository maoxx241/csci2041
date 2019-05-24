open Undolist;;                 (* set_to_list, add_elem, remove_elem, merge_with_list, undo, redo *)
open Printf;;
open Mltest;;

let strlistlist2str strlistlist =
  let lines = List.map strlist2str strlistlist in
  let linestr = String.concat ";\n  " lines in
  "[ " ^ linestr ^ " ]\n"
;;

(* generate error message for an string list *)
let make_msg expect_curr expect_undo expect_redo =
  String.concat "\n" [
      if expect_curr = !curr_list then "curr_list ok" else "**MISMATCH** curr_list";
      "Expect curr_list:  "  ^ (strlist2str expect_curr);
      "Actual curr_list:  "  ^ (strlist2str !curr_list);
      "";
      if expect_undo = !undo_stack then "undo_stack ok" else "**MISMATCH** undo_stack";
      "Expect undo_stack:\n" ^ (strlistlist2str expect_undo);
      "Actual undo_stack:\n" ^ (strlistlist2str !undo_stack);
      if expect_redo = !redo_stack then "redo_stack ok" else "**MISMATCH** redo_stack";
      "Expect redo_stack:\n" ^ (strlistlist2str expect_redo);
      "Actual redo_stack:\n" ^ (strlistlist2str !redo_stack);
    ]
;;


(* generate error message for an string list *)
let make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo =
  let str = String.concat "\n" [
      if expect_ret = actual_ret then "return value ok" else "**MISMATCH** return value";
      "Expect undo/redo return: " ^ (string_of_bool expect_ret);
      "Actual undo/redo return: " ^ (string_of_bool actual_ret);
      "";
  ] in
  str^"\n"^(make_msg expect_curr expect_undo expect_redo)^"\n"
;;

Mltest.main [|
(******************************************)
(* undolist.ml tests *)

(fun () ->
  
  (* BEG_TEST *)
  (* Undolist.set_to_list reset_all *)
  Undolist.reset_all ();
  let expect_curr = [] in
  let expect_undo = [] in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );

  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.set_to_list set_to_list then reset_all *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  let expect_curr = ["Korra"] in
  let expect_undo = [[]] in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  Undolist.reset_all ();
  let expect_curr = [] in
  let expect_undo = [] in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
);


(fun () ->
  
  (* BEG_TEST *)
  (* Undolist.set_to_list several times *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  Undolist.set_to_list ["Mako"];
  let expect_curr = ["Mako"] in
  let expect_undo =
    [ ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.set_to_list several times *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  Undolist.set_to_list ["Mako"];
  Undolist.set_to_list ["Bolin"];
  Undolist.set_to_list ["Asami"];
  let expect_curr = ["Asami"] in
  let expect_undo =
    [ ["Bolin"];
      ["Mako"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  (* Undolist.set_to_list several times *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  Undolist.set_to_list ["Korra";"Mako"];
  Undolist.set_to_list ["Bolin";"Korra";"Mako"];
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  let expect_curr = ["Asami";"Bolin";"Korra";"Mako"] in
  let expect_undo =
    [ ["Bolin"; "Korra"; "Mako"];
      ["Korra"; "Mako"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
);

(fun () ->
  
  (* BEG_TEST *)
  (* Undolist.add_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  let expect_curr = ["Korra"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.add_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.add_elem "Asami";
  Undolist.add_elem "Mako";
  Undolist.add_elem "Bolin";
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"] in
  let expect_undo =
    [ ["Asami"; "Korra"; "Mako"];
      ["Asami"; "Korra"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.add_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.add_elem "Asami";
  Undolist.add_elem "Tenzin";
  Undolist.add_elem "Mako";
  Undolist.add_elem "Bolin";
  Undolist.add_elem "Amon";
  Undolist.add_elem "Kuvira";
  let expect_curr = ["Amon"; "Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Tenzin"] in
  let expect_undo =
    [ ["Amon"; "Asami"; "Bolin"; "Korra"; "Mako"; "Tenzin"];
      ["Asami"; "Bolin"; "Korra"; "Mako"; "Tenzin"];
      ["Asami"; "Korra"; "Mako"; "Tenzin"];
      ["Asami"; "Korra"; "Tenzin"];
      ["Asami"; "Korra"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
);

(fun () ->
  
  (* BEG_TEST *)
  (* Undolist.add_elem and remove_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.remove_elem "Korra";
  let expect_curr = [] in
  let expect_undo =
    [ ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.add_elem and remove_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.remove_elem "Mako";
  let expect_curr = ["Korra"] in
  let expect_undo =
    [ ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.add_elem and remove_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.add_elem "Mako";
  Undolist.remove_elem "Korra";
  let expect_curr = ["Mako"] in
  let expect_undo =
    [ ["Korra"; "Mako"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.add_elem and remove_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.add_elem "Mako";
  Undolist.remove_elem "Korra";
  Undolist.add_elem "Bolin";
  Undolist.add_elem "Mako";
  Undolist.remove_elem "Bolin";
  Undolist.remove_elem "Mako";
  let expect_curr = [] in
  let expect_undo =
    [ ["Mako"];
      ["Bolin"; "Mako"];
      ["Bolin"; "Mako"];
      ["Mako"];
      ["Korra"; "Mako"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.add_elem and remove_elem several times *)
  Undolist.reset_all ();
  Undolist.add_elem "Korra";
  Undolist.remove_elem "Korra";
  Undolist.add_elem "Bolin";
  Undolist.remove_elem "Bolin";
  Undolist.remove_elem "Mako";
  Undolist.add_elem "Mako";
  Undolist.add_elem "Korra";
  Undolist.add_elem "Asami";
  Undolist.add_elem "Bolin";
  Undolist.add_elem "Tenzin";
  Undolist.add_elem "Amon";
  Undolist.remove_elem "Korra";
  Undolist.remove_elem "Tenzin";
  let expect_curr = ["Amon"; "Asami"; "Bolin"; "Mako"] in
  let expect_undo =
    [ ["Amon"; "Asami"; "Bolin"; "Mako"; "Tenzin"];
      ["Amon"; "Asami"; "Bolin"; "Korra"; "Mako"; "Tenzin"];
      ["Asami"; "Bolin"; "Korra"; "Mako"; "Tenzin"];
      ["Asami"; "Bolin"; "Korra"; "Mako"];
      ["Asami"; "Korra"; "Mako"];
      ["Korra"; "Mako"];
      ["Mako"];
      [];
      [];
      ["Bolin"];
      [];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
);

(fun () ->
  
  (* BEG_TEST *)
  (* Undolist.merge_with_list *)
  Undolist.reset_all ();
  Undolist.merge_with_list ["Korra"];
  let expect_curr = ["Korra"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.merge_with_list *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  Undolist.merge_with_list ["Bolin";"Mako"];
  let expect_curr = ["Bolin"; "Korra"; "Mako"] in
  let expect_undo =
    [ ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.merge_with_list *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  Undolist.merge_with_list ["Bolin";"Mako"];
  Undolist.merge_with_list ["Asami";"Kuvira";"Tenzin";"Zahir";];
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Tenzin"; "Zahir"] in
  let expect_undo =
    [ ["Bolin"; "Korra"; "Mako"];
      ["Korra"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.merge_with_list *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Korra";"Kuvira";"Mako";"Tenzin";"Zahir";];
  Undolist.merge_with_list ["Asami";"Kuvira";"Tenzin";"Zahir";];
  let expect_curr = ["Asami"; "Korra"; "Kuvira"; "Mako"; "Tenzin"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Korra"; "Kuvira"; "Mako"; "Tenzin"; "Zahir"];
      [] ]
  in
  let expect_redo = [] in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
);

(fun () ->
  
  (* BEG_TEST *)
  (* Undolist.undo operations then undos *)
  Undolist.reset_all ();
  let actual_ret = Undolist.undo () in
  let expect_ret = false in
  let expect_curr = [] in
  let expect_undo =
    [ ]
  in
  let expect_redo =
    [ ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.undo operations then undos *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Korra"];
  let actual_ret = Undolist.undo () in
  let expect_ret = true in
  let expect_curr = [] in
  let expect_undo =
    [ ]
  in
  let expect_redo =
    [ ["Korra"]; ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
  
  (* BEG_TEST *)
  (* Undolist.undo operations then undos *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Bolin";"Korra";"Mako"];
  let actual_ret = Undolist.undo () in
  let expect_ret = true in
  let expect_curr = [] in
  let expect_undo =
    [ ]
  in
  let expect_redo =
    [ ["Bolin"; "Korra"; "Mako"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist.undo times 4 *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Bolin";"Korra";"Mako"];
  Undolist.add_elem "Kuvira";
  Undolist.add_elem "Meelo";
  (* FIRST undo *)
  let actual_ret = Undolist.undo () in
  let expect_ret = true in
  let expect_curr = ["Bolin"; "Korra"; "Kuvira"; "Mako"] in
  let expect_undo =
    [ ["Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [ ["Bolin"; "Korra"; "Kuvira"; "Mako"; "Meelo"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* SECOND undo *)
  let actual_ret = Undolist.undo () in
  let expect_ret = true in
  let expect_curr = ["Bolin"; "Korra"; "Mako"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo =
    [ ["Bolin"; "Korra"; "Kuvira"; "Mako"];
      ["Bolin"; "Korra"; "Kuvira"; "Mako"; "Meelo"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* THIRD undo *)
  let actual_ret = Undolist.undo () in
  let expect_ret = true in
  let expect_curr = [] in
  let expect_undo =
    [ ]
  in
  let expect_redo =
    [ ["Bolin"; "Korra"; "Mako"];
      ["Bolin"; "Korra"; "Kuvira"; "Mako"];
      ["Bolin"; "Korra"; "Kuvira"; "Mako"; "Meelo"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* FOURTH undo fails *)
  let actual_ret = Undolist.undo () in
  let expect_ret = false in
  let expect_curr = [] in
  let expect_undo =
    [ ]
  in
  let expect_redo =
    [ ["Bolin"; "Korra"; "Mako"];
      ["Bolin"; "Korra"; "Kuvira"; "Mako"];
      ["Bolin"; "Korra"; "Kuvira"; "Mako"; "Meelo"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)
);

(fun () ->

  (* BEG_TEST *)
  (* Undolist.undo then redo *)
  Undolist.reset_all ();
  let actual_ret = Undolist.redo () in
  let expect_ret = false in
  let expect_curr = [] in
  let expect_undo =
    [ ]
  in
  let expect_redo =
    [ ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist.undo then redo *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  let _          = Undolist.undo () in
  let actual_ret = Undolist.redo () in
  let expect_ret = true in
  let expect_curr = ["Asami";"Bolin";"Korra";"Mako"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo =
    [ ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist.undo times 4 then redo times 4 *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.merge_with_list ["Asami";"Bolin";"Kuvira";"Zahir"];
  Undolist.remove_elem "Bolin";
  let _          = Undolist.undo () in
  let _          = Undolist.undo () in
  let _          = Undolist.undo () in
  (* FIRST REDO *)
  let actual_ret = Undolist.redo () in
  let expect_ret = true in
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo =
    [ ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* SECOND REDO *)
  let actual_ret = Undolist.redo () in
  let expect_ret = true in
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [ ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* THIRD REDO *)
  let actual_ret = Undolist.redo () in
  let expect_ret = true in
  let expect_curr = ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [ ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* FOURTH REDO fails *)
  let actual_ret = Undolist.redo () in
  let expect_ret = false in
  let expect_curr = ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [ ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

);

(fun () ->

  (* BEG_TEST *)
  (* Undolist undo / redo mixed *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.merge_with_list ["Asami";"Bolin";"Kuvira";"Zahir"];
  Undolist.remove_elem "Bolin";
  Undolist.add_elem "Meelo";
  Undolist.remove_elem "Tenzin";
  Undolist.merge_with_list ["Ikki";"Jinora";"Korra";"Pema";"Tenzin"];
  (* 4 undos *)
  for i=1 to 3 do
    ignore (Undolist.undo ());
  done;
  let actual_ret = Undolist.undo () in
  let expect_ret = true in
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [ ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Zahir"];
      ["Asami"; "Ikki"; "Jinora"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Pema"; "Tenzin"; "Zahir"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.merge_with_list ["Asami";"Bolin";"Kuvira";"Zahir"];
  Undolist.remove_elem "Bolin";
  Undolist.add_elem "Meelo";
  Undolist.remove_elem "Tenzin";
  Undolist.merge_with_list ["Ikki";"Jinora";"Korra";"Pema";"Tenzin"];
  (* 5 undos *)
  for i=1 to 5 do
    ignore (Undolist.undo ());
  done;
  (* 4 redos *)
  for i=1 to 3 do
    ignore (Undolist.undo ());
  done;
  let actual_ret = Undolist.redo () in
  let expect_ret = true in
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo =
    [ ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Zahir"];
      ["Asami"; "Ikki"; "Jinora"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Pema"; "Tenzin"; "Zahir"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.merge_with_list ["Asami";"Bolin";"Kuvira";"Zahir"];
  Undolist.remove_elem "Bolin";
  Undolist.add_elem "Meelo";
  Undolist.remove_elem "Tenzin";
  Undolist.merge_with_list ["Ikki";"Jinora";"Korra";"Pema";"Tenzin"];
  for i=1 to 5 do
    ignore (Undolist.undo ());
  done;
  for i=1 to 3 do
    ignore (Undolist.redo ());
  done;
  for i=1 to 2 do
    ignore (Undolist.undo ());
  done;
  let actual_ret = Undolist.redo () in
  let expect_ret = true in
  let expect_curr = ["Asami"; "Korra"; "Kuvira"; "Mako"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Kuvira"; "Mako"; "Zahir"];
      ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [ ["Asami"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Zahir"];
      ["Asami"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Zahir"];
      ["Asami"; "Ikki"; "Jinora"; "Korra"; "Kuvira"; "Mako"; "Meelo"; "Pema"; "Tenzin"; "Zahir"] ]
  in
  let msg = make_msg_ret expect_ret actual_ret expect_curr expect_undo expect_redo in
  __check__ ( expect_ret  =  actual_ret && expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

);

(fun () ->

  (* BEG_TEST *)
  (* Undolist undo / redo mixed with modifications *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.merge_with_list ["Asami";"Bolin";"Kuvira";"Zahir"];
  ignore (Undolist.undo ());
  Undolist.add_elem "Meelo";
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [  ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed with modifications *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.remove_elem "Tenzin";
  ignore (Undolist.undo ());
  Undolist.add_elem "Meelo";
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [  ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed with modifications *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.remove_elem "Tenzin";
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  Undolist.add_elem "Meelo";
  let expect_curr = ["Meelo"] in
  let expect_undo =
    [ [] ]
  in
  let expect_redo =
    [  ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed with modifications *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.remove_elem "Tenzin";
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  Undolist.add_elem "Meelo";
  ignore (Undolist.redo ());
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [  ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed with modifications *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.remove_elem "Tenzin";
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  Undolist.add_elem "Meelo";
  Undolist.merge_with_list ["Asami";"Bolin";"Korra";"Tenzin";"Zahir"];
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"; "Tenzin"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"];
      ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [  ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

  (* BEG_TEST *)
  (* Undolist undo / redo mixed with modifications *)
  Undolist.reset_all ();
  Undolist.set_to_list ["Asami";"Bolin";"Korra";"Mako"];
  Undolist.remove_elem "Tenzin";
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  Undolist.add_elem "Meelo";
  Undolist.merge_with_list ["Asami";"Bolin";"Korra";"Tenzin";"Zahir"];
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  ignore (Undolist.redo ());
  let expect_curr = ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"; "Tenzin"; "Zahir"] in
  let expect_undo =
    [ ["Asami"; "Bolin"; "Korra"; "Mako"; "Meelo"];
      ["Asami"; "Bolin"; "Korra"; "Mako"];
      [] ]
  in
  let expect_redo =
    [  ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

);

(fun () ->

  (* BEG_TEST *)
  (* Undolist stress tests *)
  Undolist.reset_all ();
  Undolist.set_to_list ["baby";"college";"computer";"enough";"he";"mention";"sure";"table";"usually";"weight";];
  Undolist.merge_with_list ["administration";"agent";"ago";"argue";"brother";"degree";"end";"fish";"great";"need";"project";"red";"result";"same";"scene";"send";"service";"sound";"special";"suddenly";"suggest";"through";"throughout";"vote";"who";];
  Undolist.add_elem "flab";
  Undolist.add_elem "zebra";
  ignore (Undolist.undo ());
  Undolist.remove_elem "enough";
  Undolist.remove_elem "weight";
  Undolist.remove_elem "red";
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  Undolist.merge_with_list ["else";"meeting";"per";"person";"position";];
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  ignore (Undolist.redo ());
  Undolist.add_elem "red";
  Undolist.remove_elem "brother";
  Undolist.remove_elem "he";
  Undolist.add_elem "great";
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  Undolist.merge_with_list ["administration";"agent";"ago";"argue";"brother";"degree";"end";"fish";"great";"need";"project";"red";"result";"same";"scene";"send";"service";"sound";"special";"suddenly";"suggest";"through";"throughout";"vote";"who";];
  Undolist.add_elem "sister";
  Undolist.add_elem "mass";
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.undo ());
  ignore (Undolist.redo ());
  let expect_curr = ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"] in
  let expect_undo =
    [ ["administration"; "agent"; "ago"; "argue"; "baby"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "he"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "he"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "he"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "enough"; "fish"; "flab"; "great"; "he"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "enough"; "fish"; "great"; "he"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["baby"; "college"; "computer"; "enough"; "he"; "mention"; "sure"; "table"; "usually"; "weight"];
      [] ]
  in
  let expect_redo =
    [ ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sister"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"];
      ["administration"; "agent"; "ago"; "argue"; "baby"; "brother"; "college"; "computer"; "degree"; "end"; "fish"; "flab"; "great"; "mass"; "mention"; "need"; "project"; "red"; "result"; "same"; "scene"; "send"; "service"; "sister"; "sound"; "special"; "suddenly"; "suggest"; "sure"; "table"; "through"; "throughout"; "usually"; "vote"; "weight"; "who"] ]
  in
  let msg = make_msg expect_curr expect_undo expect_redo in
  __check__ ( expect_curr = !curr_list && expect_undo = !undo_stack && expect_redo = !redo_stack );
  (* END_TEST *)

);

|];;    

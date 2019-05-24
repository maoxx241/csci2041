open Document;;                 (* make, set, undo, redo *)
open Printf;;
open Mltest;;

(* Convert string list list to a string representation. *)
let strlistlist2str strlistlist =
  let lines = List.map strlist2str strlistlist in
  let linestr = String.concat ";\n  " lines in
  "[ " ^ linestr ^ " ]\n"
;;

(* Create a string version of a doc. tostring is a function that will
   stringify the data type of the doc. *)
let doc2str tostring doc =
  let state_str = tostring doc.current in
  let undo_strings = List.map tostring doc.undo_stack in
  let undo_str = String.concat ";\n    " undo_strings in
  let redo_strings = List.map tostring doc.redo_stack in
  let redo_str = String.concat ";\n    " redo_strings in
  String.concat "\n" [
      sprintf  "{ current= %s;" state_str;
      sprintf  "  undo_stack= [";
      "    "^undo_str;
      "  ];";
      sprintf  "  redo_stack= [";
      "    "^redo_str;
      "  ];";
      "}"
    ];;
;;

(* generate error message for an string list *)
let make_msg tostring expect_doc actual_doc  =
  String.concat "\n" [
      "FIELDS:";
      "current:    "^(if expect_doc.current = actual_doc.current       then "ok" else "**MISMATCH**");
      "undo_stack: "^(if expect_doc.undo_stack = actual_doc.undo_stack then "ok" else "**MISMATCH**");
      "redo_stack: "^(if expect_doc.redo_stack = actual_doc.redo_stack then "ok" else "**MISMATCH**");
      "";
      "EXPECT DOC:";
      doc2str tostring expect_doc;
      "";
      "ACTUAL DOC:";
      doc2str tostring actual_doc;
      "";
    ];;
;;

(* generate error message for an string list *)
let make_msg_ret tostring expect_ret actual_ret expect_doc actual_doc =
  let str = String.concat "\n" [
      if expect_ret = actual_ret then "return value ok" else "**MISMATCH** return value";
      "Expect undo/redo return: " ^ (string_of_bool expect_ret);
      "Actual undo/redo return: " ^ (string_of_bool actual_ret);
      "";
  ] in
  str^"\n"^(make_msg tostring expect_doc actual_doc)^"\n"
;;

Mltest.main [|
(******************************************)
(* document.ml tests *)

(fun () ->
  (* BEG_TEST *)
  (* Document.make with string doc *)
  let actual_doc = Document.make "Korra" in
  let expect_doc =
    { current= "Korra";
      undo_stack= [
          
        ];
      redo_stack= [
          
        ];
    }
  in
  let msg = make_msg quote_str expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Document.make with int doc *)
  let actual_doc = Document.make 42 in
  let expect_doc =
    { current= 42;
      undo_stack= [

        ];
      redo_stack= [
          
        ];
    }
  in
  let msg = make_msg string_of_int expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);

(fun () ->

  (* BEG_TEST *)
  (* Document.set once *)
  let actual_doc = Document.make "Korra" in
  Document.set actual_doc "Asami";
  let expect_doc =
    { current= "Asami";
      undo_stack= [
          "Korra"
        ];
      redo_stack= [
          
        ];
    }
  in
  let msg = make_msg quote_str expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Document.set multiple times *)
  let actual_doc = Document.make "Korra" in
  Document.set actual_doc "Asami";
  Document.set actual_doc "Mako";
  Document.set actual_doc "Bolin";
  Document.set actual_doc "Tenzin";
  let expect_doc =
    { current= "Tenzin";
      undo_stack= [
          "Bolin";
          "Mako";
          "Asami";
          "Korra"
        ];
      redo_stack= [
          
        ];
    }
  in
  let msg = make_msg quote_str expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* Document.undo false *)
  let actual_doc = Document.make "Korra" in
  let actual_ret = Document.undo actual_doc in
  let expect_ret = false in
  let expect_doc =
    { current= "Korra";
      undo_stack= [
        ];
      redo_stack= [

        ];
    }
  in
  let msg = make_msg_ret quote_str expect_ret actual_ret expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
  (* BEG_TEST *)
  (* Document.redo false *)
  let actual_doc = Document.make "Korra" in
  let actual_ret = Document.redo actual_doc in
  let expect_ret = false in
  let expect_doc =
    { current= "Korra";
      undo_stack= [
        ];
      redo_stack= [

        ];
    }
  in
  let msg = make_msg_ret quote_str expect_ret actual_ret expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* Document.set single undo *)
  let actual_doc = Document.make "Korra" in
  Document.set actual_doc "Asami";
  Document.set actual_doc "Mako";
  Document.set actual_doc "Bolin";
  Document.set actual_doc "Tenzin";
  let actual_ret = Document.undo actual_doc in
  let expect_ret = true in
  let expect_doc =
    { current= "Bolin";
      undo_stack= [
          "Mako";
          "Asami";
          "Korra"
        ];
      redo_stack= [
          "Tenzin"
        ];
    }
  in
  let msg = make_msg_ret quote_str expect_ret actual_ret expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);
 
(fun () ->
  (* BEG_TEST *)
  (* Document.set multiple undo *)
  let actual_doc = Document.make "Korra" in
  Document.set actual_doc "Asami";
  Document.set actual_doc "Mako";
  Document.set actual_doc "Bolin";
  Document.set actual_doc "Tenzin";
  Document.set actual_doc "Amon";
  Document.set actual_doc "Kuvira";
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let actual_ret = Document.undo actual_doc in
  let expect_ret = true in
  let expect_doc =
      { current= "Bolin";
        undo_stack= [
            "Mako";
            "Asami";
            "Korra"
          ];
        redo_stack= [
            "Tenzin";
            "Amon";
            "Kuvira"
          ];
      }
  in
  let msg = make_msg_ret quote_str expect_ret actual_ret expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);  

(fun () ->
  (* BEG_TEST *)
  (* Multiple undo then redo once *)
  let actual_doc = Document.make "Korra" in
  Document.set actual_doc "Asami";
  Document.set actual_doc "Mako";
  Document.set actual_doc "Bolin";
  Document.set actual_doc "Tenzin";
  Document.set actual_doc "Amon";
  Document.set actual_doc "Kuvira";
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let actual_ret = Document.redo actual_doc in
  let expect_ret = true in
  let expect_doc =
    { current= "Tenzin";
      undo_stack= [
          "Bolin";
          "Mako";
          "Asami";
          "Korra"
        ];
      redo_stack= [
          "Amon";
          "Kuvira"
        ];
    }
  in
  let msg = make_msg_ret quote_str expect_ret actual_ret expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);  

(fun () ->
  (* BEG_TEST *)
  (* Multiple undo then redo multiple *)
  let actual_doc = Document.make "Korra" in
  Document.set actual_doc "Asami";
  Document.set actual_doc "Mako";
  Document.set actual_doc "Bolin";
  Document.set actual_doc "Tenzin";
  Document.set actual_doc "Amon";
  Document.set actual_doc "Kuvira";
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let _ = Document.undo actual_doc in
  let _ = Document.redo actual_doc in
  let _ = Document.redo actual_doc in
  let actual_ret = Document.redo actual_doc in
  let expect_ret = true in
  let expect_doc =
    { current= "Tenzin";
      undo_stack= [
          "Bolin";
          "Mako";
          "Asami";
          "Korra"
        ];
      redo_stack= [
          "Amon";
          "Kuvira"
        ];
    }
  in
  let msg = make_msg_ret quote_str expect_ret actual_ret expect_doc actual_doc in
  __check__ ( expect_doc = actual_doc );
  (* END_TEST *)
);
(fun () ->
  (* BEG_TEST *)
  (* Multiple docs *)
  let doc1 = Document.make "Korra" in
  let doc2 = Document.make "Amon" in
  Document.set doc1 "Asami";
  Document.set doc1 "Mako";
  Document.set doc2 "Kuvira";
  Document.set doc1 "Bolin";
  let ret1 = Document.undo doc1 in
  let expect_ret1 = true in
  let ret2 = Document.redo doc2 in
  let expect_ret2 = false in
  let expect_doc1 =
    { current= "Mako";
      undo_stack= [
          "Asami";
          "Korra"
        ];
      redo_stack= [
          "Bolin"
        ];
    }
  in
  let expect_doc2 =
    { current= "Kuvira";
      undo_stack= [
          "Amon"
        ];
      redo_stack= [
          
        ];
    }
  in
  let msg = make_msg_ret quote_str expect_ret1 ret1 expect_doc1 doc1 in
  __check__ ( expect_doc1 = doc1 );
  let msg = make_msg_ret quote_str expect_ret2 ret2 expect_doc2 doc2 in
  __check__ ( expect_doc2 = doc2 );
  (* END_TEST *)
);  
|];;    

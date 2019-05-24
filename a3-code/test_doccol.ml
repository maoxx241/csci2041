open Doccol;;                   (* make, add, remove, has, switch, string_of_doccol *)
open Document;;
open Printf;;
open Mltest;;

(* files for diffing *)
let actual_file = "test-data/actual.tmp";;
let expect_file = "test-data/expect.tmp";;
let diff_file   = "test-data/diff.tmp"  ;;
let msgref = ref "";;

(* Create a string version of a doc. tostring is a function that will
   stringify the data type of the doc. *)
let doc2str tostring doc =
  let state_str = tostring doc.current in
  let undo_strings = List.map tostring doc.undo_stack in
  let undo_str = String.concat ";\n      " undo_strings in
  let redo_strings = List.map tostring doc.redo_stack in
  let redo_str = String.concat ";\n      " redo_strings in
  String.concat "\n" [
      sprintf  "   { current= %s;" state_str;
      sprintf  "     undo_stack= [";
      "       "^undo_str;
      "     ];";
      sprintf  "     redo_stack= [";
      "       "^redo_str;
      "     ];";
      "   }"
    ];;
;;

(* Create a string version of a doccol *)
let doccol2str tostring doccol =
  let name_docstr (name,doc) =
    sprintf "  (\"%s\",\n%s);\n" name (doc2str tostring doc)
  in
  let ndstrs = List.map name_docstr doccol.docs in
  let ndline = String.concat "\n" ndstrs in
  String.concat "\n" [
      sprintf "{ curname= \"%s\"; count= %d;" doccol.curname doccol.count;
      sprintf "  curdoc = \n%s;" (doc2str tostring doccol.curdoc);
      sprintf "  docs = [\n%s  ];" ndline;
      sprintf "}";
    ]
;;

(* ok or mismatch string *)
let ok_str a b =
  if a = b then "ok" else "**MISMATCH**"
;;


(* generate error message for an string list *)
let make_msg tostring expect_col actual_col  =
  let helper exp act =
    let fmt = format_of_string "%-20s vs %-20s : %s\n        " in
    match exp,act with
      | Some (exp_name,exp_doc), Some (act_name,act_doc) ->
         sprintf fmt exp_name act_name (ok_str exp act)
      | Some (exp_name,_), None ->
         sprintf fmt exp_name "-" (ok_str true false)
      | None, Some (act_name,_) ->
         sprintf fmt "-" act_name (ok_str true false)
      | _ ->
         failwith "shouldn't get None,None from map2opt"
  in
  let pair_strs = Mltest.map2opt helper expect_col.docs actual_col.docs in
  let docs_match_str = String.concat " " pair_strs in
  let fmt = format_of_string "%-20s vs %-20s : %s" in

  (* let docs_match_str = "" in *)

  String.concat "\n" [
      "DOCCOL FIELDS:";
      "curname: "^(sprintf fmt expect_col.curname actual_col.curname (ok_str expect_col.curname actual_col.curname));
      "count:   "^(sprintf fmt (string_of_int expect_col.count) (string_of_int actual_col.count) (ok_str expect_col.count actual_col.count));
      "curdoc:  "^(sprintf fmt "..." "..." (ok_str expect_col.curdoc actual_col.curdoc));
      "docs:    "^(ok_str expect_col.docs       actual_col.docs);
      "         "^docs_match_str;
      "EXPECT DOCCOL:";
      doccol2str tostring expect_col;
      "";
      "ACTUAL DOC:";
      doccol2str tostring actual_col;
      "";
    ];;
;;

(* generate error message for an string list *)
let make_msg_ret tostring expect_ret actual_ret expect_col actual_col =
  let str = String.concat "\n" [
      if expect_ret = actual_ret then "return value ok" else "**MISMATCH** return value";
      "Expect return: " ^ (string_of_bool expect_ret);
      "Actual return: " ^ (string_of_bool actual_ret);
      "";
  ] in
  str^"\n"^(make_msg tostring expect_col actual_col)^"\n"
;;

(* (\* Create a *\)
 * let make_msg_str expect_str actual_str =
 *   List.concat "\n" [
 *       "EXPECT:";
 *       expect_str;
 *       "";
 *       "ACTUAL";
 *       actual_str;
 *     ]
 * ;; *)

Mltest.main [|
(******************************************)
(* doccol.ml tests *)

(fun () ->
  (* BEG_TEST *)
  (* make with string doc *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 1;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg quote_str expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
  (* BEG_TEST *)
  (* make with int doc *)
  let actual_col = Doccol.make "num7.txt" (Document.make 7) in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "num7.txt"; count= 1;
  curdoc = 
   { current= 7;
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("num7.txt",
   { current= 7;
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg string_of_int expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* make and add with string doc *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let act_ret = Doccol.add actual_col "other.txt" (Document.make "Asami") in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 2;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("other.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
  (* BEG_TEST *)
  (* make and add several *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let _ = Doccol.add actual_col "first.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Mako") in
  let act_ret = Doccol.add actual_col "last.txt" (Document.make "Tenzin") in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 6;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("last.txt",
   { current= "Tenzin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* add preservies uniqueness several *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let _ = Doccol.add actual_col "first.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Korra") in
  let act_ret = Doccol.add actual_col "second.txt" (Document.make "Tenzin") in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 5;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("fourth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* make and add with string doc *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let act_ret = Doccol.add actual_col "other.txt" (Document.make "Asami") in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 2;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("other.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
  (* BEG_TEST *)
  (* make and add several *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let _ = Doccol.add actual_col "first.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Mako") in
  let act_ret = Doccol.add actual_col "last.txt" (Document.make "Tenzin") in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 6;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("last.txt",
   { current= "Tenzin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
  (* BEG_TEST *)
  (* add preservies uniqueness *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let _ = Doccol.add actual_col "first.txt"  (Document.make "Asami") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Korra") in
  let act_ret = Doccol.add actual_col "second.txt" (Document.make "Tenzin") in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 5;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("fourth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* remove success *)
  let actual_col = Doccol.make "default.txt" (Document.make "Korra") in
  let _ = Doccol.add actual_col "first.txt"  (Document.make "Asami") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Korra") in
  let act_ret = Doccol.remove actual_col "second.txt" in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "default.txt"; count= 4;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("fourth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("default.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
  (* BEG_TEST *)
  (* remove several *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let _ = Doccol.remove actual_col "third.txt" in
  let _ = Doccol.remove actual_col "fifth.txt" in
  let act_ret = Doccol.remove actual_col "second.txt" in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.txt"; count= 3;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
  (* BEG_TEST *)
  (* remove fails *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let _ = Doccol.remove actual_col "x.txt" in
  let _ = Doccol.remove actual_col "y.txt" in
  let act_ret = Doccol.remove actual_col "z.txt" in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.txt"; count= 6;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* has succeeds/fails *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.txt"; count= 6;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let act_ret = Doccol.has actual_col "fourth.txt" in
  let exp_ret = true in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  let act_ret = Doccol.has actual_col "nope.txt" in
  let exp_ret = false in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  let act_ret = Doccol.has actual_col "first.txt" in
  let exp_ret = true in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  let act_ret = Doccol.has actual_col "nada.txt" in
  let exp_ret = false in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* switch succeeds *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let act_ret = Doccol.switch actual_col "fourth.txt" in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "fourth.txt"; count= 6;
  curdoc = 
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)

  (* BEG_TEST *)
  (* several switches succeed *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let _ = Doccol.switch actual_col "fourth.txt" in
  let _ = Doccol.switch actual_col "second.txt" in
  let _ = Doccol.switch actual_col "first.txt" in
  let act_ret = Doccol.switch actual_col "third.txt" in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "third.txt"; count= 6;
  curdoc = 
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)

  (* BEG_TEST *)
  (* switch fails *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let act_ret = Doccol.switch actual_col "third.txt" in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.txt"; count= 2;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)

  (* switch mixed success *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let _ = Doccol.switch actual_col "fourth.txt" in
  let _ = Doccol.switch actual_col "x.txt" in
  let _ = Doccol.switch actual_col "sixth.txt" in
  let act_ret = Doccol.switch actual_col "y.txt" in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "sixth.txt"; count= 6;
  curdoc = 
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* can't remove current *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let act_ret = Doccol.remove actual_col "first.txt" in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.txt"; count= 6;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)

  (* BEG_TEST *)
  (* can't remove current *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let _ = Doccol.switch actual_col "fifth.txt" in
  let _ = Doccol.remove actual_col "first.txt" in
  let _ = Doccol.remove actual_col "third.txt" in
  let act_ret = Doccol.remove actual_col "fifth.txt" in
  let exp_ret = false in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "fifth.txt"; count= 4;
  curdoc = 
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("sixth.txt",
   { current= "Meelo";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fifth.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* string_of_doccol  *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let act_str = Doccol.string_of_doccol actual_col in
  let exp_str = "\
1 docs
- first.txt
" in
  __check_output___ (check_diff_str exp_str expect_file act_str actual_file diff_file msgref);
  (* END_TEST *)

  (* BEG_TEST *)
  (* string_of_doccol  *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let act_str = Doccol.string_of_doccol actual_col in
  let exp_str = "\
2 docs
- second.txt
- first.txt
" in
  __check_output___ (check_diff_str exp_str expect_file act_str actual_file diff_file msgref);
  (* END_TEST *)

  (* BEG_TEST *)
  (* string_of_doccol  *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let _ = Doccol.add actual_col "third.txt"  (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.add actual_col "fifth.txt"  (Document.make "Korra") in
  let _ = Doccol.add actual_col "sixth.txt"  (Document.make "Meelo") in
  let act_str = Doccol.string_of_doccol actual_col in
  let exp_str = "\
6 docs
- sixth.txt
- fifth.txt
- fourth.txt
- third.txt
- second.txt
- first.txt
" in
  __check_output___ (check_diff_str exp_str expect_file act_str actual_file diff_file msgref);
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* stress test  *)
  let actual_col = Doccol.make  "first.txt"  (Document.make "Korra") in
  let act_str = Doccol.string_of_doccol actual_col in
  let exp_str = "\
1 docs
- first.txt
" in
  __check_output___ (check_diff_str exp_str expect_file act_str actual_file diff_file msgref);

  let _ = Doccol.add actual_col "second.txt" (Document.make "Asami") in
  let act_ret = Doccol.switch actual_col "second.txt" in
  let exp_ret = true in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "second.txt"; count= 2;
  curdoc = 
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("second.txt",
   { current= "Asami";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  __check__ ( exp_ret = act_ret );
   
  let act_ret = Doccol.switch actual_col "x.txt" in
  let exp_ret = false in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( exp_ret = act_ret );

  let _ = Doccol.add actual_col "third.txt" (Document.make "Mako") in
  let _ = Doccol.add actual_col "fourth.txt" (Document.make "Bolin") in
  let _ = Doccol.switch actual_col "third.txt" in
  let act_ret = Doccol.remove actual_col "second.txt" in
  let exp_ret = true in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( exp_ret = act_ret );
  
  let expect_col =
  (* BEG_OMIT *)
{ curname= "third.txt"; count= 3;
  curdoc = 
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("fourth.txt",
   { current= "Bolin";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("third.txt",
   { current= "Mako";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("first.txt",
   { current= "Korra";
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  in
  let msg = make_msg_ret quote_str exp_ret act_ret expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);


|];;    

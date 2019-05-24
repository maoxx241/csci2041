(* Mon Oct 15 11:13:15 CDT 2018 : bug fix for empty string in last test *)

open Bulkops;;                  (* showall, saveall, addall, mergeall *)
open Doccol;;
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
      "ACTUAL DOCCOL:";
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

(* Create a doccol from a list of name/entries *)
let make_doccol lists =
  match lists with
  | [] -> failwith "Can't make an empty doccol"
  | (name,entries)::tail ->
     let doccol = Doccol.make name (Document.make entries) in
     List.iter (fun (name,entries)-> ignore(Doccol.add doccol name (Document.make entries))) tail;
     doccol
;;

(* Check name/list files have been created with appropriate contents *)
let check_all_entry_files entries =
  let check_one (name,list)  =
    let expect_file = (name^".expect.tmp") in
    Util.strlist_to_file list expect_file;
    __check_output__ (check_diff expect_file name diff_file msgref);
  in
  List.iter check_one entries;
;;

(* generate error message merge tests *)
let make_mergeall_msg expect_merge actual_merge expect_col actual_col =
  String.concat "\n" [
      "mergeall incorrect results";
      "Merged lists: "^(ok_str expect_merge actual_merge);
      "EXPECT: "^(strlist2str expect_merge);
      "ACTUAL: "^(strlist2str actual_merge);
      "";
      "doccol should not be mutated by mergeall";
    ]^(make_msg strlist2str expect_col actual_col)
;;



Mltest.main [|
(******************************************)
(* bulkops.ml tests *)

(fun () ->
  (* BEG_TEST *)
  (* showall with single document in collection *)
  let entries = 
    [("test-data/first.tmp", ["Bolin";"Korra";"Mako"]);
    ] in
  let actual_col = make_doccol entries in
  let thunk = (fun () -> Bulkops.showall actual_col) in
  save_stdout actual_file thunk;
  let expect_string = "\
--List test-data/first.tmp--
Bolin
Korra
Mako
" in
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)

  (* BEG_TEST *)
  (* showall with several documents in collection *)
  let entries = 
    [("test-data/first.tmp", ["Asami"; "Bolin";"Korra";"Mako"]);
     ("test-data/second.tmp", ["Pema";"Tenzin"]);
     ("test-data/third.tmp", ["Ikki";"Jinora";"Meelo";"Rohan"]);
    ] in
  let actual_col = make_doccol entries in
  let thunk = (fun () -> Bulkops.showall actual_col) in
  save_stdout actual_file thunk;
  let expect_string = "\
--List test-data/third.tmp--
Ikki
Jinora
Meelo
Rohan

--List test-data/second.tmp--
Pema
Tenzin

--List test-data/first.tmp--
Asami
Bolin
Korra
Mako
" in
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* showall with many short documents in collection *)
  let entries = 
    [("test-data/x.tmp", ["Korra";]);
     ("test-data/y.tmp", ["Bolin";]);
     ("test-data/z.tmp", ["Mako"]);
     ("test-data/a.tmp", ["Asami"]);
     ("test-data/b.tmp", ["Meelo"]);
     ("test-data/c.tmp", ["Bumi"]);
     ("test-data/d.tmp", ["Kya"]);
    ] in
  let actual_col = make_doccol entries in
  let thunk = (fun () -> Bulkops.showall actual_col) in
  save_stdout actual_file thunk;
  let expect_string = "\
--List test-data/d.tmp--
Kya

--List test-data/c.tmp--
Bumi

--List test-data/b.tmp--
Meelo

--List test-data/a.tmp--
Asami

--List test-data/z.tmp--
Mako

--List test-data/y.tmp--
Bolin

--List test-data/x.tmp--
Korra
" in
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)

  (* BEG_TEST *)
  (* showall with many documents in collection *)
  let entries = 
    [("test-data/x.tmp", ["Korra";"Mako"]);
     ("test-data/y.tmp", ["Bolin";"Mako"]);
     ("test-data/z.tmp", ["Mako";"Tenzin";]);
     ("test-data/a.tmp", ["Asami";"Hiroshi";"Lin"]);
     ("test-data/b.tmp", ["Jinora";"Meelo"]);
     ("test-data/d.tmp", ["Bumi";"Kya"]);
     ("test-data/t.tmp", ["Amon";"Kuvira";"Unalaq";"Zaheer"]);
     ("test-data/u.tmp", ["Iroh";"Toph";"Zuko"]);
    ] in
  let actual_col = make_doccol entries in
  let thunk = (fun () -> Bulkops.showall actual_col) in
  save_stdout actual_file thunk;
  let expect_string = "\
--List test-data/u.tmp--
Iroh
Toph
Zuko

--List test-data/t.tmp--
Amon
Kuvira
Unalaq
Zaheer

--List test-data/d.tmp--
Bumi
Kya

--List test-data/b.tmp--
Jinora
Meelo

--List test-data/a.tmp--
Asami
Hiroshi
Lin

--List test-data/z.tmp--
Mako
Tenzin

--List test-data/y.tmp--
Bolin
Mako

--List test-data/x.tmp--
Korra
Mako
" in
  __check_output__ (check_diff_expect expect_string expect_file actual_file diff_file msgref);
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* saveall with single document in collection *)
  let entries = 
    [("test-data/first.tmp", ["Bolin";"Korra";"Mako"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.saveall actual_col;
  check_all_entry_files entries;
  (* END_TEST *)

  (* BEG_TEST *)
  (* showall with several documents in collection *)
  let entries = 
    [("test-data/first.tmp", ["Asami"; "Bolin";"Korra";"Mako"]);
     ("test-data/second.tmp", ["Pema";"Tenzin"]);
     ("test-data/third.tmp", ["Ikki";"Jinora";"Meelo";"Rohan"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.saveall actual_col;
  check_all_entry_files entries;
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* saveall with with many short documents in collection  *)
  let entries = 
    [("test-data/x.tmp", ["Korra";]);
     ("test-data/y.tmp", ["Bolin";]);
     ("test-data/z.tmp", ["Mako"]);
     ("test-data/a.tmp", ["Asami"]);
     ("test-data/b.tmp", ["Meelo"]);
     ("test-data/c.tmp", ["Bumi"]);
     ("test-data/d.tmp", ["Kya"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.saveall actual_col;
  check_all_entry_files entries;
  (* END_TEST *)

  (* BEG_TEST *)
  (* saveall with many documents in collection *)
  let entries = 
    [("test-data/x.tmp", ["Korra";"Mako"]);
     ("test-data/y.tmp", ["Bolin";"Mako"]);
     ("test-data/z.tmp", ["Mako";"Tenzin";]);
     ("test-data/a.tmp", ["Asami";"Hiroshi";"Lin"]);
     ("test-data/b.tmp", ["Jinora";"Meelo"]);
     ("test-data/d.tmp", ["Bumi";"Kya"]);
     ("test-data/t.tmp", ["Amon";"Kuvira";"Unalaq";"Zaheer"]);
     ("test-data/u.tmp", ["Iroh";"Toph";"Zuko"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.saveall actual_col;
  check_all_entry_files entries;
  (* END_TEST *)
);


(fun () ->
  (* BEG_TEST *)
  (* addall with single document in collection *)
  let entries = 
    [("test-data/first.tmp", ["Bolin";"Korra";"Mako"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.addall actual_col "Jinora";
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/first.tmp"; count= 1;
  curdoc = 
   { current= ["Bolin"; "Jinora"; "Korra"; "Mako"];
     undo_stack= [
       ["Bolin"; "Korra"; "Mako"]
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/first.tmp",
   { current= ["Bolin"; "Jinora"; "Korra"; "Mako"];
     undo_stack= [
       ["Bolin"; "Korra"; "Mako"]
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg strlist2str expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)

  (* BEG_TEST *)
  (* addall with several documents in collection *)
  let entries = 
    [("test-data/first.tmp", ["Asami"; "Bolin";"Korra";"Mako"]);
     ("test-data/second.tmp", ["Pema";"Tenzin"]);
     ("test-data/third.tmp", ["Ikki";"Jinora";"Meelo";"Rohan"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.addall actual_col "Jinora";
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/first.tmp"; count= 3;
  curdoc = 
   { current= ["Asami"; "Bolin"; "Jinora"; "Korra"; "Mako"];
     undo_stack= [
       ["Asami"; "Bolin"; "Korra"; "Mako"]
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/third.tmp",
   { current= ["Ikki"; "Jinora"; "Meelo"; "Rohan"];
     undo_stack= [
       ["Ikki"; "Jinora"; "Meelo"; "Rohan"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/second.tmp",
   { current= ["Jinora"; "Pema"; "Tenzin"];
     undo_stack= [
       ["Pema"; "Tenzin"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/first.tmp",
   { current= ["Asami"; "Bolin"; "Jinora"; "Korra"; "Mako"];
     undo_stack= [
       ["Asami"; "Bolin"; "Korra"; "Mako"]
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg strlist2str expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* addall with with many short documents in collection  *)
  let entries = 
    [("test-data/x.tmp", ["Korra";]);
     ("test-data/y.tmp", ["Bolin";]);
     ("test-data/z.tmp", ["Mako"]);
     ("test-data/a.tmp", ["Asami"]);
     ("test-data/b.tmp", ["Jinora"]);
     ("test-data/c.tmp", ["Bumi"]);
     ("test-data/d.tmp", ["Kya"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.addall actual_col "Jinora";
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/x.tmp"; count= 7;
  curdoc = 
   { current= ["Jinora"; "Korra"];
     undo_stack= [
       ["Korra"]
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/d.tmp",
   { current= ["Jinora"; "Kya"];
     undo_stack= [
       ["Kya"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/c.tmp",
   { current= ["Bumi"; "Jinora"];
     undo_stack= [
       ["Bumi"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/b.tmp",
   { current= ["Jinora"];
     undo_stack= [
       ["Jinora"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/a.tmp",
   { current= ["Asami"; "Jinora"];
     undo_stack= [
       ["Asami"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/z.tmp",
   { current= ["Jinora"; "Mako"];
     undo_stack= [
       ["Mako"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/y.tmp",
   { current= ["Bolin"; "Jinora"];
     undo_stack= [
       ["Bolin"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/x.tmp",
   { current= ["Jinora"; "Korra"];
     undo_stack= [
       ["Korra"]
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg strlist2str expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)

  (* BEG_TEST *)
  (* addall with many documents in collection *)
  let entries = 
    [("test-data/x.tmp", ["Korra";"Mako"]);
     ("test-data/y.tmp", ["Bolin";"Mako"]);
     ("test-data/z.tmp", ["Mako";"Tenzin";]);
     ("test-data/a.tmp", ["Asami";"Hiroshi";"Lin"]);
     ("test-data/b.tmp", ["Jinora";"Meelo"]);
     ("test-data/d.tmp", ["Bumi";"Kya"]);
     ("test-data/t.tmp", ["Amon";"Kuvira";"Unalaq";"Zaheer"]);
     ("test-data/u.tmp", ["Iroh";"Toph";"Zuko"]);
    ] in
  let actual_col = make_doccol entries in
  Bulkops.addall actual_col "Jinora";
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/x.tmp"; count= 8;
  curdoc = 
   { current= ["Jinora"; "Korra"; "Mako"];
     undo_stack= [
       ["Korra"; "Mako"]
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/u.tmp",
   { current= ["Iroh"; "Jinora"; "Toph"; "Zuko"];
     undo_stack= [
       ["Iroh"; "Toph"; "Zuko"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/t.tmp",
   { current= ["Amon"; "Jinora"; "Kuvira"; "Unalaq"; "Zaheer"];
     undo_stack= [
       ["Amon"; "Kuvira"; "Unalaq"; "Zaheer"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/d.tmp",
   { current= ["Bumi"; "Jinora"; "Kya"];
     undo_stack= [
       ["Bumi"; "Kya"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/b.tmp",
   { current= ["Jinora"; "Meelo"];
     undo_stack= [
       ["Jinora"; "Meelo"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/a.tmp",
   { current= ["Asami"; "Hiroshi"; "Jinora"; "Lin"];
     undo_stack= [
       ["Asami"; "Hiroshi"; "Lin"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/z.tmp",
   { current= ["Jinora"; "Mako"; "Tenzin"];
     undo_stack= [
       ["Mako"; "Tenzin"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/y.tmp",
   { current= ["Bolin"; "Jinora"; "Mako"];
     undo_stack= [
       ["Bolin"; "Mako"]
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/x.tmp",
   { current= ["Jinora"; "Korra"; "Mako"];
     undo_stack= [
       ["Korra"; "Mako"]
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_msg strlist2str expect_col actual_col in
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* mergeall with single document in collection *)
  let entries = 
    [("first.tmp", ["Bolin";"Korra";"Mako"]);
    ] in
  let actual_col = make_doccol entries in
  let actual_merge = Bulkops.mergeall actual_col in
  let expect_merge =
    ["Bolin"; "Korra"; "Mako"]
  in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.tmp"; count= 1;
  curdoc = 
   { current= ["Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("first.tmp",
   { current= ["Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_mergeall_msg expect_merge actual_merge expect_col actual_col in
  __check__ ( expect_merge = actual_merge );
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* mergeall, 1 doc with contents, second empty *)
  let entries = 
    [("first.tmp", ["Bolin";"Korra";"Mako"]);
     ("second.tmp", []);
    ] in
  let actual_col = make_doccol entries in
  let actual_merge = Bulkops.mergeall actual_col in
  let expect_merge =
    ["Bolin"; "Korra"; "Mako"]
  in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "first.tmp"; count= 2;
  curdoc = 
   { current= ["Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("second.tmp",
   { current= [];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("first.tmp",
   { current= ["Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_mergeall_msg expect_merge actual_merge expect_col actual_col in
  __check__ ( expect_merge = actual_merge );
  __check__ ( expect_col = actual_col );
  (* END_TEST *)

  (* BEG_TEST *)
  (* mergeall with several docs, all unique elements *)
  let entries = 
    [("test-data/first.tmp", ["Asami"; "Bolin";"Korra";"Mako"]);
     ("test-data/second.tmp", ["Pema";"Tenzin"]);
     ("test-data/third.tmp", ["Ikki";"Jinora";"Meelo";"Rohan"]);
    ] in
  let actual_col = make_doccol entries in
  let actual_merge = Bulkops.mergeall actual_col in
  let expect_merge =
    ["Asami"; "Bolin"; "Ikki"; "Jinora"; "Korra"; "Mako"; "Meelo"; "Pema"; "Rohan"; "Tenzin"]
  in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/first.tmp"; count= 3;
  curdoc = 
   { current= ["Asami"; "Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/third.tmp",
   { current= ["Ikki"; "Jinora"; "Meelo"; "Rohan"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/second.tmp",
   { current= ["Pema"; "Tenzin"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/first.tmp",
   { current= ["Asami"; "Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_mergeall_msg expect_merge actual_merge expect_col actual_col in
  __check__ ( expect_merge = actual_merge );
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);

(fun () ->
  (* BEG_TEST *)
  (* mergeall, many short docs with unique elements *)
  let entries = 
    [("test-data/x.tmp", ["Korra";]);
     ("test-data/y.tmp", ["Bolin";]);
     ("test-data/z.tmp", ["Mako"]);
     ("test-data/a.tmp", ["Asami"]);
     ("test-data/b.tmp", ["Jinora"]);
     ("test-data/c.tmp", ["Bumi"]);
     ("test-data/d.tmp", ["Kya"]);
    ] in
  let actual_col = make_doccol entries in
  let actual_merge = Bulkops.mergeall actual_col in
  let expect_merge =
    ["Asami"; "Bolin"; "Bumi"; "Jinora"; "Korra"; "Kya"; "Mako"]
  in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/x.tmp"; count= 7;
  curdoc = 
   { current= ["Korra"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/d.tmp",
   { current= ["Kya"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/c.tmp",
   { current= ["Bumi"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/b.tmp",
   { current= ["Jinora"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/a.tmp",
   { current= ["Asami"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/z.tmp",
   { current= ["Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/y.tmp",
   { current= ["Bolin"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/x.tmp",
   { current= ["Korra"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_mergeall_msg expect_merge actual_merge expect_col actual_col in
  __check__ ( expect_merge = actual_merge );
  __check__ ( expect_col = actual_col );
  (* END_TEST *)
);

(fun ()->
  (* BEG_TEST *)
  (* mergeall, many docs with redundancies *)
  let entries = 
    [("test-data/x.tmp", ["Asami";"Bolin";"Korra";"Mako"]);
     ("test-data/y.tmp", ["Bolin";"Mako"]);
     ("test-data/z.tmp", ["Mako";"Tenzin";"Zuko"]);
     ("test-data/a.tmp", ["Asami";"Hiroshi";"Lin";"Toph"]);
     ("test-data/b.tmp", ["Jinora";"Korra";"Meelo"]);
     ("test-data/d.tmp", ["Bolin";"Bumi";"Kya";"Mako"]);
     ("test-data/t.tmp", ["Amon";"Kuvira";"Unalaq";"Zaheer"]);
     ("test-data/u.tmp", ["Asami";"Iroh";"Toph";"Zuko"]);
    ] in
  let actual_col = make_doccol entries in
  let actual_merge = Bulkops.mergeall actual_col in
  let expect_merge =
    ["Amon"; "Asami"; "Bolin"; "Bumi"; "Hiroshi"; "Iroh"; "Jinora"; "Korra"; "Kuvira"; "Kya"; "Lin"; "Mako"; "Meelo"; "Tenzin"; "Toph"; "Unalaq"; "Zaheer"; "Zuko"]
  in
  let expect_col =
  (* BEG_OMIT *)
{ curname= "test-data/x.tmp"; count= 8;
  curdoc = 
   { current= ["Asami"; "Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   };
  docs = [
  ("test-data/u.tmp",
   { current= ["Asami"; "Iroh"; "Toph"; "Zuko"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/t.tmp",
   { current= ["Amon"; "Kuvira"; "Unalaq"; "Zaheer"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/d.tmp",
   { current= ["Bolin"; "Bumi"; "Kya"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/b.tmp",
   { current= ["Jinora"; "Korra"; "Meelo"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/a.tmp",
   { current= ["Asami"; "Hiroshi"; "Lin"; "Toph"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/z.tmp",
   { current= ["Mako"; "Tenzin"; "Zuko"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/y.tmp",
   { current= ["Bolin"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });

  ("test-data/x.tmp",
   { current= ["Asami"; "Bolin"; "Korra"; "Mako"];
     undo_stack= [
       
     ];
     redo_stack= [
       
     ];
   });
  ];
}
  (* END_OMIT *)
  in
  let msg = make_mergeall_msg expect_merge actual_merge expect_col actual_col in
  __check__ ( expect_merge = actual_merge );
  __check__ ( expect_col = actual_col );
  (* END_TEST *)

);

|];;    

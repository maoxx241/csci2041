(* lpe_main.ml: main routine which uses Lex_parse_eval functions to
   evaluate an expression that is passed on the command line. *)

open Buggy_lpe;;
open Printf;;

module LPE = Buggy_lpe;;        (* module alias *)

(* Printexc.record_backtrace true;; *)

(* Main function *)
let _ =
  if Array.length Sys.argv < 2 then                      (* handle command line arguments *)
    begin
      printf "usage: %s 'expression'\n" Sys.argv.(0);    (* no argument given *)
      Pervasives.exit 1;
    end;

  (* Input string from command line *)
  let input_str = Sys.argv.(1) in

  (* Lex input *)
  printf "Tokens:\n%!";
  let tokens = LPE.lex_string input_str in
  let token_str = LPE.tokenlist_string tokens in (* string version of token list, defined below *)
  printf "%s\n\n%!" token_str;

  (* Parse token list *)
  printf "Parse Tree:\n%!";
  let (expr,_) = LPE.parse_expr tokens in
  let tree_str = LPE.parsetree_string expr in (* string version of tree, defined beloe *)
  printf "%s\n%!" tree_str;

  (* Evaluate results *)
  printf "Result:\n%!";
  let result_data = LPE.eval_expr LPE.empty_varmap expr in
  let result_str = LPE.data_string result_data in
  printf "%s\n%!" result_str;
;;


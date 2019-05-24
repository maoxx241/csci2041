open Printf

exception TestFail of string*string*string;;

let __check__ cond = assert(cond);;

let major_sep () = printf "==================================================\n";;
let minor_sep () = printf "--------------------------------------------------\n";;

(* identity function *)
let ident x = x;;
(* surround a string by double quotes *)
let quote_str str = sprintf "\"%s\"" str;;

(* Convert list to a string using the provided convert function to
   stringify elements of the list *)
let list2str convert lst =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "[";
  let iterf x =
    let s = convert x in
    Buffer.add_string buf (sprintf "%s; " s);
  in
  List.iter iterf lst;
  let buflen = Buffer.length buf in
  if buflen > 2 then
    Buffer.truncate buf (buflen-2);
  Buffer.add_string buf "]";
  Buffer.contents buf;
;;


let strlist2str = list2str quote_str;;
let intlist2str = list2str string_of_int;;
let floatlist2str = list2str string_of_float;;
let boollist2str = list2str string_of_bool;;

(* Convert array to a string using the provided convert function to
   stringify elements *)
let arr2str convert lst =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "[|";
  let iterf x =
    let s = convert x in
    Buffer.add_string buf (sprintf "%s; " s);
  in
  Array.iter iterf lst;
  let buflen = Buffer.length buf in
  if buflen > 2 then
    Buffer.truncate buf (buflen-2);
  Buffer.add_string buf "|]";
  Buffer.contents buf;
;;

let strarr2str = arr2str quote_str;;
let intarr2str = arr2str string_of_int;;
let floatarr2str = arr2str string_of_float;;
let boolarr2str = arr2str string_of_bool;;


type runtype =
  | All                                 (* run all tests *)
  | Single of int                       (* run a single test *)
;;

let run_type = ref All;;                (* Controls all or singl test *)

(* Command line options for the Arg.parse *)
let options = Arg.([
  (* ("-all", Unit(fun ()-> run_type := All),      "     Run all tests (default)");   
   * ("-one",Int(fun i ->  run_type := Single(i)),"int  Run only test # given"); *)
]);;

(* Assume additional args are single tests to run *)
let handle_extra_args arg =
  let i = int_of_string arg in
  run_type := Single(i);
;;

(* Simple usage message for Arg.parse *)
let usage =
  let lines = [
      sprintf "usage: %s [options]" Sys.argv.(0);
      sprintf "       %s test#" Sys.argv.(0);
      "By default all tests are run. Provide a single integer";
      "argument to run an individual test";
    ] in
  String.concat "\n" lines
;;

(* Main routine to be called with an array of tests from other modules
   which define the tests. *)
let main (tests : (unit -> unit) array)  = 
  Printexc.record_backtrace true;
  Arg.parse options handle_extra_args usage; (* parse command line options *)

  let found_tests = Array.length tests in
  printf "Found %d tests\n" found_tests;
  
  let start_idx,stop_idx =              (* determine start/stop *)
    match !run_type with
    | All -> (1, (Array.length tests))  (* run everything *)
    | Single(i) -> (i, i)               (* run only a single test *)
  in
  let total_tests = (stop_idx-start_idx+1) in
  printf "RUNNING %d tests\n" total_tests;
  major_sep ();

  let passed = ref 0 in                 (* count number of passing tests *)
  for i=start_idx to stop_idx do        (* iterate over the tests *)
    try
      printf "Test %2d: " i;
      let test = tests.(i-1) in
      test ();                          (* run the test *)
      printf "ok\n";
      incr passed;                      (* increment # passed *)
    with except ->                      (* catch exceptions *)
      let backtrace_str =               (* grab the back *)
        Printexc.get_backtrace () in
      printf "FAIL\n";
      minor_sep ();
      begin match except with
      | TestFail(loc,msg,testcode) ->
        printf "%s\n%s\n%s\n" loc testcode msg;
      | _ ->
        let except_str = Printexc.to_string except in
        printf "Uncaught Exception: %s\nBACKTRACE:\n%s" except_str backtrace_str;
      end;
      minor_sep ();
  done;
  major_sep ();
  printf "%2d / %2d tests passed\n" !passed total_tests;
;;
    

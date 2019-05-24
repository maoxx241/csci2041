open Printf

(* Flip to true to show full actual output for output based
   tests. Makes it easier to generate tests. *)
let full_actual = true;;

(* Testfail exception: reports information on a failed test. *)
exception TestFail of {
    loc:  string; (* location of failed check as in sourcefile.ml:19 *)
    msg:  string; (* msg:  error message to print on failure *)
    code: string; (* code: source code to print out *)
};;

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


(* String conversions for lists *)
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

(* String conversions for arrays *)
let strarr2str = arr2str quote_str;;
let intarr2str = arr2str string_of_int;;
let floatarr2str = arr2str string_of_float;;
let boolarr2str = arr2str string_of_bool;;

let rec map2opt func alist blist =
  match alist,blist with
  | [],[]         -> []
  | ah::at,[]     -> (func (Some ah) None)     ::(map2opt func at [])
  | [],bh::bt     -> (func None (Some bh))     ::(map2opt func [] bt)
  | ah::at,bh::bt -> (func (Some ah) (Some bh))::(map2opt func at bt)

(* run given thunk () and save any output to filename *)
let save_stdout filename thunk =
  flush stdout;  
  let oldstdout = Unix.dup Unix.stdout in
  let newstdout = open_out filename in
  Unix.dup2 (Unix.descr_of_out_channel newstdout) Unix.stdout;
  thunk ();
  flush stdout;
  Unix.dup2 oldstdout Unix.stdout;
;;

(* Read entire contents of a file and return a string of those contents *)
let slurp filename =
  let inchan = open_in filename in
  let len = in_channel_length inchan in
  let bytes = Bytes.create len in
  let readlen = input inchan bytes 0 len in
  if len != readlen then
    eprintf "Warning: reading file '%s', expected %d bytes but got %d\n" filename len readlen;
  Bytes.to_string bytes
;;

(* print a string the given filename *)
let dump string filename =
  let outchan = open_out filename in
  output_string outchan string;
  close_out outchan;
;;

(* Runs a side-by-side diff on the expect_file and actual_file and
   stores the result in diff_file.  If the files match, returns true.
   If the files don't match, returns false and sets msgref to an error
   message showing the results.

   These options should work on most Unices and Mac OS X

   diff -bBy: 
     -b ignore whitespace
     -B ignore blank lines
     -y do side-by-side comparison 
*)
let check_diff expect_file actual_file diff_file msgref =
  let cmd = sprintf "diff -bBy %s %s > %s" expect_file actual_file diff_file in
  let retcode = Sys.command cmd in
  if retcode = 0 then
    true
  else
    begin
      let lines =
        String.concat "\n"
          (["Expected / Actual Output Mismatch";
            "Expect file: "^expect_file;
            "Actual file: "^actual_file;
            "";
            "Side-by-side diff below";
            "  LEFT  COLUMN:   Expected Ouptut";
            "  RIGHT COLUMN:   Actual Ouptut";
            "  MIDDLE SYMBOLS: Indicate lines with differences";
            "";
            "EXPECT\t\t\t\t\t\t\t\tACTUAL";
            slurp diff_file;
           ] @
             if full_actual then
               ["---";
                "FULL ACTUAL";
                slurp actual_file;
               ]
             else
               []
          )
      in
      msgref := lines;
      false
    end    
;;

(* Same as check_diff except that the expect_string is passed and put
   into the expect_file. *)
let check_diff_expect expect_string expect_file actual_file diff_file msgref =
  dump expect_string expect_file;
  check_diff expect_file actual_file diff_file msgref
;;

(* Same as check_diff except that the expect/actual are both strings
   passed and put into files. *)
let check_diff_str expect_string expect_file actual_string actual_file diff_file msgref =
  dump expect_string expect_file;
  dump actual_string actual_file;
  check_diff expect_file actual_file diff_file msgref
;;

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
      (* | TestFail(loc,msg,testcode) -> *)
      | TestFail{loc=loc; msg=msg; code=code}->
        printf "%s\n%s\n%s\n" loc code msg;
      | _ ->
        let except_str = Printexc.to_string except in
        printf "Uncaught Exception: %s\nBACKTRACE:\n%s" except_str backtrace_str;
      end;
      minor_sep ();
  done;
  major_sep ();
  printf "%2d / %2d tests passed\n" !passed total_tests;
;;
    

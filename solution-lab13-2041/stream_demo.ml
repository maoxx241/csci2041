(* stream_demo.ml: show basics of using Streams to convert immutable
   data or stateful functions into a source of stream data.  *)

open Printf;;

let _ = 
  printf "=====finite stream from list=====\n";
  let crew_list =
    ["Mal"; "Zoe"; "Wash"; "Kaylee"; "Book";
     "Jayne"; "Inara"; "Simon"; "River"]
  in

  let crew_stream = Stream.of_list crew_list in

  (* crew_stream loop *)
  while (Stream.peek crew_stream) <> None do  (* loop over crew_stream  *)
    printf "%d: " (Stream.count crew_stream);
    printf "List.hd: %s  " (List.hd crew_list);
    printf "Stream.next: %s\n" (Stream.next crew_stream);
  done;

  printf "=====infinite int stream from function=====\n";

  let two_pows = 
    let cur = ref 1 in                        (* local "private" ref *)
    fun i -> let c = !cur in                  (* anonymous function, i is not used *)
             cur := !cur*2;                   (* double private local *)
             Some c                           (* return something *)
  in

  let twop_stream = Stream.from two_pows in   (* create a stream from two_pows *)

  (* twop_stream loop *)
  printf "<enter>: advance  <Ctrl-c>: exit\n";
  while (Stream.peek twop_stream) <> None do
    printf "%d: " (Stream.count twop_stream);
    printf "%d%!" (Stream.next twop_stream);
    ignore (read_line ());                    (* wait for <enter> to be pressed *)
  done;


;;

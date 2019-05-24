open Printf;;
let _ =
  let quit_now = ref false in
  while !quit_now = false do                (* loop until quit command is issued *)
    printf "Type anything ('q' to quit)> ";
    let line = read_line () in              (* read a line of input from stdin *)
    if line="q" then
      quit_now := true
    else
      begin
        Counter.increment ();
        let count = Counter.current () in
        printf "%d lines typed\n" count;
      end;
  done;
  printf "Bye!\n";
;;


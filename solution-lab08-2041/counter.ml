let the_count = ref 0;;
let increment () =
  the_count := !the_count + 1;
;;
let decrement () =
  the_count := !the_count - 1;
;;
let current () =
  !the_count
;;

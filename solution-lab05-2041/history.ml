open Imu_stack;;                      (* make/push are Imu_stack versions *)

let imu_history =                     (* create a history of stack states *)
  let empty = make () in              (* empty initial stack *)
  let hist = Array.make 11 empty in   (* array of historical stacks *)
  for i=1 to 10 do                    (* loop over array *)
    let prev_stack = hist.(i-1) in    (* get previous stack *)
    hist.(i) <- push prev_stack i;    (* assign to result of pushing *)
  done;
  hist                                (* imu_history gets array hist *)
;;

open Mut_stack;;                      (* make/push are Mut_stack versions *)

let mut_history =                     (* make/push are Mut_stack ops *)
  let empty = make () in              (* empty initial stack *)
  let hist = Array.make 11 empty in   (* array of historical stacks *)
  for i=1 to 10 do                    (* loop over array *)
    let prev_stack = hist.(i-1) in    (* get previous stack *)
    push prev_stack i;                (* push, unit return type *)
  done;
  hist                                (* mut_history gets array of hist *)
;;

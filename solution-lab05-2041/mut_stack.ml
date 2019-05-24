(* type for a mutable stack *)
type 'a mut_stack = {
  mutable size : int;           (* # elements in stack *)
  mutable data : 'a list;       (* elements in stack, top-first *)
};;

let make () =                   (* make an empty stack *)
  {size=0; data=[]}
;;

let is_empty stack =            (* determine if a stack is empty *)
  stack.size = 0
;;

let push stack elem =           (* push a element into the stack *)
  stack.size <- stack.size+1;
  stack.data <- elem::stack.data;
;;

let pop stack =                  (* pop and element off the stack *)
  match stack.data with          (* match the field data which is a list *)
  | [] -> failwith "empty stack" (* thrown an exceptio if empty *)
  | top::rest ->                 
     stack.size <- stack.size-1; (* otherwise decrease size and *)
     stack.data <- rest;         (* remove the element *)
;;

let top stack =                  (* return the top element of the stack *)
  match stack.data with
  | [] -> failwith "empty stack" (* exception if empty *)
  | top::rest -> top             (* otherwise top element *)
;;

let poptop stack =               (* pop the top element and return it *)
  match stack.data with
  | [] -> failwith "empty stack"
  | top::rest ->                 (* bind top to top element *)
     pop stack;                  (* pop off using function *)
     top                         (* return top element *)
;;

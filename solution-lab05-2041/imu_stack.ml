(* type for an immutable stack. Most operations return a new version
   of the stack which has its state altered. However, the original
   stack is unchanged. *)
type 'a imu_stack = {
  size : int;           (* # elements in stack *)
  data : 'a list;       (* elements in stack, top-first *)
};;

let make () =                   (* make an empty stack *)
  {size=0; data=[]}
;;

let is_empty stack =            (* determine if a stack is empty *)
  stack.size = 0
;;

let push stack elem =           (* return a new stack with elem pushed *)
  {size = stack.size+1;         (* new stack with larger size *)
   data = elem::stack.data}     (* and elem at the top *)
;;

let pop stack =                  (* return a NEW stack with top element popped *)
  match stack.data with          (* match the field data which is a list *)
  | [] -> failwith "empty stack" (* thrown an exceptio if empty *)
  | top::rest ->                 
     {size = stack.size-1;       (* new stack with decreased size *)
      data = rest}               (* and top element removed *)
;;

let top stack =                  (* return the top element of the stack *)
  match stack.data with
  | [] -> failwith "empty stack" (* exception if empty *)
  | top::rest -> top             (* otherwise top element *)
;;

let poptop stack =               (* return a pair: NEW stack with top popped and *)
  match stack.data with          (* the top element *)
  | [] -> failwith "empty stack"
  | top::rest ->                 (* bind top to top element *)
     ((pop stack),top)           (* pair of popped stack and top element *)
;;

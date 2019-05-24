
let curr_list : string list ref = ref [];;
(* The current list of strings. *)

let undo_stack : string list list ref =  ref  [];;
(* List of previous curr_lists to enable undo. *)

let redo_stack : string list list ref = ref [];;
(* List of undone curr_lists to enable redo. *)

let reset_all () =
    redo_stack := [];
    undo_stack := [];
    curr_list := [];;
(* Reset curr_list, undo_stack, redo_stack to all be empty lists. Used
   only in testing, not in main programs. *)

let set_to_list new_list =
    undo_stack :=  ((!curr_list):: (!undo_stack));
    curr_list := new_list;
    redo_stack := [];;
(* curr_list is moved to the top of the undo_Stack. Then curr_list is
   set to the new_list. Empties redo_stack. *)

let add_elem elem =
    let new_list = Sortedlist.insert !curr_list elem in
    set_to_list new_list;;
(* Add the given elem to curr_list producing a new_list.  Calls
   set_to_list on result to unable undoing. *)

let remove_elem elem =
    let new_list = Sortedlist.remove !curr_list elem in
    set_to_list new_list;;
(* Remove the given elem from curr_list producing a new list. Calls
   set_to_list on result to unable undoing.  *)

let merge_with_list list =
    let new_list = Sortedlist.merge list !curr_list in
    set_to_list new_list;;
(* Merge the param list with the current list. Calls set_to_list on
   the result to enable undoing. *)

let undo () =
    if undo_stack = ref [] then false else let func()=
    redo_stack :=  ((!curr_list):: (!redo_stack));
    curr_list := List.hd !undo_stack;
    undo_stack := List.tl !undo_stack;
    true;
    in func();;
(* If the undo_stack is not empty, undo the last operation. curr_list
   is moved to the redo_stack and the top element of undo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if undo_stack is empty and no changes are made. Operates
   in constant time. *)

let redo () =
    if redo_stack = ref[] then false else let func ()=
    undo_stack :=  ((!curr_list):: (!undo_stack));
    curr_list := List.hd !redo_stack ;
    redo_stack :=  (List.tl !redo_stack );
    true ;
    in func();;

(* document.ml: Defines a "document" type which tracks a current state
   along with undo/redo history.  The document type is polymorphic
   meaning data can be any type such as 'int document' or 'string list
   document'. *)

(* document type declaration. *)
type 'a document = {
    mutable current    : 'a ;       (* current document state *)
    mutable undo_stack : 'a list;   (* past states that can be reached via undo *)
    mutable redo_stack : 'a list;   (* undone states that can be reached via redo *)
};;

let make initial = {
    current= initial;
    undo_stack = [];
    redo_stack = [];
};;
(* val make : 'a -> 'a document
   Create a new document with initial as the current state and empty
   undo/redo stacks. *)

let set document data =
    document.undo_stack <- document.current :: document.undo_stack;
    document.current <- data;
    document.redo_stack <-[];
;;
(* val set : 'a document -> 'a -> unit
   Set the document to the given data. Push current state into the
   undo stack. Empty the redo stack *)

let undo document =
    match document.undo_stack with
    | [] -> false;
    | top::rest ->
        document.redo_stack <- document.current :: document.redo_stack;
        document.current <- top;
        document.undo_stack <- rest;
        true;
        ;;
(* val undo : 'a document -> bool
   If the undo_stack is not empty, undo the last operation. current
   is moved to the redo_stack and the top element of undo_stack is
   removed and becomes current.  Returns true if changes are made
   and false if undo_stack is empty and no changes are made. Operates
   in constant time. *)

let redo document =
    match document.redo_stack with
    | [] -> false;
    | top::rest ->
        document.undo_stack <- document.current :: document.undo_stack;
        document.current <- top;
        document.redo_stack <- rest;
        true;
        ;;
(* val redo : 'a document -> bool
   If the redo_stack is not empty, redo the last operation. current
   is moved to the undo_stack and the top element of redo_stack is
   removed and becomes current.  Returns true if changes are made
   and false if redo_stack is empty and no changes are made. Operates
   in constant time. *)

(* strtree.ml: beginnings of a persistent binary tree which tracks
   unique strings. *)

open Printf;;

(* Algebraic type for a persistent binary search tree of strings. *)
type strtree =
  | Empty               (* no data: bottom of tree  *)
  | Node of {           (* node of anonymous record *)
      data  : string;   (* data at this node        *)
      left  : strtree;  (* left branch              *)
      right : strtree;  (* right branch             *)
    }
;;

(* val add : strtree -> string -> strtree
   let newtree = add tree str in ...

   Add to the given tree using the standard left/less-than
   right/greater than binary tree convention. Duplicates are not
   added. Trees are persistent: adding creates a new tree distinct
   from the original*)
let rec add tree str =
  match tree with
  | Empty ->                                        (* bottom of tree: didn't find *)
     Node{data=str; left=Empty; right=Empty}        (* str so make a new node *)
  | Node(node) ->                                   (* at a node *)
     let diff = String.compare str node.data in     (* compute a difference *)
     if diff = 0 then                               (* 0 indicates equal *)
       tree                                         (* return current tree *)
     else if diff < 0 then                          (* negative indicates str less than data *)
       let left_result = add node.left str in       (* compute result for adding on the left *)
       Node{node with left=left_result}             (* create a new node with the new left branch *)
     else                                           (* positive indicates str greater than data *)
       Node{node with right=(add node.right str)}   (* terse version of two-step left add *)
;;

(* val tree_string : tree -> string
   let s = tree_string tree in ...

   Use a Buffer (extensible string) and an in-order traversal to
   construct a string version of a tree. Nodes are indented according
   to their depth: root is left-most with children farther to the
   right. Each data element is preceded by its integer
   depth. Left-most elements appear at the top while right-most
   elements appear at the bottom. This means elements read in order
   from top to bottom. Example:

       2: Bullet Bill
         3: Chain Chomp
     1: Donkey Kong
       2: Luigi
   0: Mario
     1: Princess
       2: Toad
*)
let tree_string tree =
  let buf = Buffer.create 256 in                    (* extensibel character buffer *)

  let rec build tree depth =                        (* recursive helper *)
    match tree with
    | Empty -> ()                                   (* out of tree, done with this branch *)
    | Node(node) ->                                 (* have a node *)
       build node.left (depth+1);                   (* recurse on left branch first *)
       for i=1 to depth do                          (* indent according to depth of this node *)
         Buffer.add_string buf "  ";
       done;
       let datastr =                                (* string with depth and data  *)
         sprintf "%2d: %s\n" depth node.data
       in
       Buffer.add_string buf datastr;               (* add to buffer *)
       build node.right (depth+1);                  (* recurse on right branch *)
  in                                                (* end helper *)

  build tree 0;                                     (* recurse from root *)
  Buffer.contents buf                               (* return string from Buffer *)


(* val getopt : strtree -> string -> string option
   let opt = getopt tree str in ...

   Search tree for string str. Return None if str is not in the tree
   and return (Some str) if it is found in tree.
*)
let rec getopt tree str =
  ()
;;

(* val contains : strtree -> string -> bool
   let present = contains tree str in ...

   Returns true if str is present in strtree and false
   otherwise. Uses funciton getopt for this.
*)
let contains tree str =
  ()
;;

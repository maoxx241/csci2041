(* treemap.ml: Provides a functor to create a map from an arbitrary
   key type to an arbitrary value type. Functor Make requires a module
   input matching the KEYVAL_SIG signature.  The resulting module
   has the same functions as Ssmap but that works for maps with the
   specified key/value types.  As the name implies, a BST is the
   underlying data structure for the map.
*)

(* Type of key/vals that go into Treemaps. Includes key and value
   type, key comparison function, and a string function for easy
   display. *)
module type KEYVAL_SIG =
sig
  type key_t
  type value_t
  val compare_keys : key_t -> key_t -> int
  val keyval_string : key_t -> value_t -> string
end

(* Functor which creates a module for maps with key/value types
   specified by the parameter module KVMod. *)
module Make (KVMod : KEYVAL_SIG) :
sig
  type treemap =
      Empty
    | Node of { 
        key : KVMod.key_t;
        value : KVMod.value_t;
        left : treemap;
        right : treemap;
     }

  val empty : treemap

  val add : treemap -> KVMod.key_t -> KVMod.value_t -> treemap

  val tree_string : treemap -> string

  val getopt : treemap -> KVMod.key_t -> KVMod.value_t option

  val contains_key : treemap -> KVMod.key_t -> bool

  val iter : (KVMod.key_t -> KVMod.value_t -> 'a) -> treemap -> unit

  val fold : ('a -> KVMod.key_t -> KVMod.value_t -> 'a) -> 'a -> treemap -> 'a

  val to_string : treemap -> string

  val findmin_keyval : treemap -> KVMod.key_t * KVMod.value_t

  val remove_key : treemap -> KVMod.key_t -> treemap
 
end = struct 
type treemap =
      Empty
    | Node of { 
        key : KVMod.key_t;
        value : KVMod.value_t;
        left : treemap;
        right : treemap;
     }

    let empty = Empty;;

    let rec add map key value =
  match map with
  | Empty ->                                             (* bottom of tree: didn't find *)
     Node{key=key; value=value;                          (* make a new node with key/val binding *)
          left=Empty; right=Empty}
  | Node(node) ->                                        (* at a node *)
     let diff = KVMod.compare_keys key node.key in           (* compute a difference *)
     if diff = 0 then                                    (* 0 indicates equal *)
       Node{node with value=value}                       (* replace value binding with new value *)
     else if diff < 0 then                               (* negative indicates str less than data *)
       Node{node with left=(add node.left key value)}    (* create a new node with new left branch *)
     else                                                (* positive indicates str greater than data *)
       Node{node with right=(add node.right key value)}  (* create new node with new right branch *)
;;

(* val tree_string : ssmap -> string

   let s = tree_string map in ...

   Use a Buffer (extensible string) and a right-to-left in-order
   traversal of the internal tree to construct a string version of the
   map. Nodes are indented according to their depth: root is left-most
   with children farther to the right. Each data element is preceded
   by its integer depth. Right-most elements appear at the top while
   left-most elements appear at the bottom. This means elements read
   in REVERSE order from top to bottom and that mentally rotating the
   tree clockwise gives appropriate left/right branch positions.  
*)
let tree_string map =
  let buf = Buffer.create 256 in                    (* extensibel character buffer *)

  let rec build tree depth =                        (* recursive helper *)
    match tree with
    | Empty -> ()                                   (* out of tree, done with this branch *)
    | Node(node) ->                                 (* have a node *)
       build node.right (depth+1);                  (* recurse on right branch *)
       for i=1 to depth do                          (* indent according to depth of this node *)
         Buffer.add_string buf "  ";
       done;
       let datastr =                                (* string with depth and data  *)
         (Printf.sprintf "%2d: " depth)^(KVMod.keyval_string node.key node.value)^(Printf.sprintf "\n")
       in
       Buffer.add_string buf datastr;            (* add to buffer *)
       build node.left (depth+1);                   (* recurse on left branch *)
  in                                                (* end helper *)

  build map 0;                                      (* recurse from root *)
  Buffer.contents buf                               (* return string from Buffer *)


(* IMPLEMENT

   val getopt : ssmap -> string -> string option

   let opt = getopt map key in ...

   Search the map for given key. If found, return the Some of the
   associated value. Otherwise return None.
*)
let rec getopt map key =
  match map with
  |Empty ->None
  |Node(node) ->
  let diff = KVMod.compare_keys key node.key in          
     if diff = 0 then                                   
       Some node.value                       
     else if diff < 0 then                               
       getopt node.left key 
     else                                               
       getopt node.right key  

;;

(* IMPLEMENT

   val contains_key : ssmap -> string -> bool

   let present = contains_key map key in ...

   Returns true if key is present in the map and false
   otherwise. Uses function getopt.
*)
let contains_key map str =
  if getopt map str <> None then true else false;
;;


(* IMPLEMENT

   val iter : (string -> string -> unit) -> ssmap -> unit

   let func key value = ... in

   fold func map;

   Apply func to all elements key/value pairs in the map for
   side-effects. Keys are visited in sorted order from minimum to
   maximum. Works as other iter-like functions such as List.iter
   or Array.iter.  
*)
let rec iter func map =
   match map with
  |Empty -> ()
  |Node(node) ->
  iter func node.left;                                    
       func node.key node.value;                                                                                             
       iter func node.right;
;;

(* IMPLEMENT

   val fold : ('a -> string -> string -> 'a) -> 'a -> ssmap -> 'a

   let func cur key value = ... in

   let reduction = fold func init map in ...

   Apply func to all elements key/value pairs in the map to produce a
   single value at the end. Keys are visited in sorted order from
   minimum to maximum. Works as other fold-like functions such as
   List.fold_left or Array.fold_left.  
*)
let rec fold func cur map =
  match map with
  |Empty -> cur
  |Node(node) ->
  let cur_= func (fold func cur node.left) (node.key) (node.value) in
  fold func cur_ node.right;
  
;;
  

(* IMPLEMENT

   to_string : ssmap -> string

   let displaystr = to_string map in ...

   Produces a string representation of the map. The format is

   "[{k1 -> v1}, {k2 -> v2}, {k3 -> v4}]"

   Key/vals appear from minimum key to maximum key in the output
   string. Functionality from the Buffer module is used with an
   in-order traversal to produce the string efficiently. May make use
   of a higher-order map function such as iter or fold.
*)
let to_string map =                                 (* verbose version: no use of iter *)
  let cur=Buffer.create 20 in
  let func key value = Buffer.add_string cur ((KVMod.keyval_string key value)^(Printf.sprintf ", ")) in
  iter func map ;
  if Buffer.length cur >0 then Buffer.truncate cur ((Buffer.length cur)-2) ;
  "["^Buffer.contents cur^"]"
;;  

(* IMPLEMENT

   val findmin_keyval : ssmap -> (string * string)

   let (minkey,minval) = findmin ssmap in ...

   Find the "minimum" key in the given map and return it and its value
   as a pair. If used with an empty map, fails with exception message
   "No minimum in an empty tree".  Since the map is based on a BST,
   the minimum key is at the left-most node.  
*)
let rec findmin_keyval map =
  match map with
  |Empty -> Pervasives.failwith "No minimum in an empty tree"
  |Node(node) ->                                   
       if node.left = Empty then (node.key,node.value)else                                          
       findmin_keyval node.left;                                         
       
;;

(* IMPLEMENT

   val remove_key : ssmap -> string -> ssmap

   let newmap = remove map key in ...

   Returns a new map without key in it.  The internal tree is
   re-arranged according to standard BST conventions: (1) If key is a
   leaf, it is replaced with Empty, (2) If key is an internal node
   with one child, it's left or right branch replaces it, (3) If key
   is an internal node with both left/right children, it successor
   replaces it. Makes use of findmin_keyval to locate a succesor as
   the minimum of on the right child branch.
*)
let rec remove_key map key =
  match map with
  | Empty -> map
  | Node(node) ->                                   
     let diff = KVMod.compare_keys key node.key in
     if diff <0 then Node{node with left=(remove_key node.left key )} 
     else if diff > 0 then Node{node with right=(remove_key node.right key )} 
     else                                
       if node.left = Empty && node.right = Empty then Empty else
       if node.left <>Empty && node.right = Empty then node.left else
       if node.left = Empty && node.right <> Empty then node.right else
       (* if node.left <>Empty && node.right <> Empty then  *)
       let (new_key,new_value) = findmin_keyval node.right 
       in 
       Node{key=new_key;
       value=new_value;
       left=node.left;
       right=(remove_key node.right new_key )}       
                          
       
;;
end
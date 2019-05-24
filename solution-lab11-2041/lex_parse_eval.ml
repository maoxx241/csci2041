(* Tue Nov 20 10:58:17 CST 2018 :: Corrected version of
   parsetree_string *)

(* Mon Nov 19 10:22:07 CST 2018 :: Bug fix to establish higher
   precedence for subtraction and division. *)

(* lex_parse_eval.ml:  *)
open Printf;;

(**********************************************************************************)
(* Lexer *)

(* algebraic types for tokens: lexing results *)
type token =
  | Int of int
  | Bool of bool
  | Ident of string 
  | OParen | CParen 
  | Plus  | Times | Minus | Slash
  | Let | In | Equal
  | If | Then | Else 
;;


(* true if the given character is a digit 0-9 and false otherwise *)
let is_digit c =
  let digits = "0123456789" in
  let loc = String.index_opt digits c in
  loc <> None
;;

(* true if character is letter a-z or A-Z, false otherwise *)
let is_letter c =
  let letters = "abcdefghijklmnopqrstuvwxyz" in
  let lower = Char.lowercase_ascii c in
  let loc = String.index_opt letters lower in
  loc <> None
;;


(* create a list of tokens *)
let lex_string string =         
  let len = String.length string in
  let rec lex pos =                          (* recursive helper *)
    if pos >= len then                       (* off end of string ? *)
      []                                     (* end of input *)
    else                                     (* more to lex *)
      match string.[pos] with                (* match a single character *)
      |' ' | '\t' | '\n' -> lex (pos+1)      (* skip whitespace *)
      |'+' -> Plus   :: (lex (pos+1))        (* single char ops become operators *)
      |'-' -> Minus  :: (lex (pos+1))        
      |'*' -> Times  :: (lex (pos+1))
      |'/' -> Slash  :: (lex (pos+1))
      |'(' -> OParen :: (lex (pos+1))        (* and open/close parens *)
      |')' -> CParen :: (lex (pos+1))
      |'=' -> Equal  :: (lex (pos+1))
      | d when is_digit d ->                 (* see a digit *)
         let stop = ref pos in               (* scan through until a non-digit is found *)
         while !stop < len && is_digit string.[!stop] do
           incr stop;
         done;
         let numstr = String.sub string pos (!stop - pos) in (* substring is the int *)
         let num = int_of_string numstr in   (* parse the integer *)
         Int(num) :: (lex !stop)             (* and tack onto the stream of tokens *)
      | a when is_letter a ->                (* see a letter *)
         let stop = ref pos in               (* scan through until a non-letter is found *)
         while !stop < len && is_letter string.[!stop] do
           incr stop;
         done;
         let ident = String.sub string pos (!stop - pos) in (* substring is the identifier *)
         let tok = 
           match ident with                  (* look for reserved keywords *)
           | "true"  -> Bool true
           | "false" -> Bool false
           | "let"   -> Let
           | "in"    -> In
           | "if"    -> If
           | "then"  -> Then
           | "else"  -> Else
           | _ -> Ident(ident)               (* not reserved, return an identifier *)
         in
         tok :: (lex !stop)                  (* and tack onto the stream of tokens *)
      | _ ->                                 (* any other characters lead to failures *)
         let msg = sprintf "lex error at char %d, char '%c'" pos string.[pos] in
         failwith msg
  in                                         (* end helper *)
  lex 0                                      (* call helper *)
;;

(**********************************************************************************)
(* Parser *)

(* exception type for parsing error *)
exception ParseError of {
    msg  : string;
    toks : token list;
};;

(* algebraic types for expression tree: parsing results *)
type expr =
  | IConst of int                             (* integer data *)
  | BConst of bool                            (* boolean data *)
  | Varname of string                         (* variable name that should be looked up *)
  | Add of expr * expr                        (* arithmetic operators *)
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Letin of {var_name: string;               (* let expression binding a var_name *)
              var_expr: expr;                 (* to resutls of var_expr then evaluating *)
              in_expr:  expr;}                (* in_expr to get results *)
  | Cond of {if_expr: expr;                   (* if/then/else, expressions associated with each  *)
             then_expr: expr;                 (* when if_expr is true,  eval then_expr *)
             else_expr: expr;}                (* when if_expr is false, eval else_expr *)
;;

(* Top-level entry for recursive descent parser: create an expression
   tree from a series of tokens. Starts a series of mutually recursive
   functions. *)
let rec parse_expr tokens =
  let (expr, rest) as result = parse_add tokens in
  result

(* parse addition and subtraction *)
and parse_add toks =
  let (lexpr, rest) = parse_sub toks in       (* try higher precdence expression first *)
  match rest with
  | Plus :: tail ->                           (* + is first *)
     let (rexpr,rest) = parse_add tail in     (* recursively generate right-had expression *)
     (Add(lexpr,rexpr), rest)                 (* return addition of these two *)
  | _ -> (lexpr, rest)                        (* not an addition so return expression and remaining tokens *)

and parse_sub toks =
  let (lexpr, rest) = parse_mul toks in       (* try higher precdence expression first *)
  match rest with
  | Minus :: tail ->                          (* - is first *)
     let (rexpr,rest) = parse_sub tail in     (* recursively generate right-had expression *)
     (Sub(lexpr,rexpr), rest)                 (* return subtraction of these two *)
  | _ -> (lexpr, rest)                        (* not subtraction so return expression and remaining tokens *)

(* parse multiplication and division *)
and parse_mul toks =
  let (lexpr, rest) = parse_div toks in     (* try higher precdence expression first *)
  match rest with
  | Times :: tail ->                          (* * is first *)
     let (rexpr,rest) = parse_mul tail in     (* recursively generate right-had expression *)
     (Mul(lexpr,rexpr), rest)                 (* return multiplication of these two *)
  | _ -> (lexpr, rest)                        (* not a multiply so return expression and remaining tokens *)

and parse_div toks =
  let (lexpr, rest) = parse_letin toks in     (* try higher precdence expression first *)
  match rest with
  | Slash :: tail ->                          (* / is first *)
     let (rexpr,rest) = parse_div tail in     (* recursively generate right-had expression *)
     (Div(lexpr,rexpr), rest)                 (* return division of these two *)
  | _ -> (lexpr, rest)                        (* not division so return expression and remaining tokens *)

(* parse a let/in expression *)
and parse_letin toks =
  match toks with
  | Let :: Ident var_name :: Equal :: rest ->          (* look for a sequence of 'let name = ...' *)
     begin 
       let (var_expr,rest) = parse_expr rest in        (* parse the rest of the expression *)
       begin match rest with
       | In :: rest ->                                 (* check that it ends with an 'in' *)
          let (in_expr,rest) = parse_expr rest in      (* parse the 'in <expr>' *)
          (Letin{var_name; var_expr; in_expr}, rest)   (* return result and rest of tokens *)
       | _ -> raise (ParseError{msg="Expected 'in' after 'let'"; toks=rest})
       end
     end
  | _ -> parse_cond toks                               (* didn't find 'let', recurse lower *)

(* parse an if/then/else experssion *)
and parse_cond toks =
  match toks with
  | If :: rest ->                                          (* look for an 'if <expr>' *)
     let (if_expr,rest) = parse_expr rest in               (* parse the <expr> *)
     begin match rest with                             
      | Then :: rest ->                                    (* look for 'then <expr>'  *)
         let (then_expr,rest) = parse_expr rest in         (* parse the <expr> *)
         begin match rest with
          | Else :: rest ->                                (* look for an 'else <expr>' *)
             let (else_expr,rest) = parse_expr rest in     (* parse the <expr> *)
             (Cond{if_expr; then_expr; else_expr}, rest)   (* return the result and rest of tokens *)
          | _ -> raise (ParseError{msg="Expected 'else' ";toks=rest})
         end
      | _ -> raise (ParseError{msg="Expected 'then' ";toks=rest})
     end
  | _ -> parse_ident toks

(* parse identifiers, integers, booleans, and open/close parentheses *)
and parse_ident toks =
  match toks with
  | []              -> raise (ParseError {msg="expected an expression"; toks=toks})
  | Int n   :: tail -> (IConst(n),tail)
  | Bool b  :: tail -> (BConst(b),tail)
  | Ident s :: tail -> (Varname(s),tail)
  | OParen  :: tail -> begin                   (* parenthesized expresion *)
      let (expr,rest) = parse_expr tail in     (* start back at highest precedence *)
      match rest with
      | CParen::tail -> (expr,tail)
      | _ -> raise (ParseError {msg="unclosed parentheses"; toks=tail})
    end
  | _ -> raise (ParseError {msg="syntax error"; toks=toks})
;;

(**********************************************************************************)
(* Evaluator *)

(* Algebraic type for evaluation results and variable bindings *)
type varval_t =
  | Int  of int                 (* integers and *)
  | Bool of bool                (* booleans *)
;;

(* Module to allow variable bindings with 
     Varmap.add varname varval varmap
   and data retrieval using
     Varmap.find_opt varname varmap
*)
module Varmap = Map.Make(String);; (* string keys for Map module *)

(* Type for a map from string variable name to data of Int or Bool *)
type varmap_t = varval_t Varmap.t;;

(* Empty variable map *)
let empty_varmap : varmap_t = Varmap.empty;;

(* Create a string version of the given data for printing. *)
let data_string data = 
  match data with
  | Int(i)  -> sprintf "Int(%d)"  i
  | Bool(b) -> sprintf "Bool(%b)" b
;;

(* Exception type raised during evaluation on errors  *)
exception EvalError of {
    msg    : string;            (* message indicating problem *)
};;

(* Evaluate an expression tree using the bindings provided in varmap *)
let rec eval_expr varmap expr =
  match expr with
  | IConst i -> Int i                                      (* ints/bools are self-evaluating *)
  | BConst b -> Bool b
  | Varname varname -> begin
      match Varmap.find_opt varname varmap with            (* look up the variable in the varmap *)
      | Some d -> d                                        (* found variable binding - return the value *)
      | None ->                                            (* no binding found - error *)
         let msg = sprintf "No variable '%s' bound" varname in
         raise (EvalError{msg})
    end 
  | Add(lexpr,rexpr) | Sub(lexpr,rexpr)                    (* arithmetic expressions: each has *)
  | Mul(lexpr,rexpr) | Div(lexpr,rexpr) -> begin           (* a left/right branch *)
      let ldata = eval_expr varmap lexpr in                (* evaluate left *)
      let rdata = eval_expr varmap rexpr in                (* evaluate right *)
      match ldata,rdata with
      | (Int li),(Int ri) -> begin                         (* check that the results on both sides are ints *)
          match expr with                                  (* match to perform the correct operation *)
          | Add _   -> Int (li + ri)
          | Sub _   -> Int (li - ri)
          | Mul _   -> Int (li * ri)
          | Div _   -> Int (li / ri)
          | _ -> failwith "impossible case (right?)"       (* keep the compiler happy about pattern matching *)
        end
      | Int li,rerr ->                                     (* error: left/right expression was not an int *)
         let msg = sprintf "Expect Int for right arithmetic expression, found '%s'" (data_string rerr) in
         raise (EvalError{msg})
      | lerr,_ ->
         let msg = sprintf "Expect Int for right arithmetic expression, found '%s'" (data_string lerr) in
         raise (EvalError{msg})
    end
  | Letin(l) ->                                            (* eval a let binding *)
     let var_data = eval_expr varmap l.var_expr in         (* evaluate the let-expr to determine the variable's value *)
     let new_varmap =                                      
       Varmap.add l.var_name var_data varmap               (* add the binding to the variable map *)
     in
     eval_expr new_varmap l.in_expr                        (* evaluate the in expression with the variable bound *)

  (* COMPLETE THIS CASE *)
  | Cond(c) -> begin                                       (* eval a condition *)
      let test = eval_expr varmap c.if_expr in             (* evaluate the test, 'if <expr>'  *)
      match test with                                      
      | Bool b ->                                          (* ensure result of <expr> was a true/false value  *)
         if b = true then                                  
           eval_expr varmap c.then_expr                    (* true: do the 'then <expr>'  *)
         else
           eval_expr varmap c.else_expr                    (* false: do the 'else <expr>' *)
      | _ ->                                               (* error: 'if <expr>' did not give a true/false *)
         let msg = sprintf "Expected Bool for if <expr>, found '%s'" (data_string test) in
         raise (EvalError{msg})
    end
;;

(**********************************************************************************)
(* To-string functions for tokens and expression trees *)

(* Create a string of a list of tokens *)
let tokenlist_string tokens =
  let buf = Buffer.create 256 in                    (* extensible character buffer *)
  Buffer.add_string buf "[";
  let toks = ref tokens in
  let count = ref 0 in
  while !toks <> [] do
    if !count > 0  && !count mod 10 = 0 then
      Buffer.add_string buf "\n ";
    let str = match List.hd !toks with
      | Plus     -> "Plus"
      | Times    -> "Times"
      | Minus    -> "Minus"
      | Slash    -> "Slash"
      | Equal    -> "Equal"
      | OParen   -> "OParen"
      | CParen   -> "CParen"
      | If       -> "If"
      | Then     -> "Then"
      | Else     -> "Else "
      | Let      -> "Let"
      | In       -> "In"
      | Int(i)   -> sprintf "Int(%d)" i
      | Bool(b)  -> sprintf "Bool(%b)" b
      | Ident(s) -> sprintf "Ident(%s)" s 
    in
    Buffer.add_string buf (sprintf "%s; " str);
    toks := List.tl !toks;
    incr count;
  done;
  if tokens <> [] then
    Buffer.truncate buf ((Buffer.length buf)-2);
  Buffer.add_string buf "]";
  Buffer.contents buf           
;;

(* Create a string version of the given parsed expression tree *)
let parsetree_string expr =
  let buf = Buffer.create 256 in                    (* extensibel character buffer *)
  let indent n =
    for i=1 to n do                           
      Buffer.add_string buf "  ";
    done;
  in
  let rec build expr depth =                        (* recursive helper *)
    indent depth;
    match expr with
    | IConst(i)  -> Buffer.add_string buf (sprintf "IConst(%d)\n" i);
    | BConst(b)  -> Buffer.add_string buf (sprintf "BConst(%b)\n" b);
    | Varname(s) -> Buffer.add_string buf (sprintf "Varname(%s)\n" s);
    | Add(left,right) | Sub(left,right)
    | Mul(left,right) | Div(left,right) ->
       let str = match expr with
         | Add _ -> "Add" | Sub _ -> "Sub"
         | Mul _ -> "Mul" | Div _ -> "Div"
         | _ -> failwith "impossible to reach (right?)"
       in
       Buffer.add_string buf (sprintf "%s\n" str);
       build left  (depth+1);
       build right (depth+1);
    | Cond(c) ->
       Buffer.add_string buf "Cond\n";
       indent (depth+1);
       Buffer.add_string buf ".if_expr:\n";
       build c.if_expr   (depth+2);
       indent (depth+1);
       Buffer.add_string buf ".then_expr:\n";
       build c.then_expr (depth+2);
       indent (depth+1);
       Buffer.add_string buf ".else_expr:\n";
       build c.else_expr (depth+2);
    | Letin(l) ->
       Buffer.add_string buf (sprintf "Letin( %s )\n" l.var_name);
       indent (depth+1);
       Buffer.add_string buf ".var_expr:\n";
       build l.var_expr   (depth+2);
       indent (depth+1);
       Buffer.add_string buf ".in_expr:\n";
       build l.in_expr (depth+2);
  in
  build expr 0;
  Buffer.contents buf                               (* return string from Buffer *)
;;

(* calcparse.ml:  *)
open Calclex;;
open Printf;;

(* RE-ORDER the production rules to go top-down. Re-arrange the
   calling order between arithmetic and control to allow them to be
   nested. *)

(* exception type for parsing error *)
exception ParseError of {
    msg  : string;
    toks : token list;
  };;

(* type for operations on two integers for expressions that combine
   results from left/right branches. *)
type intop =
  | Add | Sub | Mul | Div
  | Less | Greater | Equal
;;

(* PATCH PROBLEM: type for operations on two booleans boolean *)
type boolop =
  | And | Or
;;


(* algebraic types for expression tree: parsing results *)
type expr =
  | IntExp  of int
  | BoolExp of bool
  | Varname of string                         (* variable name that should be looked up *)
  | Intop   of {op         : intop;           (* binary op on ints like Plus, Minus, Less, etc. *)
                lexpr      : expr;
                rexpr      : expr; }
  | Boolop  of {op         : boolop;          (* PATCH PROBLEM: binary op on booleans like And/Or. *)
                lexpr      : expr;
                rexpr      : expr; }
  | Cond    of {if_expr    : expr;            (* if/then/else, expressions associated with each  *)
                then_expr  : expr;
                else_expr  : expr; }
  | Letin   of {var_name   : string;          (* let expression binding a new name *)
                var_expr   : expr;
                in_expr    : expr; }
  | Lambda  of {param_name : string;          (* create a function with named parameter *)
                code_expr  : expr; }
  | Apply   of {func_expr  : expr;            (* apply a function to a single parameter *)
                param_expr : expr; }
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
    | IntExp(i) ->
       Buffer.add_string buf (sprintf "IntExp(%d)\n" i);
    | BoolExp(b) ->
       Buffer.add_string buf (sprintf "BoolExp(%b)\n" b);
    | Varname(s) ->
       Buffer.add_string buf (sprintf "Varname(%s)\n" s);
    | Intop(o) ->
       let opstr = match o.op with
         | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div"
         | Less -> "Less" | Greater -> "Greater" | Equal -> "Equal"
       in
       Buffer.add_string buf (sprintf "%s\n" opstr);
       build o.lexpr (depth+1);
       build o.rexpr (depth+1);
    | Boolop(o) ->
       let opstr = match o.op with
         | And -> "And" | Or -> "Or"
       in
       Buffer.add_string buf (sprintf "%s\n" opstr);
       build o.lexpr (depth+1);
       build o.rexpr (depth+1);
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
    | Lambda(l) ->
       Buffer.add_string buf (sprintf "Lambda( %s )\n" l.param_name);
       build l.code_expr   (depth+1);
    | Apply(a) ->
       Buffer.add_string buf "Apply\n";
       indent (depth+1);
       Buffer.add_string buf ".func_expr:\n";
       build a.func_expr   (depth+2);
       indent (depth+1);
       Buffer.add_string buf ".param_expr:\n";
       build a.param_expr   (depth+2);
  in
  build expr 0;
  Buffer.contents buf                               (* return string from Buffer *)
;;


(* Top-level entry for recursive descent parser: create an expression
   tree from a series of tokens. Starts a series of mutually recursive
   functions. *)
let rec parse_expr tokens =
  let (expr, rest) as result = parse_oropt tokens in
  result

and parse_oropt toks =
  let rec iter lexpr toks =
    match toks with
    | OrTok :: rest ->
      let (rexpr,rest) = parse_andopt rest in
      iter (Boolop{op=Or;lexpr;rexpr}) rest
    | _ -> (lexpr, toks)
    in
    let (lexpr, rest) = parse_andopt toks in             (* create the initial left expression *)
    iter lexpr rest

and parse_andopt toks =
let rec iter lexpr toks =
  match toks with
  | AndTok :: rest ->
    let (rexpr,rest) = parse_compare rest in
    iter (Boolop{op=And;lexpr;rexpr}) rest
  | _ -> (lexpr, toks)
  in
  let (lexpr, rest) = parse_compare toks in             (* create the initial left expression *)
  iter lexpr rest

  (* parse a number comparison using <, >, or =. These cannot be chained
     together so are simpler than add/sub. *)
and parse_compare toks =
  let (lexpr, rest) = parse_addsub toks in
  match rest with
  (* FILL IN CASES for comparison of values *)
  | GreatThan :: rest -> let (rexpr,rest)=parse_addsub rest in (Intop{op=Greater;lexpr;rexpr},rest)
  | Equal :: rest -> let (rexpr,rest)=parse_addsub rest in (Intop{op=Equal;lexpr;rexpr},rest)
  | LessThan :: rest -> let (rexpr,rest)=parse_addsub rest in (Intop{op=Less;lexpr;rexpr},rest)
  | _ -> parse_addsub toks

(* parse addition and subtraction, left-associative *)
and parse_addsub toks =
  let rec iter lexpr toks =                            (* loop through adjacent + and - expressions *)
    match toks with
    | Plus :: rest ->                                  (* found + *)
       let (rexpr,rest) = parse_muldiv rest in         (* consume a higher-prec expression *)
       iter (Intop{op=Add;lexpr;rexpr}) rest           (* create an Add tree and iterate again *)
    | Minus :: rest ->                                 (* found - *)
       let (rexpr,rest) = parse_muldiv rest in         (* consume a higher-prec expression *)
       iter (Intop{op=Sub;lexpr;rexpr}) rest           (* create a  Sub tree and iterate again *)
    | _ -> (lexpr, toks)
  in
  let (lexpr, rest) = parse_muldiv toks in             (* create the initial left expression *)
  iter lexpr rest                                      (* start iterating *)

(* parse multiplication and division, same principle as parse_addsub *)
and parse_muldiv toks =
  let rec iter lexpr toks =
    match toks with
    | Times :: rest ->
       let (rexpr,rest) = parse_letin rest in
       iter (Intop{op=Mul;lexpr;rexpr}) rest
    | Slash :: rest ->
       let (rexpr,rest) = parse_letin rest in
       iter (Intop{op=Div;lexpr;rexpr}) rest
    | _ -> (lexpr, toks)
  in
  let (lexpr, rest) = parse_letin toks in
  iter lexpr rest

(* parse a let/in expression *)
and parse_letin toks =
  match toks with
  | Let :: Ident var_name :: Equal :: rest ->          (* look for a sequence of 'let name = ...' *)
     begin
       let (var_expr,rest) = parse_expr rest in        (* parse the rest of the expression *)
       match rest with
       | In :: rest ->                                 (* check that it ends with an 'in' *)
          let (in_expr,rest) = parse_expr rest in      (* parse the 'in <expr>' *)
          (Letin{var_name; var_expr; in_expr}, rest)   (* return result and rest of tokens *)
       | _ -> raise (ParseError{msg="Expected 'in' after 'let'"; toks=rest})
     end
  | _ -> parse_cond toks                               (* didn't find 'let', recurse lower *)

(* parse a conditional if <expr> then <expr> else <expr> *)
and parse_cond toks =
  match toks with
  (* P1: FILL IN CASES to parse if/then/else *)
  | If :: rest ->
      begin
       let (if_expr,rest) = parse_expr rest in
       match rest with
       | Then :: rest ->
          begin
           let (then_expr,rest) = parse_expr rest in
           match rest with
           | Else :: rest ->
              let (else_expr,rest) = parse_expr rest in
              (Cond{if_expr;then_expr;else_expr},rest)
           | _ -> raise (ParseError{msg="Expected 'else' "; toks=rest})
           end
       | _ -> raise (ParseError{msg="Expected 'then' "; toks=rest})
      end
  | _ -> parse_lambda toks


(* parse a lambda @x <expr> *)
and parse_lambda toks =
  (* P3: Fill in cases to m *)
  match toks with
  | At :: rest ->
   begin
    match rest with
    |Ident param_name :: rest ->
      let (code_expr,rest) = parse_expr rest in (Lambda{param_name;code_expr},rest)
    |_ -> raise (ParseError{msg="Expected identifier in lambda expression after @"; toks=rest})
   end
  | _ -> parse_apply toks

(* Parse a function application which is left-associative. Repeatedly
   matches expressions and applies them in left-to-right order. *)
and parse_apply toks =
  let rec iter lexpr toks =
    match toks with
    | IntTok _ :: rest | BoolTok _ :: rest | Ident _ :: rest | OParen :: rest ->
       let (rexpr,rest) = parse_ident toks in
       iter (Apply{func_expr=lexpr; param_expr=rexpr}) rest
    | _ -> (lexpr,toks)
  in
  let (lexpr, rest) = parse_ident toks in
  iter lexpr rest

(* parse identifiers, integers, booleans, and open/close parentheses *)
and parse_ident toks =
  match toks with
  | []              -> raise (ParseError {msg="expected an expression"; toks=toks})
  | IntTok n   :: tail -> (IntExp(n),tail)
  | BoolTok b  :: tail -> (BoolExp(b),tail)
  | Ident s :: tail -> (Varname(s),tail)
  | OParen  :: tail ->                                 (* parenthesized expresion *)
     begin
       let (expr,rest) = parse_expr tail in            (* start back at highest precedence *)
       match rest with
       | CParen::tail -> (expr,tail)
       | _ -> raise (ParseError {msg="unclosed parentheses"; toks=tail})
     end
  | _ -> raise (ParseError {msg="syntax error"; toks=toks})
;;

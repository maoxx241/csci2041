(* parser.ml: gives solution to lex_string problem, requires
   modification of the parse_tokens function. *)

open Printf;;

(* algebraic types for tokens: lexing results *)
type token =
  | Plus | Times | OParen | CParen | Int of int
  | Minus | Slash | Ident of string;;

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

let lex_string string =                      (* create a list of tokens  *)
  let len = String.length string in
  let rec lex pos =                          (* recursive helper *)
    if pos >= len then                       (* off end of string ? *)
      []                                     (* end of input *)
    else                                     (* more to lex *)
      match string.[pos] with                (* match a single character *)
      |' ' | '\t' | '\n' -> lex (pos+1)      (* skip whitespace *)
      |'+' -> Plus :: (lex (pos+1))          (* single char ops become operators *)
      |'-' -> Minus :: (lex (pos+1))          (* single char ops become operators *)
      |'*' -> Times :: (lex (pos+1))
      |'/' -> Slash :: (lex (pos+1))
      |'(' -> OParen :: (lex (pos+1))        (* and open/close parens *)
      |')' -> CParen :: (lex (pos+1))
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
         Ident(ident) :: (lex !stop)         (* and tack onto the stream of tokens *)
      | _ ->                                 (* any other characters lead to failures *)
         let msg = sprintf "lex error at char %d, char '%c'" pos string.[pos] in
         failwith msg
  in                                         (* end helper *)
  lex 0                                      (* call helper *)
;;

(* exception type for parsing error *)
exception ParseError of {
    msg  : string;
    toks : token list;
};;

(* algebraic types for expression tree: parsing results *)
type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Const of int
  | Varname of string
;;


(* rote example of expected parsing of valid input *)
let input =  "(a + b + c) / (9 * d - 10 / 4)";;
let parsed =
  Div (Add (Varname "a",
            Add (Varname "b", Varname "c")),
       Sub (Mul (Const 9, Varname "d"),
            Div (Const 10, Const 4)))
;;

(* Create an expression tree from a series of tokens *)
let parse_tokens tokens =

  (* prec0: self-evaluating tokens like Int and parenthsized expressions *)
  let rec prec0 toks =
    match toks with
    | [] ->                                  (* out of input *)
       raise (ParseError {msg="expected an expression"; toks=toks})
    | Int n ::  tail ->                      (* ints are self-evaluating *)
       (Const(n),tail)
    | Ident s ::  tail ->                      (* identifiers will require lookup *)
       (Varname(s),tail)
    | OParen :: tail ->                      (* parenthesized expresion *)
       begin
         let (expr,rest) = parse tail in     (* start back at highest precedence *)
         match rest with
         | CParen::tail -> (expr,tail)
         | _ -> raise (ParseError {msg="unclosed parentheses"; toks=tail})
       end
    | _ ->
       raise (ParseError {msg="syntax error"; toks=toks})

  (* prec1: multiplication and division *)
  and prec1_div toks =
    let (lexpr, rest) = prec0 toks in        (* try higher precdence expression first *)
    match rest with
    | Slash :: tail ->                       (* / is first *)
       let (rexpr,rest) = prec1_div tail in  (* recursively generate right-had expression *)
       (Div(lexpr,rexpr), rest)              (* return division of these two *)
    | _ -> (lexpr, rest)                     (* not a multiply so return expression and remaining tokens *)

  and prec1_mul toks =
    let (lexpr, rest) = prec1_div toks in    (* try higher precedence expression first *)
    match rest with
    | Times :: tail ->                       (* * is first *)
       let (rexpr,rest) = prec1_mul tail in  (* recursively generate right-had expression *)
       (Mul(lexpr,rexpr), rest)              (* return multiplication of these two *)
    | _ -> (lexpr, rest)                     (* not a multiply so return expression and remaining tokens *)


  (* prec2: addition and subtraction *)
  and prec2_sub toks =
    let (lexpr, rest) = prec1_mul toks in    (* try higher precdence expression first *)
    match rest with
    | Minus :: tail ->                       (* - is first *)
       let (rexpr,rest) = prec2_sub tail in  (* recursively generate right-had expression *)
       (Sub(lexpr,rexpr), rest)              (* return subtraction of these two *)
    | _ -> (lexpr, rest)                     (* not an addition so return expression and remaining tokens *)

  and prec2_add toks =
    let (lexpr, rest) = prec2_sub toks in    (* try higher precdence expression first *)
    match rest with
    | Plus :: tail ->                        (* + is first *)
       let (rexpr,rest) = prec2_add tail in  (* recursively generate right-had expression *)
       (Add(lexpr,rexpr), rest)              (* return addition of these two *)
    | _ -> (lexpr, rest)                     (* not an addition so return expression and remaining tokens *)

  (* top-level parsing entry *)
  and parse toks =
    prec2_add toks
  in

  let (expr, rest) = parse tokens in
  match rest with
  | [] -> expr
  | _  -> raise (ParseError{msg="tokens remain in stream";toks=rest})
;;

(* AFTER MODIFICATION uncommnet the below to test results *)

(* rote example of expected parsing of valid input *)
let input =  "(a + b + c) / (9 * d - 10 / 4)";;
let parsed =
  Div (Add (Varname "a",
            Add (Varname "b", Varname "c")),
       Sub (Mul (Const 9, Varname "d"),
            Div (Const 10, Const 4)))
;;

(* A few non-interactive tests *)
let lexed1  = lex_string "(a + b + c) / (9 * d - 10 / 4)";;
let parsed1 = parse_tokens lexed1 ;;
let expect1 =
  Div (Add (Varname "a",
            Add (Varname "b", Varname "c")),
       Sub (Mul (Const 9, Varname "d"),
            Div (Const 10, Const 4)))
;;
let ok1 = parsed1 = expect1;;

let lexed2  = lex_string "1/weight + - hello";;
let parsed2 = parse_tokens lexed2;; (* should raise a parse exception *)

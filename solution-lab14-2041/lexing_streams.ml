(* Solution to review problem that produces a lexing stream of tokens
   rather than a list of tokens. *)

(* ORIGINAL VERSION *)
(* create a list of tokens based on the string given.  *)
let lex_string string =
  let len = String.length string in
  let rec lex pos =                          (* recursive helper *)
    if pos >= len then                       (* off end of string ? *)
      []                                     (* end of input *)
    else                                     (* more to lex *)
      match string.[pos] with                (* match a single character *)
      |' ' | '\t' | '\n' -> lex (pos+1)      (* skip whitespace *)
      |'+' -> Plus      :: (lex (pos+1))        (* single char ops become operators *)
      |'-' -> Minus     :: (lex (pos+1))        
      |'*' -> Times     :: (lex (pos+1))
      |'/' -> Slash     :: (lex (pos+1))
      |'^' -> Carrot    :: (lex (pos+1))
      |'<' -> LessThan  :: (lex (pos+1))
      |'>' -> GreatThan :: (lex (pos+1))
      |'@' -> At        :: (lex (pos+1))
      |'=' -> Equal     :: (lex (pos+1))
      |'(' -> OParen    :: (lex (pos+1))        (* and open/close parens *)
      |')' -> CParen    :: (lex (pos+1))
      |';' -> Semicolon :: (lex (pos+1))
      | d when is_digit d ->                 (* see a digit *)
         let stop = ref pos in               (* scan through until a non-digit is found *)
         while !stop < len && is_digit string.[!stop] do
           incr stop;
         done;
         let numstr = String.sub string pos (!stop - pos) in (* substring is the int *)
         let num = int_of_string numstr in   (* parse the integer *)
         (IntTok num) :: (lex !stop)          (* and tack onto the stream of tokens *)
      | a when is_letter a ->                (* see a letter *)
         let stop = ref pos in               (* scan through until a non-letter is found *)
         while !stop < len && is_letter string.[!stop] do
           incr stop;
         done;
         let ident = String.sub string pos (!stop - pos) in (* substring is the identifier *)
         let tok = 
           match ident with
           | "def"   -> Def
           | "let"   -> Let
           | "in"    -> In
           | "if"    -> If
           | "then"  -> Then
           | "else"  -> Else
           | "and"   -> AndTok
           | "or"    -> OrTok
           | "true"  -> BoolTok true
           | "false" -> BoolTok false
           | _ -> Ident(ident)
         in
         tok :: (lex !stop)         (* and tack onto the stream of tokens *)
      | _ ->                                 (* any other characters lead to failures *)
         let msg = sprintf "char '%c' not recognized, at position %d in the input '%s'" string.[pos] pos string in
         raise (LexError{msg})
         (* let msg = sprintf "lex error at char %d, char '%c'" pos string.[pos] in
          * failwith msg *)
  in                                         (* end helper *)
  lex 0                                      (* call helper *)
;;

(* STREAM VERSION *)
(* produce stream which would repeatedly produce tokens via calls to Stream.next  *)
let lex_stream string =
  let len = String.length string in
  let pos = ref 0 in
  let rec lex i =
    if !pos >= len then
      None
    else
      match string.[!pos] with
      |' ' | '\t' | '\n' -> incr pos; lex i
      |'+' -> incr pos; Some Plus
      |'-' -> incr pos; Some Minus            
      |'*' -> incr pos; Some Times     
      |'/' -> incr pos; Some Slash     
      |'^' -> incr pos; Some Carrot    
      |'<' -> incr pos; Some LessThan  
      |'>' -> incr pos; Some GreatThan 
      |'@' -> incr pos; Some At        
      |'=' -> incr pos; Some Equal     
      |'(' -> incr pos; Some OParen
      |')' -> incr pos; Some CParen    
      |';' -> incr pos; Some Semicolon 
      | d when is_digit d ->
         let stop = ref !pos in
         while !stop < len && is_digit string.[!stop] do
           incr stop;
         done;
         let numstr = String.sub string !pos (!stop - !pos) in
         let num = int_of_string numstr in
         pos := !stop;
         Some (IntTok num)
      | a when is_letter a ->
         let stop = ref !pos in
         while !stop < len && is_letter string.[!stop] do
           incr stop;
         done;
         let ident = String.sub string !pos (!stop - !pos) in
         let tok = 
           match ident with
           | "def"   -> Def
           | "let"   -> Let
           | "in"    -> In
           | "if"    -> If
           | "then"  -> Then
           | "else"  -> Else
           | "and"   -> AndTok
           | "or"    -> OrTok
           | "true"  -> BoolTok true
           | "false" -> BoolTok false
           | _ -> Ident(ident)
         in
         pos := !stop;
         Some(tok)
      | _ ->
         let msg = sprintf "char '%c' not recognized, at position %d in the input '%s'" string.[!pos] !pos string in
         raise (LexError{msg})
  in
  Stream.from lex
;;


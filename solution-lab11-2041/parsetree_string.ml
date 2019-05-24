
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

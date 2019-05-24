(* tail recursive version *)
let rec colsum_tr n =
  let rec helper n sum =
    Printf.printf "%d\n" n;
    if n=1 then
      sum+1
    else
      let next = 
        if n mod 2 = 0 
        then n/2
        else 3*n+1
      in
      helper next (sum+n)
  in
  helper n 0
;;
        
let sum = colsum_tr 10 in
Printf.printf "sum: %d\n" sum;
;;

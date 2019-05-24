let rec colsum_nt n =
  Printf.printf "%d\n" n;
  if n = 1 then
    1
  else
    let next = 
      if n mod 2 = 0 
      then n/2
      else 3*n+1
    in
    let rest = colsum_nt next in
    n + rest
;;
        
let sum = colsum_nt 10 in
Printf.printf "sum: %d\n" sum;
;;

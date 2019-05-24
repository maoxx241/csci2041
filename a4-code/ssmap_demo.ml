(* ssmap_samples.ml: Demonstrate use of the Ssmap module to create
   maps of string to string. *)

open Printf;;

let _ =
  (* Demo the IntTreeset *)
  let map1 = Ssmap.empty in
  let map2 = Ssmap.add map1 "17" "Seventeen" in
  let map3 = Ssmap.add map2 "99" "Ninety-nine" in
  printf "map3:\n%s\n" (Ssmap.tree_string map3);

  let int_list = ["50","Fifty";      "25","Twenty-five";
                  "42","Forty-two";  "11","Eleven";
                  "86","Eighty-six"; "61","Sixty-one";
                  "99","Ninety-nine";"14","Fourteen";
                  "75","Seventy five";
                 ] in
  let map4 =
    let help map (k,v) = Ssmap.add map k v in
    List.fold_left help Ssmap.empty int_list
  in
  printf "map4:\n%s\n" (Ssmap.tree_string map4);

  printf "map4 to_string: %s\n" (Ssmap.to_string map4);

  let sum_keys cur key value = cur + (int_of_string key) in
  let sum = Ssmap.fold sum_keys 0 map4 in
  printf "Sum of keys is %d\n" sum;

  let concat_vals cur key value = cur^" "^value in
  let all_vals = Ssmap.fold concat_vals "" map4 in
  printf "All vals: %s\n" all_vals;
  
  let print_keyvals key value = printf "Key: %s, Value: %s\n" key value in
  Ssmap.iter print_keyvals map4;
  
;;

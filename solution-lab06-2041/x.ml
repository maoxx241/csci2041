# let int_transforms = [(fun x-> 2*x);
                        (fun x-> x+7);
                        (fun x-> 0);];;
val int_transforms : (int -> int) list = [<fun>; <fun>; <fun>]


# transforms [(fun x-> 2*x);
              (fun x-> x+7);
              (fun x-> 0);] 
             8;;
- : int list = [16; 15; 0]

# transforms [(fun x-> 2*x);
              (fun x-> x+7);
              (fun x-> 0);]
              20;;
- : int list = [40; 27; 0]

# transforms [(fun x-> x="indeed"); 
              (fun x-> (String.length x) > 4);]
             "no";;
- : bool list = [false; false]
# transforms [(fun x-> x="indeed"); 
              (fun x-> (String.length x) > 4);]
             "indeed";;
- : bool list = [true; true]
# transforms [(fun x-> x="indeed"); 
              (fun x-> (String.length x) > 4);]
             "indubitably";;
- : bool list = [false; true]


   # let string_transforms = [(fun x-> x="indeed");
                              (fun x-> (String.length x) > 4)];;
   val string_transforms : (string -> bool) list = [<fun>; <fun>]

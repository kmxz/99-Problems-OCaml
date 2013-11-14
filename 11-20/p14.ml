(* Duplicate the elements of a list *)

let rec duplicate = function
    | [] -> []
    | a::b -> a::a::(duplicate b)
;;
    
assert (duplicate [`a;`b;`c;`c;`d] = [`a;`a;`b;`b;`c;`c;`c;`c;`d;`d]) ;;

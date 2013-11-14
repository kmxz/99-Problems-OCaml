(* Eliminate consecutive duplicates of list elements *)

let rec compress = function
    | a::(b::c as t) when a = b -> compress t
    | a::b -> a :: (compress b)
    | [] -> []
;;

(* Drops the first element if it's equal to the second element, then recursively work 
   on the rest of the list (including the second element). *)

assert (compress [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [`a;`b;`c;`a;`d;`e]) ;;

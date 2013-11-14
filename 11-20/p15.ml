(* Duplicate the elements of a list a given number of times *)

let rec rep l ?(d=0) t = match l with
    | [] -> []
    | hd::tl -> if d = t then rep tl t else begin hd::(rep (hd::tl) t ~d:(d+1)) end 
;;

assert (rep [`a;`b;`c] 3 = [`a;`a;`a;`b;`b;`b;`c;`c;`c]) ;;

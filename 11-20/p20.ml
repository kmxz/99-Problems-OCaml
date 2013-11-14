(* Remove the K'th element from a list *)

let rec drp t = function
    | [] -> []
    | hd::tl -> if t = 0 then tl else begin hd::(drp (t-1) tl) end 
;;

assert (drp 2 [`a;`b;`c;`d] = [`a;`c;`d]) ;;

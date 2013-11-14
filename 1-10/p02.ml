(* Find the last but one element of a list *)

let rec slast = function
    | [] -> raise (Failure "Empty list")
    | h::le::[] -> (h, le)
    | h::t -> slast t
;;
        
assert (slast [ `a ; `b ; `c ; `d ] = (`c,`d)) ;;
(* Find the last element of a list. *)

let rec last = function
    | [] -> raise (Failure "Empty list")
    | h::[] -> h
    | h::t -> last t
;;

assert (last [ `a ; `b ; `c ; `d ] = `d) ;;
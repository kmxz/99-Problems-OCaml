(* Find the K'th element of a list *)

let rec find lst idx = match lst with
    | [] -> raise (Failure "List too short")
    | h::t -> if idx = 1 then h else find t (idx - 1)
;;

(* If k is smaller than 1, it will return None after traversing the entire list. *)

assert (find 3 [ `a ; `b ; `c ; `d ; `e ] = `c) ;;
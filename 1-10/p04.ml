(* Find the number of elements of a list *)

let rec lth ?(ini=0) lst = match lst with
    | [] -> ini
    | _::t -> lth t ~ini:(ini + 1)
;;

(* This function is tail-recursive: it uses a constant amount of 
   stack memory regardless of list size. *)

assert (lth [ `a ; `b ; `c] = 3) ;;
assert (lth [] = 0) ;;

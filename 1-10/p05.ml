(* Reverse a list. *)

let rec rev ?(blt=[]) = function
    | [] -> blt
    | h::t -> rev t ~blt:(h::blt)
;;
    
(* This function is tail-recursive: it uses a constant amount of 
   stack memory regardless of list size. *)

assert (rev [`a ; `b ; `c] = [`c ; `b ; `a]) ;;

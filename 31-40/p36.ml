(* Determine the prime factors of of a given positive integer. *)

(* Still it's not as elegant as the given one. *)

let factors n =
    let rec r cur lst = function
        | 1 -> lst
        | rem when (rem mod cur = 0) -> r cur 
            begin match lst with
                | (cn, ct)::tl when (cn = cur) -> ((cur, ct + 1)::tl)
                | tl -> (cur, 1)::tl
            end (rem / cur)
        | rem -> r (cur + 1) lst rem
    in
    List.rev (r 2 [] n)
;;
    
assert (factors 315 = [3,2 ; 5,1 ; 7,1]) ;;
  

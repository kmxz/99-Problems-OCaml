(* Determine whether a given integer number is prime. *)

let is_prime i =
    let rec rip = function
        | 1 -> true
        | s -> if (i mod s) = 0 then false else (rip (s - 1))
    in
    rip (int_of_float (sqrt (float_of_int i)))
;;

assert (is_prime 7) ;;
assert (not (is_prime 12)) ;;

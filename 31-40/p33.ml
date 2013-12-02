(* Determine if two positive integer numbers are coprime. *)

(* The answer of 32 *)

let rec gcd a = function
    | 0 -> a
    | b -> gcd b (a mod b)
;;

(* Using the answer of 32 *)

let coprime a b = (gcd a b) = 1;;

assert (coprime 13 27) ;;
assert (not (coprime 20536 7826)) ;;

(* Calculate Euler's totient function phi(m) *)

(* The answer of 32 *)

let rec gcd a = function
    | 0 -> a
    | b -> gcd b (a mod b)
;;

(* The answer of 33 *)

let coprime a b = (gcd a b) = 1;;

(* Using the answers of 32 and 33 *)

let rec phi ?cur ?(acc=0) m = match cur with
    | None -> phi ~cur:m m
    | Some 0 -> acc
    | Some i ->  phi ~cur:(i - 1) ~acc:(if coprime m i then acc + 1 else acc) m
;;

assert (phi 10 = 4) ;;
assert (phi 13 = 12) ;;

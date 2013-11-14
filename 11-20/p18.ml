(* Extract a slice from a list *)

let rec spl lst ctp = match lst with
    | [] -> ([], [])
    | h::t -> if ctp = 0 then ([], lst) else begin let (a, b) = spl t (ctp - 1) in (h::a, b) end
;;

(* The part above is exactly the solution of p17 *)
    
let slice lst stp edp = let (h1, t1) = spl lst (stp - 1) in let (h2, _) = spl t1 (edp - stp + 1) in h2;;

assert (slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 7 = [`c;`d;`e;`f;`g]) ;;

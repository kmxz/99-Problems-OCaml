(* Determine the prime factors of of a given positive integer. *)

(* This solution is much worse than the given one. *)

(* The answer of 31 *)

let is_prime i =
    let rec rip = function
        | 1 -> true
        | s -> if (i mod s) = 0 then false else (rip (s - 1))
    in
    rip (int_of_float (sqrt (float_of_int i)))
;;

(* Using the answer of 31 *)
        
let factors el = 
    let rec tsg tn ori cel = 
        if tn = 1 then ori else begin
            if (is_prime tn) && (cel mod tn = 0) then 
                tsg tn (tn::ori) (cel / tn)
            else tsg (tn - 1) ori cel;
        end
    in
    if is_prime el then [el] else (tsg (int_of_float (sqrt (float_of_int el))) [] el)
;;
        
assert (factors 315 = [3;3;5;7]) ;;
  

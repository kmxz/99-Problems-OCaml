(* 31. Determine whether a given integer number is prime. *)

    let is_prime i =
        let rec rip = function
            | 1 -> true
            | s -> if (i mod s) = 0 then false else (rip (s - 1))
        in
        rip (int_of_float (sqrt (float_of_int i)))

(* 32. Determine the greatest common divisor of two positive integer numbers. *)

    let rec gcd a = function
        | 0 -> a
        | b -> gcd b (a mod b)
    
(* 33. Determine whether two positive integer numbers are coprime. *)

    (* Using the answer of 32 *)
    
    let coprime a b = (gcd a b) = 1  
    
(* 34. Calculate Euler's totient function ¦Õ(m) *)

    (* Using the answers of 32 and 33 *)
    
    let rec phi ?cur ?(acc=0) m = match cur with
        | None -> phi ~cur:m m
        | Some 0 -> acc
        | Some i ->  phi ~cur:(i - 1) ~acc:(if coprime m i then acc + 1 else acc) m
        
(* 35. Determine the prime factors of a given positive integer. *)

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
        
(* 36. Determine the prime factors of a given positive integer, using tuples *)

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

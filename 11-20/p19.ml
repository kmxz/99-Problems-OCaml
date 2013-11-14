(* Rotate a list N places to the left. *)

(* Rotation by N is the same as rotation by (list length) + N, so rotating by -2 is the
   same as rotating by (list length) - 2. *)
   
let rec spl lst ctp = match lst with
    | [] -> ([], [])
    | h::t -> if ctp = 0 then ([], lst) else begin let (a, b) = spl t (ctp - 1) in (h::a, b) end
;;

(* The part above is exactly the solution of p17 *)
    
let rotate lst pos = 
    let rpos = 
        let len = List.length lst
        in
        (fun x -> if x < 0 then x + len else x) (pos mod len)
    in
    let (a, b) = spl lst rpos in b @ a
;;

assert (rotate [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`d;`e;`f;`g;`h;`a;`b;`c]) ;;
assert (rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-2) = [`g;`h;`a;`b;`c;`d;`e;`f]) ;;

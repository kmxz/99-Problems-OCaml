(* Split a list into two parts; the length of the first part is given. *)

(* Assuming here that if the length of the first part is longer than the entire
   list, then the first part is the list and the second part is empty. *)

(* Pure (w/o library) *)

let rec spl lst ctp = match lst with
    | [] -> ([], [])
    | h::t -> if ctp = 0 then ([], lst) else begin let (a, b) = spl t (ctp - 1) in (h::a, b) end
;;
    
(* Tail recursive *)

let rec spl lst ?(hd=[]) ctp = match lst with
    | [] -> (List.rev hd, [])
    | h::t -> if ctp = 0 then (List.rev hd, lst) else begin spl t (ctp - 1) ~hd:(h::hd) end 
;;

assert (spl [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = 
    ([`a;`b;`c] , [`d;`e;`f;`g;`h;`i;`j])) ;;

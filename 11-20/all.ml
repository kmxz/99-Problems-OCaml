(* 11. Modified run-length encoding. *)
        
    let rec enc ?(ald=0) = function
        | a::(b::c as t) when a = b -> enc t ~ald:(ald + 1)
        | a::b -> (if ald = 0 then One a else (Many (ald + 1, a))) :: (enc b ~ald:0)
        | [] -> []
        
(* 12. Decode a run-length encoded list. *)

    (* Pure *)

        let rec dec = function
            | (One e)::tl ->  e :: dec tl
            | (Many (0, e))::tl -> dec tl
            | (Many (c, e))::tl -> e :: dec ((Many (c - 1, e))::tl)
            | [] -> []
            
    (* Library *)
    
        let dec l = 
            let mf = function
                | One e -> [e]
                | Many (c, e) -> Array.to_list(Array.make c e)
            in
            List.flatten (List.map mf l)

(* 13. Run-length encoding of a list (direct solution). *)

    (* Already solved above *)
    
(* 14. Duplicate the elements of a list. *)

    let rec dup = function
        | [] -> []
        | a::b -> a::a::(dup b)
        
(* 15. Replicate the elements of a list a given number of times. *)

    let rec rep l ?(d=0) t = match l with
        | [] -> []
        | hd::tl -> if d = t then rep tl t else begin hd::(rep (hd::tl) t ~d:(d+1)) end 
       
(* 16. Drop every N'th element from a list. *)

    let rec drp l ?(d=1) t = match l with
        | [] -> []
        | hd::tl -> if d = t then drp tl t else begin hd::(drp tl t ~d:(d+1)) end 
        
(* 17. Split a list into two parts; the length of the first part is given. *)

    (* Pure (w/o library) *)

    let rec spl lst ctp = match lst with
        | [] -> ([], [])
        | h::t -> if ctp = 0 then ([], t) else begin let (a, b) = spl t (ctp - 1) in (h::a, b) end
        
    (* Tail recursive *)
    
    let rec spl lst ?(hd=[]) ctp = match lst with
        | [] -> (List.rev hd, [])
        | h::t -> if ctp = 0 then (List.rev hd, t) else begin spl t (ctp - 1) ~hd:(h::hd) end 
        
(* 18. Extract a slice from a list. *)
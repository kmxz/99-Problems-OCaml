(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. *)

    let rec last = function
        | [] -> raise (Failure "Empty list")
        | h::[] -> h
        | h::t -> last t
        
(* 2. Find the last but one (last and penultimate) elements of a list. *)

    let rec slast = function
        | [] -> raise (Failure "Empty list")
        | h::le::[] -> (h, le)
        | h::t -> slast t
        
(* 3. Find the k'th element of a list. *)

    let rec find lst idx = match lst with
        | [] -> raise (Failure "Too shortx")
        | h::t -> if idx = 1 then h else find t (idx - 1)
        
(* 4. Find the number of elements of a list. *)
    
    let rec lth ?(ini=0) lst = match lst with
        | [] -> ini
        | _::t -> lth t ~ini:(ini + 1)
        
(* 5. Reverse a list. *)

    let rec rev ?(blt=[]) = function
        | [] -> blt
        | h::t -> rev t ~blt:(h::blt)
        
(* 6. Find out whether a list is a palindrome. *)

    (* Pure, utilizing "rev" function above *)
    
    let ip lst = 
        let rec rist lst1 = begin function
            | [] -> true
            | l2h::l2t -> let l1h::l1t = lst1 in
                if l2h = l1h then rist l1t l2t else false
        end in rist lst (rev lst)

    (* Library *)
    
    let ip lst = lst = List.rev lst
    
(* 7. Flatten a nested list structure. *)

    let flat l =
        let rec fldl nei lst = begin match nei with
            | One x -> x::lst
            | Many x -> List.fold_right fldl x lst
        end in
        List.fold_right fldl l []
        
(* 8. Eliminate consecutive duplicates of list elements. *)

    let rec compress = function
        | a::(b::c as t) when a = b -> compress t
        | a::b -> a :: (compress b)
        | [] -> []
        
(* 9. Pack consecutive duplicates of list elements into sublists. *)
     
    let pack l =
        let rec passive nei (csl, gls) = begin match csl with
            | h::_ when h <> nei -> ([nei], csl::gls)
            | _ -> (nei::csl, gls)
        end in
        let (u, v) = List.fold_right passive l ([], []) in
        u::v
            
(* 10. Run-length encoding of a list. *)

    let rec enc ?(ald=0) = function
        | a::(b::c as t) when a = b -> enc t ~ald:(ald + 1)
        | a::b -> (ald + 1, a) :: (enc b ~ald:0)
        | [] -> []
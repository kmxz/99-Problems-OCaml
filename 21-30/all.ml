(* 21. Insert an element at a given position into a list. *)

    let rec insert_at el pos lst = 
        if pos = 0 then
            el::lst
        else begin match lst with
            | [] -> []
            | h::t -> h::(insert_at el (pos - 1) t)
        end 
    
(* 22. Create a list containing all integers within a given range. *)

    (* Pure, tail recursive *)

        let clst s e = 
            let symbol = if s > e then (+) else (-)
            in let rec symstep opl ts = begin
                if ts = s then ts::opl else (symstep (ts::opl) (symbol ts 1))
            end in symstep [] e
            
    (* Imperative *)
    
        let clst s e =
            let op = ref [] in
            if s > e then begin
                for i = e to s do
                    op := i::(!op)
                done
            end else begin
                for i = e downto s do
                    op := i::(!op)
                done
            end;
            !op
            
(* 23. Extract a given number of randomly selected elements from a list *)
    
    let rand_select lst total = 
        let len = List.length lst in
        let rec pick rst ori = 
            if rst > len then ori else
            (pick (rst + 1) ((Random.int rst)::ori))  in
        let rec drp t = function
            | [] -> raise (Failure "No such position!")
            | hd::tl -> if t = 0 then (hd, tl) else begin let (rh, rs) = (drp (t-1) tl) in (rh, hd::rs) end in
        let pls = ref lst
        in List.map (fun x -> let (pu, rs) = (drp x (!pls)) in pls := rs; pu) (pick (len - total + 1) [])
     
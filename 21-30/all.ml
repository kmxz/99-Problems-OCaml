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

(* 24. Lotto: draw N different random numbers from the set 1 -- M *)

    (* Making use of solution of 22-23 *)

    let lotto_select n m = rand_select (clst 1 m) n
    
(* 25. Generate a random permutation of the elements of a list. *)

    (* Making use of solution of 24, which further depends on 22 and 23 *)

    let permutation l = let len = List.length l in let seed = uld len len in
        List.map (fun i -> List.nth l (i - 1)) seed

    (* Implementing Fisher-Yates shuffle, by converting the list into an array *)
    
    let permutation l = let a = Array.of_list l in let swap i j = let oi = a.(i) in a.(i) <- a.(j); a.(j) <- oi in
        for i = List.length l - 1 downto 0 do
            swap i (Random.int (i + 1))
        done;
        Array.to_list a

(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list. *)

    let extract k l = 
        let rec esingle rmditm rmdlst rmdtot current_list current_total =
            if rmditm = 0 then
                current_list::current_total
            else let hd::tl = rmdlst in
                esingle (rmditm - 1) tl (rmdtot - 1) (hd::current_list) (if rmditm < rmdtot then (esingle rmditm tl (rmdtot - 1) current_list current_total) else current_total)
        in let len = List.length l in
        List.map List.rev (esingle k l len [] [])
        
(* 27. Group the elements of a set into disjoint subsets. *)

    (* Using the answer of 26 *)
    
    let group lst dis = 
        let kill lst exc = List.filter (fun i -> not (List.mem i exc)) lst in
        let rec pick_from_dis done_part survivers = function
            | [] -> [done_part]
            | hd::tl -> List.flatten (List.map (fun x -> pick_from_dis (x::done_part) (kill survivers x) tl) (extract hd survivers))
        in
    List.map List.rev (pick_from_dis [] lst dis)
    
(* 28. Sorting a list of lists according to length of sublists. *)

    let length_sort (lst: 'a list list) = List.stable_sort (fun sla slb -> (List.length sla) - (List.length slb)) lst
    
    let frequency_sort (lst: 'a list list) = 
        let mt = Hashtbl.create 1 in
        let mkd len = 
            let ori = try Hashtbl.find mt len with | Not_found -> 0 in
            Hashtbl.replace mt len (ori + 1)
        in
        List.map (fun (f, a) -> a) (List.sort (fun (f1, _) (f2,_) -> f1 - f2) (List.map (fun (l, a) -> (Hashtbl.find mt l, a)) (List.map (fun a -> let l = List.length a in mkd l; (l, a)) lst)))
        
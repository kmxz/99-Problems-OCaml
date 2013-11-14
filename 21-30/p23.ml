(* Extract a given number of randomly selected elements from a list *)

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
;;
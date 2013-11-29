(* Lotto: draw N different random numbers from the set 1 -- M *)

(* From problem 22 ---------------- *)

let clst s e = 
    let symbol = if s > e then (+) else (-)
    in let rec symstep opl ts = begin
        if ts = s then ts::opl else (symstep (ts::opl) (symbol ts 1))
    end in symstep [] e
;;

(* From problem 23 ---------------- *)

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

(* The solution ---------------- *)

let lotto_select n m = rand_select (clst 1 m) n
;;
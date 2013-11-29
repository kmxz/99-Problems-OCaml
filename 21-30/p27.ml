(* Group the elements of a set into disjoint subsets of specified sizes. *)

(* This part is from 26 *)

let extract k l = 
    let rec esingle rmditm rmdlst rmdtot current_list current_total =
        if rmditm = 0 then
            current_list::current_total
        else let hd::tl = rmdlst in
            esingle (rmditm - 1) tl (rmdtot - 1) (hd::current_list) (if rmditm < rmdtot then (esingle rmditm tl (rmdtot - 1) current_list current_total) else current_total)
    in let len = List.length l in
    List.map List.rev (esingle k l len [] [])
;;

(* Using the answer of 26 *)

let group lst dis = 
    let kill lst exc = List.filter (fun i -> not (List.mem i exc)) lst in
    let rec pick_from_dis done_part survivers = function
        | [] -> [done_part]
        | hd::tl -> List.flatten (List.map (fun x -> pick_from_dis (x::done_part) (kill survivers x) tl) (extract hd survivers))
    in
    List.map List.rev (pick_from_dis [] lst dis)
;;

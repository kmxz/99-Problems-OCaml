(* Construct completely balanced binary trees. *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

(* First attempt, failed with stack overflow when input = 40 *)

let rec cbal = function
    | 0 -> [Empty]
    | x when x mod 2 = 1 -> let rs = cbal (x / 2) in List.flatten (List.map (fun ftr -> List.map (fun str -> Node ('x', ftr, str)) rs) rs)
    | x -> let lt = x / 2 in let lft = cbal lt and rht = cbal (lt - 1) in (List.flatten (List.map (fun ftr -> List.map (fun str -> Node ('x', ftr, str)) lft) rht)) @ (List.flatten (List.map (fun ftr -> List.map (fun str -> Node ('x', ftr, str)) rht) lft))
;;
(* Symmetric binary trees. *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

(* Begin of solution *)

let rec is_mirror = function
    | (Empty, Empty) -> true
    | (Node(_, lst1, rst1), Node(_, lst2, rst2)) -> (is_mirror (lst1,rst2)) && (is_mirror (lst2,rst1)) 
    | _ -> false
;;

let is_symmetric = function
    | Empty -> true
    | Node(_, l, r) -> is_mirror (l, r)
;;

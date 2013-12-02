(* Binary search trees (dictionaries) *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree
;;

(* Begin of solution *)

let construct = function
    | [] -> Empty
    | h::t ->
        let rec insert (Node (i, l, r)) elm =
            if i < elm then
                if r = Empty then Node (i, l, Node (elm, Empty, Empty)) else Node (i, l, insert r elm)
            else begin
                if l = Empty then Node (i, Node (elm, Empty, Empty), r) else Node (i, insert l elm, r)
            end
        in
        let rec r_construct ctree = function
            | [] -> ctree
            | h::t -> r_construct (insert ctree h) t
        in
        r_construct (Node (h, Empty, Empty)) t
;;

(*
A utility function to check the answer

let rec btree_printer ?(d=0) = function
    | Empty -> ()
    | Node (i, l, r) -> btree_printer ~d:(d + 1) l; print_string (String.make d ' '); print_int i; print_newline (); btree_printer ~d:(d + 1) r
;;
*)
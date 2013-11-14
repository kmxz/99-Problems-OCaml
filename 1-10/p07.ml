(* Flatten a nested list structure. *)

(* There is no nested list type in Objective Caml, so we need to define
   one first. A node of a nested list is either an element, or a list
   of nodes. *)

type 'a node = 
    One of 'a
  | Many of 'a node list
;;

(* The example from the problem: (a (b (c d) e)) *)

let example = 
  [ One `a ; Many [ One `b ; Many [ One `c ; One `d ] ; One `e ] ]
;;

(* This function traverses the list, prepending any encountered elements
   to an accumulator, which flattens the list in inverse order. It can then
   be reversed to obtain the actual flattened list. *)

let flat l =
    let rec fldl nei lst = begin match nei with
        | One x -> x::lst
        | Many x -> List.fold_right fldl x lst
    end in
    List.fold_right fldl l []
;;

assert (flatten example = [ `a ; `b ; `c ; `d ; `e ])

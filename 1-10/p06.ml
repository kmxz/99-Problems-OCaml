(* Find out whether a list is a palindrome. *)

(* HINT: a palindrome is its own reverse. Use either the rev function from
   problem 5, or the built-in List.rev *)

(* Pure, utilizing "rev" function above *)

let is_palindrome lst = 
    let rec rist lst1 = begin function
        | [] -> true
        | l2h::l2t -> let l1h::l1t = lst1 in
            if l2h = l1h then rist l1t l2t else false
    end in rist lst (rev lst)
;;

(* Library *)

let is_palindrome lst = lst = List.rev lst;;

assert (is_palindrome [ `x ; `a ; `m ; `a ; `x ]) ;;
assert (not (is_palindrome [ `a ; `b ])) ;;

(* Pack consecutive duplicates of list elements into sublists. *)

(* Since Objective Caml cannot represent a list of both normal values and N,X values, one needs to 
   define a type for that. *)

type 'a rle = 
  | One of 'a 
  | Many of (int * 'a)
;;

let rec enc ?(ald=0) = function
    | a::(b::c as t) when a = b -> enc t ~ald:(ald + 1)
    | a::b -> (if ald = 0 then One a else (Many (ald + 1, a))) :: (enc b ~ald:0)
    | [] -> []
;;

assert (enc [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = 
    [Many (4,`a) ; One `b ; Many (2,`c) ; Many (2,`a) ; One `d ; Many (4,`e)]) ;;


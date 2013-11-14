(* Decode a run-length encoded list *)

type 'a rle = 
  | One of 'a 
  | Many of (int * 'a)
;;

(* Pure *)

let rec dec = function
    | (One e)::tl ->  e :: dec tl
    | (Many (0, e))::tl -> dec tl
    | (Many (c, e))::tl -> e :: dec ((Many (c - 1, e))::tl)
    | [] -> []
;;
    
(* Library *)

let dec l = 
    let mf = function
        | One e -> [e]
        | Many (c, e) -> Array.to_list(Array.make c e)
    in
    List.flatten (List.map mf l)
;;
            
let example_from = 
  [Many (4,`a) ; One `b ; Many (2,`c) ; Many (2,`a) ; One `d ; Many (4,`e)] ;;

let example_to = [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] ;;

assert (dec example_from = example_to) ;;

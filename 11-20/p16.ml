(* Drop every N'th element from a list *)

let rec drp l ?(d=1) t = match l with
    | [] -> []
    | hd::tl -> if d = t then drp tl t else begin hd::(drp tl t ~d:(d+1)) end
;;

assert (drp [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = [`a;`b;`d;`e;`g;`h;`j]) ;;

(* Insert an element at a given position into a list *)

let rec insert_at el pos lst = 
    if pos = 0 then
        el::lst
    else begin match lst with
        | [] -> []
        | h::t -> h::(insert_at el (pos - 1) t)
    end
;;

assert (insert_at `alfa 2 [`a;`b;`c;`d] = [`a;`alfa;`b;`c;`d]) ;;

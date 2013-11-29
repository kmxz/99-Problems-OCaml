(* Generate the combinations of K distinct objects chosen from the N elements of a list. *)

(* I have to admit that my implementation is not as elegant as the orignal one *)

let extract k l = 
    let rec esingle rmditm rmdlst rmdtot current_list current_total =
        if rmditm = 0 then
            current_list::current_total
        else let hd::tl = rmdlst in
            esingle (rmditm - 1) tl (rmdtot - 1) (hd::current_list) (if rmditm < rmdtot then (esingle rmditm tl (rmdtot - 1) current_list current_total) else current_total)
    in let len = List.length l in
    List.map List.rev (esingle k l len [] [])
;;

assert (extract 2 [`a;`b;`c;`d] = [[`a;`b]; [`a;`c]; [`a;`d]; [`b;`c]; [`b;`d]; [`c;`d]]) ;;

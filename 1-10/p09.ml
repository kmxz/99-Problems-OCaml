(* Pack consecutive duplicates of list elements into sublists. *)

let pack l =
    let rec passive nei (csl, gls) = begin match csl with
        | h::_ when h <> nei -> ([nei], csl::gls)
        | _ -> (nei::csl, gls)
    end in
    let (u, v) = List.fold_right passive l ([], []) in
    u::v
;;     
        
assert (pack [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e] = 
    [[`a;`a;`a;`a];[`b];[`c;`c];[`a;`a];[`d;`d];[`e;`e;`e;`e]]) ;;


(* Sorting a list of lists according to length of sublists. *)

let length_sort (lst: 'a list list) = List.stable_sort (fun sla slb -> (List.length sla) - (List.length slb)) lst  

assert (length_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e]; [`i;`j;`k;`l]; [`m;`n]; [`o] ] = 
    [[`o]; [`d; `e]; [`d; `e]; [`m; `n]; [`a; `b; `c]; [`f; `g; `h]; [`i; `j; `k; `l]]) ;;

let frequency_sort (lst: 'a list list) = 
    let mt = Hashtbl.create 1 in
    let mkd len = 
        let ori = try Hashtbl.find mt len with | Not_found -> 0 in
        Hashtbl.replace mt len (ori + 1)
    in
    List.map (fun (f, a) -> a) (List.stable_sort (fun (f1, _) (f2,_) -> f1 - f2) (List.map (fun (l, a) -> (Hashtbl.find mt l, a)) (List.map (fun a -> let l = List.length a in mkd l; (l, a)) lst)))
  
assert (frequency_sort [ [`a;`b;`c]; [`d;`e]; [`f;`g;`h]; [`d;`e]; [`i;`j;`k;`l]; [`m;`n]; [`o] ] =
    [[`i; `j; `k; `l]; [`o]; [`a; `b; `c]; [`f; `g; `h]; [`d; `e]; [`d; `e]; [`m; `n]]) ;;

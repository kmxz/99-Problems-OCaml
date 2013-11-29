(* Generate a random permutation of the elements of a list. *)

(* Making use of solution of 24, which further depends on 22 and 23 *)

let permutation l = let len = List.length l in let seed = uld len len in
    List.map (fun i -> List.nth l (i - 1)) seed

(* Implementing Fisher-Yates shuffle, by converting the list into an array *)

let permutation l = let a = Array.of_list l in let swap i j = let oi = a.(i) in a.(i) <- a.(j); a.(j) <- oi in
    for i = List.length l - 1 downto 0 do
        swap i (Random.int (i + 1))
    done;
    Array.to_list a
        
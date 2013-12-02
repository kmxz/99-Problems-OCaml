type 'a freq = Fr of ('a * int);;
type 'a inner = Raw of ('a) | Pair of ('a inner * 'a inner);;
type 'a huffman = Hc of ('a * string);;
type 'a outer = Pd of ('a inner * int);;

let huffman_enc rfr =
    let update = List.stable_sort (fun (Pd (_, ac)) (Pd (_, bc)) -> ac - bc)
    in
    let ifr = List.map (fun (Fr (content, count)) -> Pd (Raw (content), count)) rfr
    in 
    let rec opt ls = 
        if (List.length ls) > 1 then
            let nls = update ls
            in
            let (Pd (i1, frq1))::(Pd (i2, frq2))::tl = nls in
            opt ((Pd (Pair (i1, i2), (frq1 + frq2)))::tl)
        else ls
    in
    let [(Pd (result, _))] = opt ifr
    in 
    let rec prtt acc_total head = function 
        | Raw (content) -> (Hc (content, head))::acc_total
        | Pair (content1, content2) -> prtt (prtt acc_total (head ^ "0") content1) (head ^ "1") content2
    in
    (prtt [] "" result)
;;

(* 
let input = [Fr(a,45); Fr(b,13); Fr(c,12); Fr(d,16); Fr(e,9); Fr(f,5)];;
huffman_enc input;;
*)
(*
output will be something like [Hc(a,'0'); Hc(b,'101'); Hc(c,'100'); Hc(d,'111'); Hc(e,'1101'); hc(f,'1100')]
*)
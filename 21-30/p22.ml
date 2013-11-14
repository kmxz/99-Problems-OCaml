(* Create a list containing all integers within a given range. *)

(* Pure, tail recursive *)

let clst s e = 
    let symbol = if s > e then (+) else (-)
    in let rec symstep opl ts = begin
        if ts = s then ts::opl else (symstep (ts::opl) (symbol ts 1))
    end in symstep [] e
;;
    
(* Imperative *)

let clst s e =
    let op = ref [] in
    if s > e then begin
        for i = e to s do
            op := i::(!op)
        done
    end else begin
        for i = e downto s do
            op := i::(!op)
        done
    end;
    !op
;;

assert (clst 4 9 = [4;5;6;7;8;9]) ;;
assert (clst 9 4 = [9;8;7;6;5;4]) ;;

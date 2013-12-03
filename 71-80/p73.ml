(* Lisp-like tree representation. *)

type 'a mult_tree = T of 'a * 'a mult_tree list;;

(* Begin of solution *)

let lispy tr = 
    let buf = Buffer.create 128 in
    let rec prt (T (a, l)) = 
        if l = [] then (Buffer.add_char buf ' '; Buffer.add_char buf a) else begin
        Buffer.add_string buf " ("; Buffer.add_char buf a; List.iter prt l; Buffer.add_char buf ')'
        end
    in
    prt tr; Buffer.sub buf 1 (Buffer.length buf - 1)
;;

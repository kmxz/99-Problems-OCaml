(* Tree construction from a node string *)

type 'a mult_tree = T of 'a * 'a mult_tree list;;

(* Begin of solution (the second part is a lot worse than the original solution) *)

let rec string_of_tree t = 
    let buf = Buffer.create 128 in
    let rec string_of_tree (T (s, sl)) = 
        Buffer.add_char buf s;
        List.iter string_of_tree sl;
        Buffer.add_char buf '^'
    in
    string_of_tree t; Buffer.contents buf
;;

let tree_of_string s = 
    let len = String.length s in
    let rec handle pos current_stack =
        if pos = len then let [T('^', [x])] = current_stack in x
        else begin
            let c = s.[pos] in
                if c = '^' then
                    let (T (s1, sl1))::(T (s2, sl2))::t = current_stack in
                    handle (pos + 1) ((T (s2, (T (s1, List.rev sl1))::sl2))::t)
                else
                    handle (pos + 1) ((T (c, []))::current_stack)
        end
    in
    handle 0 [T('^', [])]
;;
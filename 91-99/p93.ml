(* An arithmetic puzzle *)

let operators = [(( +. ), "+");(( -. ), "-");(( *. ), "*");(( /. ), "/")]
;;

let rec permute_one lhs_list rhs_list solution_list = 
    let solution_list' = if (lhs_list <> []) && (rhs_list <> []) then
        let l_res = try_cal lhs_list and r_res = try_cal rhs_list in (* map l_res and r_res *)
            List.fold_right (fun from_left o_result -> List.fold_right (fun from_right i_result -> (try_cal [from_left; from_right]) @ i_result) r_res o_result) l_res solution_list
    else solution_list in match rhs_list with
        | [] -> solution_list'
        | h::t -> permute_one (List.append lhs_list [h]) t solution_list'
and try_cal = function
    | [] -> failwith "no operand"
    | (h::[]) as u -> (* one item, return directly *)
        u
    | (anm, aex)::(bnm, bex)::[] -> (* two? try 4 elementary arithmetic operators *)
        List.map (fun (opr, sym) -> ((opr anm bnm), ("(" ^ aex ^ sym ^ bex ^ ")"))) operators 
    | l -> (* more than two? divide into two parts *)
        permute_one [] l []
;;

let judge (lnm, lex) (rnm, rex) i_result =
    if lnm = rnm then
        (lex ^ "=" ^ rex)::i_result
    else i_result
;;
   
let rec solve_one lhs_list rhs_list solution_list = 
    let solution_list' = if (lhs_list <> []) && (rhs_list <> []) then
        let lhs = try_cal lhs_list and rhs = try_cal rhs_list in
        List.fold_right (fun from_left o_result -> List.fold_right (judge from_left) rhs o_result) lhs solution_list
    else solution_list in match rhs_list with
        | [] -> solution_list'
        | h::t -> solve_one (List.append lhs_list [h]) t solution_list'
;;
   
let solve ipt = 
    solve_one [] (List.map (fun i -> (float_of_int i, string_of_int i)) ipt) []
;;

(*
Demo:
solve [2;3;5;7;11]
*)
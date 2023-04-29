#use "parser.ml"

module Typer = struct												(* For type inference *)
	open Type;;
	open Expr;;

	(* https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules *)
	let rec infer_type (env : t_expr Env.t) (expr : expr) : t_expr =
		match expr with
			| Var x -> 							(* cf. Sequent 1 *)
				begin 
					match Env.find_opt x env with
						| Some t_expr -> t_expr
						| None -> failwith "[Type error] : variable not typed"
				end
			| Int _ -> T_Int 					(* cf. Sequent 2 *)
			| Abs { param; t_param; term } -> 	(* cf. Sequent 3 *)
				let env = Env.add param t_param env in
				let t_term = infer_type env term in
				T_Arrow { t_param = t_param; t_term = t_term }
			| App { func; arg } -> 				(* cf. Sequent 4 *)
				begin
				let t_func = infer_type env func in
				let t_arg = infer_type env arg in
					match t_func with
						| T_Int -> failwith "[Type error] : T_Int is not a valid function type"
						| T_Arrow { t_param; t_term } when Type.equal t_param t_arg -> t_term
						| _ -> failwith "[Type error] : this application type is not a valid function type"
				end;;

	let print_type (t : t_expr) : unit = 
		let rec aux (t : t_expr) : unit = 
			match t with
				| T_Int -> Printf.printf "int"
				| T_Var t -> Printf.printf "%s" t
				| T_Arrow { t_param=t1; t_term=T_Arrow { t_param=t2; t_term=t3 } } -> aux t1; Printf.printf " -> ("; aux (T_Arrow { t_param=t2; t_term=t3 }); Printf.printf ")";
				| T_Arrow { t_param=t1; t_term=t2 } -> aux t1; Printf.printf " -> "; aux t2; Printf.printf ""
	in aux t; Printf.printf "\n";;
end;;
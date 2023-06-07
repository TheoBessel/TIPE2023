#use "parser.ml"
#use "utils.ml"

module Typer = struct												(* For type inference *)
	open Type;;
	open Expr;;
	open Utils;;
	open Token;;
	open Parse;;
	open Dic;;

	(* https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Typing_rules *)
	let rec infer_type (env : t_expr Env.t) (expr : expr) : t_expr =
		match expr with
			| Var x -> 										(* cf. Sequent 1 *)
				begin 
					match Env.find_opt x env with
						| Some t_expr -> t_expr
						| None -> failwith "[Type error] : variable not typed"
				end
			| Int _ -> T_Int 								(* cf. Sequent 2 *)
			| Abs { param; t_param; term } -> 				(* cf. Sequent 3 *)
				begin
					match t_param with
						| Some t_param -> let env = Env.add param t_param env in
										  let t_term = infer_type env term in
										  T_Arrow { t_param = t_param; t_term = t_term }
						| None -> let aux p =
								  	begin
								  		match Env.find_opt p env with
											| Some t_param -> t_param
											| None -> failwith "[Type error] : variable not typed"
									end
									in let t_param = aux param in
									let t_term = infer_type env term in
									T_Arrow { t_param = t_param; t_term = t_term }
				end
			| App { func; arg } -> 							(* cf. Sequent 4 *)
				begin let t_func = infer_type env func in
				let t_arg = infer_type env arg in
					match t_func with
						| T_Int -> failwith "[Type error] : T_Int is not a valid function type"
						| T_Arrow { t_param; t_term } when Type.equal t_param t_arg -> t_term
						| _ -> failwith "[Type error] : this application type is not a valid function type"
				end
			| Ty_Abs { param; term } -> let t_ret = infer_type env term in T_Forall { t_param=param; t_ret=t_ret }
			| Ty_App { func; arg } -> begin
				let t_func = infer_type env func in
					match t_func with
						| T_Forall { t_param; t_ret } -> subst t_ret t_param arg
						| _ -> failwith "[Type error] : this application type is not a valid function type"
				end;;

	let infer (filename : string) (def : string) : t_expr =
		let file = Utils.read_file filename in
		let tokenized_defs = tokenize file in
		let defs_dic = parse tokenized_defs in
		let defs_keys = Dic.get_keys defs_dic in
		let rec add_def_to_context (keys : string list) (context : Type.t_expr Env.t) : Type.t_expr Env.t =
			match keys with
				| [] -> context
				| k::_ when k=def -> context
				| k::t -> let t_expr = infer_type context (Dic.find defs_dic k) in
						  Env.add k t_expr (add_def_to_context t context) in
		let context = Env.empty |> add_def_to_context defs_keys in
		infer_type context (Dic.find defs_dic def);;

	let infer_print (filename : string) (def : string) : unit =
		print_type (infer filename def);;

	let infer_with_context (filename : string) (def : string) (context : Type.t_expr Env.t) : t_expr =
		let file = Utils.read_file filename in
		let tokenized_defs = tokenize file in
		let defs_dic = parse tokenized_defs in
		let defs_keys = Dic.get_keys defs_dic in
		let rec add_def_to_context (keys : string list) (context : Type.t_expr Env.t) : Type.t_expr Env.t =
			match keys with
				| [] -> context
				| k::_ when k=def -> context
				| k::t -> let t_expr = infer_type context (Dic.find defs_dic k) in
						  Env.add k t_expr (add_def_to_context t context) in
		let context = add_def_to_context defs_keys context in
		infer_type context (Dic.find defs_dic def);;

	let infer_print_with_context (filename : string) (def : string) (context : Type.t_expr Env.t) : unit =
		print_type (infer_with_context filename def context);;
end;;
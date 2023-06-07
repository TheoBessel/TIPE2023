#use "parser.ml"

module Value = struct												(* To define interpretater output *)
	open Expr;;

	type value =
		| V_Int of int
		| V_Closure of { term : expr; param : string; env : value Env.t }
		| V_Forall of { term : expr; env : value Env.t }
		| V_Native of (value -> value);;
end;;

module Interpreter = struct											(* For code interpretation *)
	open Type;;
	open Expr;;
	open Value;;

	let rec interpret (env : value Env.t) (expr : expr) : value =
		match expr with
			| Int n -> V_Int n
			| Var x -> Env.find x env
			| Abs { param; t_param; term } -> V_Closure { term; param; env }
			| App { func; arg } -> begin
				let arg = interpret env arg in
				match interpret env func with
					| V_Int _ -> failwith "[Type error] : V_Int is not a function"
					| V_Closure { term; param; env } -> interpret (Env.add param arg env) term
					| V_Native f -> f arg
				end
			| Ty_Abs { param; term } -> V_Forall { env; term }
    		| Ty_App { func; arg } -> begin
		        match interpret env func with
			        | V_Forall { term; env } -> interpret env term
			        | V_Int _ | V_Closure _ | V_Native _ -> failwith "[Type error] : not a function"
		    	end;;
end;;
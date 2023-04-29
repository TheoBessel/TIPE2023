module Env = Map.Make (String);;

(* Abstract syntax tree *)
module Type = struct 												(* To define types *)
	type t_expr =
		| T_Int
		| T_Var of string
		| T_Arrow of { t_param : t_expr; t_term : t_expr };;

	let rec equal (t1 : t_expr) (t2 : t_expr) : bool =
		match (t1, t2) with
			| T_Int, T_Int -> true
			| T_Var a, T_Var b -> a = b
			| T_Arrow { t_param=t_param1; t_term=t_term1 },
			  T_Arrow { t_param=t_param2; t_term=t_term2 } -> 
				(equal t_param1 t_param2) && (equal t_term1 t_term2)
			| _ -> false;;
end;;

module Expr = struct 												(* To define λ-terms *)
	open Type;;

	type expr =
		| Int of int
		| Var of string
		| Abs of { param : string; t_param : t_expr; term : expr } 	(* λparam:t_param.term *)
		| App of { func : expr; arg : expr };;						(* (func arg) *)
end;;

module Token = struct
	type token = 
		| ID_NAME of string 	(* foo *)
		| INT of int 			(* 43 *)

		| LAMBDA				(* \ *)
		| COLON					(* : *)
		| DOT					(* . *)
		| ARROW 				(* -> *)

		| LPAR					(* ( *)
		| RPAR					(* ) *)

		| EOF;;					(* EOF *)

	let token_to_string token = match token with
		| ID_NAME s  -> "ID_NAME \"" ^ s ^ "\""
		| INT s  -> "T_VAR \"" ^ string_of_int s ^ "\""

		| LAMBDA -> "LAMBDA"
		| COLON -> "COLON"
		| DOT -> "DOT"
		| ARROW -> "ARROW"
		
		| LPAR -> "LPAR"
		| RPAR -> "RPAR"
		
		| EOF -> "EOF";;

	let is_alpha c = match c with | 'a'..'z' | 'A'..'Z' -> true | _ -> false;;
	let is_digit c = match c with | '0'..'9' -> true | _ -> false;;

	let transition_table =
	let table = Hashtbl.create 10 in
		Hashtbl.add table 0 (function 
			| ' ' | '\\' | ':' | '.' | '(' | ')'-> 0
			| 'E' -> 4
			| c when is_alpha c -> 1
			| c when is_digit c -> 2
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 1 (function 
			| ' ' | '\\' | ':' | '.' | '(' | ')'-> 0
			| 'E' -> 4
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| '_' -> 1
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 2 (function 
			| ' ' | '\\' | ':' | '.' | '(' | ')'-> 0
			| 'E' -> 4
			| c when is_alpha c -> 1
			| c when is_digit c -> 2
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 3 (function 
			| '>' -> 0
			| _ -> 10
		);
		Hashtbl.add table 4 (function 
			| ' ' | '\\' | ':' | '.' | '(' | ')'-> 0
			| 'E' -> 4
			| 'O' -> 5
			| c when is_alpha c -> 1
			| c when is_digit c -> 2
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 5 (function 
			| ' ' | '\\' | ':' | '.' | '(' | ')'-> 0
			| 'E' -> 4
			| 'F' -> 6
			| c when is_alpha c -> 1
			| c when is_digit c -> 2
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 6 (function 
			| _ -> 6
		);

		Hashtbl.add table 10 (function | _ -> 10); 															(* Trap state *)
	table;;

let tokenize (str : string) : token list = 
	let n = String.length str in
	let state = ref 0 in
	let buffer = ref "" in
	let tokens = ref [] in
	let i = ref 0 in
	while !i < n do
		let c = str.[!i] in
		let nstate = try
			Hashtbl.find transition_table !state c
		with Not_found -> 10 in
		begin match nstate with
			| 0 -> begin match !state with
				| 0 -> begin
					match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| _ -> ()
					end
				| 1 -> if !buffer <> "" then tokens := (ID_NAME !buffer)::(!tokens) else (); begin
					match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| _ -> ()
					end
				| 2 -> if !buffer <> "" then tokens := (INT (int_of_string !buffer))::(!tokens) else (); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| _ -> ()
					end
				| 3 -> tokens := ARROW::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| _ -> ()
					end
				| 4 -> tokens := (ID_NAME "E")::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| _ -> ()
					end
				| 5 -> tokens := (ID_NAME "EO")::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| _ -> ()
					end
				| _ ->  ()
				end; buffer := ""; state := nstate; incr i
			| 1 -> buffer := !buffer ^ (String.make 1 c); state := nstate; incr i
			| 2 -> buffer := !buffer ^ (String.make 1 c); state := nstate; incr i
			| 3 -> buffer := ""; state := nstate; incr i
			| 4 -> buffer := ""; state := nstate; incr i
			| 5 -> buffer := ""; state := nstate; incr i
			| 6 -> state := nstate; i := n
			| 10 -> failwith "[Parsing Error] : Syntax error in parsed expression \n"
			| _ -> failwith "[Parsing Error] : Invalid state\n"
		end
	done;
	List.rev !tokens;;
end;;
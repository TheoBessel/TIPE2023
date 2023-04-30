module Env = Map.Make (String);;

(* Abstract syntax tree *)
module Type = struct 														(* To define types *)
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

module Expr = struct 														(* To define λ-terms *)
	open Type;;

	type expr =
		| Int of int
		| Var of string
		| Abs of { param : string; t_param : t_expr option; term : expr } 	(* λparam:t_param.term *)
		| App of { func : expr; arg : expr };;								(* (func arg) *)
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

		| LET 					(* let *)
		| EQUAL 				(* = *)
		| SEMICOLON 			(* ; *)

		| EOF;;					(* EOF *)

	let token_to_string token = match token with
		| ID_NAME s  -> "ID_NAME \"" ^ s ^ "\""
		| INT s  -> "INT \"" ^ string_of_int s ^ "\""

		| LAMBDA -> "LAMBDA"
		| COLON -> "COLON"
		| DOT -> "DOT"
		| ARROW -> "ARROW"
		
		| LPAR -> "LPAR"
		| RPAR -> "RPAR"

		| LET -> "LET"
		| EQUAL -> "EQUAL"
		| SEMICOLON -> "SEMICOLON"
		
		| EOF -> "EOF";;

	let rec print_token_list (tokens : token list) : unit =
		match tokens with
			| [] -> Printf.printf "\n\n"
			| h::t -> Printf.printf "%s; " (token_to_string h); print_token_list t;;

	let is_alpha c = match c with | 'a'..'z' | 'A'..'Z' -> true | _ -> false;;
	let is_digit c = match c with | '0'..'9' -> true | _ -> false;;

	let transition_table =
	let table = Hashtbl.create 10 in
		Hashtbl.add table 0 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
			| c when is_alpha c -> 1
			| c when is_digit c -> 2
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 1 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| '_' -> 1
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 2 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
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
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
			| 'O' -> 5
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 5 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
			| 'F' -> 6
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 6 (function 
			| _ -> 6
		);
		Hashtbl.add table 7 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
			| 'e' -> 8
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 8 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| 'E' -> 4
			| 'l' -> 7
			| 't' -> 9
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| '-' -> 3
			| _ -> 10
		);
		Hashtbl.add table 9 (function 
			| ' ' | '\n' | '\\' | ':' | '.' | '(' | ')' | '=' | ';' -> 0
			| c when is_alpha c -> 1
			| c when is_digit c -> 1
			| _ -> 10
		);

		Hashtbl.add table 10 (function | _ -> 10); 							(* Trap state *)
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
		(* debug :*)
		(*if !tokens <> [] then
			let t::q = !tokens in
			Printf.printf "State is %i, read char is '%c' and new state is %i, last token is : %s\n" !state c nstate (token_to_string t);
		else Printf.printf "State is %i, read char is '%c' and new state is %i, last token is : %s\n" !state c nstate "";*)
		begin match nstate with
			| 0 | 3 | 9 -> begin match !state with
				| 0 -> begin
					match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 1 -> if !buffer <> "" then tokens := (ID_NAME !buffer)::(!tokens) else (); begin
					match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 2 -> if !buffer <> "" then tokens := (INT (int_of_string !buffer))::(!tokens) else (); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 3 -> tokens := ARROW::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 4 -> tokens := (ID_NAME "E")::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 5 -> tokens := (ID_NAME "EO")::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 7 -> tokens := (ID_NAME "l")::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 8 -> if nstate = 9 then () else tokens := (ID_NAME "le")::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| 9 -> tokens := LET::(!tokens); begin match c with
						| '\\' -> tokens := LAMBDA::(!tokens)
						| ':' -> tokens := COLON::(!tokens)
						| '.' -> tokens := DOT::(!tokens)
						| '(' -> tokens := LPAR::(!tokens)
						| ')' -> tokens := RPAR::(!tokens)
						| '=' -> tokens := EQUAL::(!tokens)
						| ';' -> tokens := SEMICOLON::(!tokens)
						| _ -> ()
					end
				| _ ->  ()
				end; buffer := ""; state := nstate; incr i
			| 1 | 2 -> buffer := !buffer ^ (String.make 1 c); state := nstate; incr i
			| 4 | 5 | 7 | 8 -> buffer := ""; state := nstate; incr i
			| 6 -> tokens := EOF::(!tokens); state := nstate; i := n
			| 10 -> failwith "[Parsing Error] : Syntax error in parsed expression\n"
			| _ -> failwith "[Parsing Error] : Invalid state\n"
		end
	done;
	let h::t = !tokens in
	if h = EOF then
		List.rev !tokens
	else
		failwith "[Parsing Error] : End Of File (EOF) not found\n";;
end;;

module Dic = struct
	open Type;;
	open Expr;;
	open Token;;

	type 'a dic = Dic of { keys : string list; values : 'a list};;

	let rec find (d : 'a dic) (k : string) : 'a =
		match d with
			| Dic { keys=[]; values=[] } -> failwith "[Error] : key not found in dictionnay"
			| Dic { keys=(h::t); values=(vh::vt) } ->
				if h = k then vh
				else find (Dic { keys=t; values=vt }) k;;

	let get_keys (d : 'a dic) : string list =
		match d with
			| Dic { keys; values } -> keys;;
end;;

module Parse = struct
	open Type;;
	open Expr;;
	open Token;;
	open Dic;;

	let rec split (tokens : token list) (e : token) : ((token list) * (token list)) =
		match tokens with
			| [] -> [], []
			| h::t when h=e -> [],t
			| h::t -> let b,a = split t e in h::b,a;;

	let parse (tokens : token list) : (expr dic) =
		let rec parse_app (token : token list) (tokens : token list) : expr =
			let rec aux (tokens : token list) (acc : token list list) : expr =
				match tokens with
					| [] -> begin let rec apply (l : token list list) : expr =
							begin match l with
								| [] -> failwith "[Parsing Error] : Empty application"
								| [y] -> parse_expr y
								| y::rest -> App { func=(apply rest); arg=(parse_expr y) }
							end
						in apply acc end
					| (INT n)::rest -> aux rest ([INT n]::acc)
					| (ID_NAME name)::rest -> aux rest ([ID_NAME name]::acc)
					| LPAR::rest -> let expr, queue = split rest RPAR in
									aux queue (expr::acc)
					| _ -> failwith "[Parsing Error]"
			in match token with
				| [] -> aux tokens []
				| _ -> aux tokens [token]
		and parse_type (tokens : token list) : t_expr =
			match tokens with
				| [ID_NAME "int"] -> T_Int
				| [ID_NAME name] -> T_Var name
				| LPAR::rest ->
					let expr, queue = split rest RPAR in
					begin
						match queue with
							| [] -> parse_type expr
							| other -> let rec aux (l : token list) (acc : token list) =
										 	match l with
										 		| [] -> acc
											 	| RPAR::t -> aux t (RPAR::acc)
											 	| _ -> failwith "[Parsing Error]"
											in parse_type (expr @ (aux other []))
					end
				| rest -> let t_param, t_term = split rest ARROW in
						  T_Arrow { t_param=(parse_type t_param); t_term=(parse_type t_term) }
				| _ -> failwith "[Parsing Error]"
		and parse_expr (tokens : token list) : expr =
			match tokens with
				| [INT n] -> Int n
				| (INT _)::_ -> failwith "[Parsing Error]"
				| [ID_NAME name] -> Var name
				| [ID_NAME f; ID_NAME x] -> App { func=(Var f); arg=(Var x) }
				| (ID_NAME _)::_ -> failwith "[Parsing Error]"
				| LPAR::rest ->
					let expr, queue = split rest RPAR in
					begin
						match queue with
							| [] -> parse_app [] expr
							| RPAR::other -> let rec aux (l : token list) (acc : token list) =
										 	match l with
										 		| [] -> acc
											 	| RPAR::t -> aux t (RPAR::acc)
											in parse_app [] (expr @ (aux other [RPAR]))
							| _ -> parse_app expr queue
					end
				| LAMBDA::(ID_NAME param)::COLON::rest ->
					let t_param, term = split rest DOT in
					Abs { param=param; t_param=Some (parse_type t_param); term=(parse_expr term) }
				| LAMBDA::(ID_NAME param)::DOT::rest ->
					Abs { param=param; t_param=None; term=(parse_expr rest) } 
				| _ -> failwith "[Parsing Error] yolooo" in
		let rec parse_defs (tokens : token list) (acc1 : string list) (acc2 : expr list) : (expr dic) =
			match tokens with
				| LET::SEMICOLON::rest -> parse_defs rest acc1 acc2
				| LET::(ID_NAME name)::EQUAL::rest ->
					let def, defs = split rest SEMICOLON in
					parse_defs defs (name::acc1) ((parse_expr def)::acc2)
				| EOF::_ -> Dic { keys=(List.rev acc1); values=(List.rev acc2) }
				| _ -> failwith "[Parsing Error] : End Of File (EOF) not found or invalid definition\n"
		in parse_defs tokens [] [];;
end;;



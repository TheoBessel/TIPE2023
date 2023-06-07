#use "interpreter.ml";;
#use "typechecker.ml";;

(* example *)

open Type;;
open Expr;;
open Value;;
open Interpreter;;
open Token;;
open Parse;;
open Dic;;
open Typer;;

let context = Env.empty;;
let context = Env.add "x" (T_Var "T") context;;

print_string "id : "; infer_print_with_context "definitions.sfw" "id" context;;
print_string "perm : "; infer_print_with_context "definitions.sfw" "perm" context;;
print_string "modus : "; infer_print_with_context "definitions.sfw" "modus" context;;
print_string "trans : "; infer_print_with_context "definitions.sfw" "trans" context;;

(* System F *)

print_string "id_abs : "; infer_print_with_context "definitions.sfw" "id_abs" context;;
print_string "id_abs_eval : "; infer_print_with_context "definitions.sfw" "id_abs_eval" context;;

print_string "true : "; infer_print_with_context "definitions.sfw" "true" context;;
print_string "false : "; infer_print_with_context "definitions.sfw" "false" context;;
print_string "empty_list : "; infer_print_with_context "definitions.sfw" "empty_list" context;;
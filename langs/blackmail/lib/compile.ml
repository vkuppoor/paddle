open Ast
open Llvm
open Preamble
open Types

exception CompileError of string

let rec compile_expr = function
| Int n -> const_int type_int n  
| Prim1 (op, e) -> compile_prim1 op e

and compile_prim1 op e = match op with
| Add1 -> build_add (compile_expr e) (const_int type_int 1) "add1" builder
| Sub1 -> build_sub (compile_expr e) (const_int type_int 1) "sub1" builder


(*let compile e = build_ret (compile_expr e) builder*)
let compile e =
  let entry_ft = function_type type_int [| |] in
  let entry_function = define_function "entry" entry_ft md in
  position_at_end (entry_block entry_function) builder;
  let rv = compile_expr e in
  let _ = (build_ret rv builder) in
  md 


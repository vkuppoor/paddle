open Ast
open Llvm
open Preamble
open Types

exception CompileError of string

let rec compile_expr e fn = match e with
| Lit n -> const_int type_int n  
| Prim1 (op, e) -> compile_prim1 op e fn
| If (e1, e2, e3) ->   
    let cond_value = compile_expr e1 fn in

    let then_block = append_block context "then" fn in
    let else_block = append_block context "else" fn in
    let merge_block = append_block context "ifcont" fn in

    ignore (build_cond_br cond_value then_block else_block builder);

    position_at_end then_block builder;
    let then_value = compile_expr e2 fn in 
    ignore (Llvm.build_br merge_block builder);  (* Jump to merge block after 'then' block *)

    position_at_end else_block builder;
    let else_value = compile_expr e3 fn in
    ignore (Llvm.build_br merge_block builder);  (* Jump to merge block after 'else' block *)

    position_at_end merge_block builder;
    let phi = build_phi [(then_value, then_block); (else_value, else_block)] "iftmp" builder in 
    phi
      
(*| If (e1, e2, e3) ->   
    let cond_value = compile_expr e1 fn in
    let then_block = append_block context "then" fn in
    let then_value = compile_expr e2 fn in 
    let else_block = append_block context "else" fn in
    let else_value = compile_expr e3 fn in
    let merge_block = append_block context "ifcont" fn in
    let branch_valblock_lst = [(then_value, then_block); (else_value, else_block)] in
    let phi = build_phi branch_valblock_lst "iftmp" builder in 
    ignore (build_cond_br cond_value then_block else_block builder);
    position_at_end then_block builder;
    ignore (Llvm.build_br merge_block builder);  (* Jump to merge block after 'then' block *)
    position_at_end else_block builder;
    ignore (Llvm.build_br merge_block builder);  (* Jump to merge block after 'else' block *)
    Llvm.position_at_end merge_block builder;
    phi
*)
and compile_prim1 op e fn = match op with
| Add1 -> build_add (compile_expr e fn) (const_int type_int 1) "add1" builder
| Sub1 -> build_sub (compile_expr e fn) (const_int type_int 1) "sub1" builder


(*let compile e = build_ret (compile_expr e) builder*)
let compile e =
  let entry_ft = function_type type_int [| |] in
  let entry_function = define_function "entry" entry_ft md in
  position_at_end (entry_block entry_function) builder;
  let rv = compile_expr e entry_function in
  let _ = (build_ret rv builder) in
  md 


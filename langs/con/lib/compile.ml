open Ast
open Llvm
open Preamble
open Types

exception CompileError of string

let rec compile_expr e fn =
  match e with
  | Lit d -> (
      match d with
      | Integer n -> const_int type_int n
      | Boolean true -> val_true
      | Boolean false -> val_false)
  | Prim1 (op, e) -> compile_prim1 op e fn
  | If (e1, e2, e3) ->
      let e1_value = compile_expr e1 fn in
      let e1_type = Llvm.classify_type (Llvm.type_of e1_value) in

      (* if e1_value is not val_false, cond_val evaluates to true *)
      let cond_value =
        match e1_type with
        | Llvm.TypeKind.Integer ->
            let bitwidth = Llvm.integer_bitwidth (Llvm.type_of e1_value) in
            if bitwidth = 1 then
              build_icmp Icmp.Eq e1_value val_true "cond_value"
                builder (* e1_value is a boolean *)
            else val_true
        | _ ->
            raise (CompileError "Invalid type for condition in If expression")
      in

      let then_block = append_block context "then" fn in
      let else_block = append_block context "else" fn in
      let merge_block = append_block context "ifcont" fn in

      ignore (build_cond_br cond_value then_block else_block builder);

      position_at_end then_block builder;
      let then_value = compile_expr e2 fn in
      ignore (Llvm.build_br merge_block builder);

      (* Jump to merge block after 'then' block *)
      position_at_end else_block builder;
      let else_value = compile_expr e3 fn in
      ignore (Llvm.build_br merge_block builder);

      (* Jump to merge block after 'else' block *)
      position_at_end merge_block builder;
      let phi =
        build_phi
          [ (then_value, then_block); (else_value, else_block) ]
          "iftmp" builder
      in
      phi

and compile_prim1 op e fn =
  match op with
  | Add1 -> build_add (compile_expr e fn) (const_int type_int 1) "add1" builder
  | Sub1 -> build_sub (compile_expr e fn) (const_int type_int 1) "sub1" builder
  | IsZero ->
      let zero = const_int type_int 0 in
      build_icmp Icmp.Eq (compile_expr e fn) zero "IsZero" builder

let compile e =
  let entry_ft = function_type (pointer_type typed_value_type) [||] in
  let entry_function = define_function "entry" entry_ft md in
  position_at_end (entry_block entry_function) builder;

  let rv = compile_expr e entry_function in
  let rv_type = classify_type (type_of rv) in
  let typed_rv = match rv_type with
    | TypeKind.Integer when integer_bitwidth (type_of rv) = 1 -> 
        create_typed_value rv type_tag_bool
    | TypeKind.Integer ->
        create_typed_value rv type_tag_int
    | _ -> raise (CompileError "Unsupported return type")
  in

  let _ = build_ret typed_rv builder in
  md

(*let compile e = build_ret (compile_expr e) builder*)
(*let compile e =
  let entry_ft = function_type type_int [||] in
  let entry_function = define_function "entry" entry_ft md in
  position_at_end (entry_block entry_function) builder;
  let rv = compile_expr e entry_function in
  let _ = build_ret rv builder in
  md
*)

(*let rec compile_expr e fn =
  match e with
  | Lit d -> (match d with
      | Integer n -> const_int type_int n
      | Boolean true -> val_true
      | Boolean false -> val_false)
  | Prim1 (op, e) -> compile_prim1 op e fn
  | If (e1, e2, e3) ->
      let e1_value = compile_expr e1 fn in
      let e1_type = Llvm.classify_type (Llvm.type_of e1_value) in

      (* if e1_value is not val_false, cond_val evaluates to true *)
      let cond_value =
        match e1_type with
        | Llvm.TypeKind.Integer ->
            let bitwidth = Llvm.integer_bitwidth (Llvm.type_of e1_value) in
            if bitwidth = 1 then
              build_icmp Icmp.Eq e1_value val_true "cond_value"
                builder (* e1_value is a boolean *)
            else val_true
        | _ ->
            raise (CompileError "Invalid type for condition in If expression")
      in
      (* let cond_value =
           match type_e e1_type type_bool with
           | true -> build_icmp Icmp.Eq e1_value val_true "cond_value" builder
           | _ -> build_icmp Icmp.Eq e1_value val_true "cond_value" builder
         in
      *)
      let then_block = append_block context "then" fn in
      let else_block = append_block context "else" fn in
      let merge_block = append_block context "ifcont" fn in

      ignore (build_cond_br cond_value then_block else_block builder);

      position_at_end then_block builder;
      let then_value = compile_expr e2 fn in
      ignore (Llvm.build_br merge_block builder);

      (* Jump to merge block after 'then' block *)
      position_at_end else_block builder;
      let else_value = compile_expr e3 fn in
      ignore (Llvm.build_br merge_block builder);

      (* Jump to merge block after 'else' block *)
      position_at_end merge_block builder;
      let phi =
        build_phi
          [ (then_value, then_block); (else_value, else_block) ]
          "iftmp" builder
      in
      phi*)

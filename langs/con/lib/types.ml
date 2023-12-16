open Llvm
open Preamble

let type_int = integer_type context 64 
let type_bool = i1_type context 

(* Type tag definitions *)
let type_tag_int = 0
let type_tag_bool = 1

(* Struct type for typed values *)
let typed_value_type = struct_type context [| i32_type context; i64_type context |]

(* Helper functions to create a typed value *)
let create_typed_value value type_tag =
  let typed_value = build_malloc typed_value_type "typedval" builder in
  let type_ptr = build_struct_gep typed_value 0 "typeptr" builder in
  ignore (build_store (const_int (i32_type context) type_tag) type_ptr builder);
  let value_ptr = build_struct_gep typed_value 1 "valueptr" builder in
  ignore (build_store value value_ptr builder);
  typed_value

let create_int_typed_value n = create_typed_value (const_int type_int n) type_tag_int
let create_bool_typed_value n = create_typed_value (const_int type_bool n) type_tag_bool

(* Common values *)
(* let val_true = create_int_typed_value 1 *)
let val_true = const_int type_bool 1
(* let val_false = create_int_typed_value 0 *)
let val_false = const_int type_bool 0

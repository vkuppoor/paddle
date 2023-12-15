type op1 = Add1 | Sub1
type expr = Int of int | Prim1 of op1 * expr

let rec string_of_expr e =
  match e with
  | Int n -> "Int(" ^ string_of_int n ^ ")"
  | Prim1 (op, e) ->
      let op_str = match op with Add1 -> "Add1" | Sub1 -> "Sub1" in
      "Prim1(" ^ op_str ^ ", " ^ string_of_expr e ^ ")"

let print_ast ast = print_endline (string_of_expr ast)

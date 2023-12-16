type op1 = Add1 | Sub1 | IsZero
type datum = Integer of int | Boolean of bool
type expr = Lit of datum | Prim1 of op1 * expr | If of expr * expr * expr

let rec string_of_expr e =
  match e with
  | Lit d -> (match d with
    | Integer n -> "Int(" ^ string_of_int n ^ ")"
    | Boolean b -> "Boolean(" ^ string_of_bool b ^ ")")
  | Prim1 (op, e) ->
      let op_str = match op with Add1 -> "Add1" | Sub1 -> "Sub1" | IsZero -> "IsZero" in
      "Prim1(" ^ op_str ^ ", " ^ string_of_expr e ^ ")"
  | If (e1, e2, e3) ->
      "If(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", "
      ^ string_of_expr e3 ^ ")"

let print_ast ast = print_endline (string_of_expr ast)

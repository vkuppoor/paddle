type expr = Int of int

let string_of_expr e = match e with Int n -> "Int(" ^ string_of_int n ^ ")"
let print_ast ast = print_endline (string_of_expr ast)

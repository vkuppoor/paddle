open Lang

let sample = Parse.parse "2"
let () = Ast.print_ast sample

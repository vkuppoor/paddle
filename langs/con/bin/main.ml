open Lang

let example = Parse.parse "2"
let () = 
  let md = Compile.compile example in
  print_endline (Llvm.string_of_llmodule md)

open Llvm

let context = global_context ()
let md = create_module context "con context"
let builder = builder context

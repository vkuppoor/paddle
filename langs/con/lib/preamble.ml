open Llvm

let context = global_context ()
let md = create_module context "abscond context"
let builder = builder context

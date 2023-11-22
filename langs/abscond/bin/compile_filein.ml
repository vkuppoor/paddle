open Lang
open Printf

let compile_rkt_file file_path =
  if Filename.extension file_path = ".rkt" then
    try
      let channel = open_in file_path in
      let file_content = really_input_string channel (in_channel_length channel) in
      close_in channel;
      
      let sample = Parse.parse file_content in
      let md = Compile.compile sample in
      
      (* Extract the file name without extension *)
      let base_name = Filename.chop_extension (Filename.basename file_path) in
      
      (* Output LLVM module content to a file with the same name and .ll extension *)
      let out_file_path = base_name ^ ".ll" in
      let out_channel = open_out out_file_path in
      fprintf out_channel "%s" (Llvm.string_of_llmodule md);
      close_out out_channel;
      
      Printf.printf "LLVM module written to %s\n" out_file_path;
    with
    | Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | _ -> Printf.eprintf "An error occurred while processing the file.\n"
  else
    Printf.eprintf "File %s is not a .rkt file.\n" file_path

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file_path.rkt>\n" Sys.argv.(0)
  else
    compile_rkt_file Sys.argv.(1)
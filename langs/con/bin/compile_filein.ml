open Lang
open Printf

let compile_rkt_file file_path =
  if Filename.extension file_path = ".rkt" then
    try
      let channel = open_in file_path in
      let rec skip_first_non_whitespace_line () =
        let line = input_line channel in
        if String.trim line <> "" then () else skip_first_non_whitespace_line ()
      in
      (* Skip the first non-whitespace line *)
      skip_first_non_whitespace_line ();

      (* Read and process the rest of the file *)
      let file_content =
        really_input_string channel (in_channel_length channel - pos_in channel)
      in
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

      Printf.printf "LLVM module written to %s\n" out_file_path
    with
    | Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | e ->
        Printf.eprintf "An error occurred while processing the file: %s\n"
          (Printexc.to_string e)
  else Printf.eprintf "File %s is not a .rkt file.\n" file_path

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file_path.rkt>\n" Sys.argv.(0)
  else compile_rkt_file Sys.argv.(1)

(* the following function has incorrect regex *)
(*let compile_rkt_file_with_regex file_path =
  if Filename.extension file_path = ".rkt" then
    try
      let channel = open_in file_path in

      (* Read the first non-whitespace line *)
      let rec read_first_non_whitespace_line chan =
        let line = input_line chan in
        if String.trim line = "" then read_first_non_whitespace_line chan else line
      in
      let first_line = read_first_non_whitespace_line channel in

      (* Check if the first line is "#lang racket" *)
      if
        not
          (Str.string_match
             (Str.regexp "#lang racket([ \n\r\x0c\t]+|)")
             first_line 0)
      then failwith "The file does not start with '#lang racket'";

      let file_content =
        really_input_string channel (in_channel_length channel - pos_in channel)
      in
      close_in channel;

      let parse_to_ast = Parse.parse file_content in
      let md = Compile.compile parse_to_ast in

      (* Extract the file name without extension *)
      let base_name = Filename.chop_extension (Filename.basename file_path) in

      (* Output LLVM module content to a file with the same name and .ll extension *)
      let out_file_path = base_name ^ ".ll" in
      let out_channel = open_out out_file_path in
      fprintf out_channel "%s" (Llvm.string_of_llmodule md);
      close_out out_channel;

      Printf.printf "LLVM module written to %s\n" out_file_path
    with
    | Sys_error msg -> Printf.eprintf "Error: %s\n" msg
    | Failure msg -> Printf.eprintf "%s\n" msg
    | _ -> Printf.eprintf "An error occurred while processing the file.\n"
  else Printf.eprintf "File %s is not a .rkt file.\n" file_path*)

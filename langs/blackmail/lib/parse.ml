open Parsexp
open Sexplib0
open Ast

exception ParseError of string

let parse_op1 op = match op with
  | "add1" -> Add1
  | "sub1" -> Sub1
  | _ -> raise (ParseError "Unknown operator")

let rec parse_to_ast (sexpr : Sexp.t) =
  match sexpr with
  | Sexp.Atom s -> (
      try Int (int_of_string s)
      with Failure _ -> raise (ParseError "Parse error"))
  | Sexp.List [Sexp.Atom op; e] ->
        Prim1 (parse_op1 op, parse_to_ast e)
  | Sexp.List _ -> raise (ParseError "Parse error")

let parse (input : string) : expr =
  match Single.parse_string input with
  | Ok s -> parse_to_ast s
  | Error _ -> raise (ParseError "Parse sexp error")

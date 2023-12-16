open Parsexp
open Sexplib0
open Ast

exception ParseError of string

let parse_op1 op =
  match op with
  | "add1" -> Add1
  | "sub1" -> Sub1
  | "zero?" -> IsZero
  | _ -> raise (ParseError "Unknown operator")

let rec parse_to_ast (sexpr : Sexp.t) =
  match sexpr with
  | Sexp.Atom s -> (
      match s with
      | "#t" -> Lit (Boolean true)
      | "#f" -> Lit (Boolean false)
      | _ -> (
          try Lit (Integer (int_of_string s))
          with Failure _ -> raise (ParseError "Parse error")))
  | Sexp.List [ Sexp.Atom op; e ] -> Prim1 (parse_op1 op, parse_to_ast e)
  | Sexp.List [ Sexp.Atom "if"; e1; e2; e3 ] ->
      If (parse_to_ast e1, parse_to_ast e2, parse_to_ast e3)
  | Sexp.List _ -> raise (ParseError "Parse error")

let parse (input : string) : expr =
  match Single.parse_string input with
  | Ok s -> parse_to_ast s
  | Error _ -> raise (ParseError "Parse sexp error")

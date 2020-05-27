open MJ
open Printf
open Print

let typ2c () = function
    | TypInt -> sprintf "int"
    | TypBool -> sprintf "int"
    | Typ id -> sprintf "struct %s" id
    | _ -> sprintf "###"

let classattr2c () (name, typ) =
  sprintf "%s %s" (typ2c () typ) name

let classdef2c () (name, c) =
  sprintf "struct %s {%s};" name ((termlist semicolon classattr2c) () (StringMap.to_association_list c.attributes))

let constant2c () = function
    | ConstBool true -> sprintf "0"
    | ConstBool false -> sprintf "1"
    | ConstInt i -> sprintf "%ld" i

let binop2c = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpLt  -> "<"
  | OpAnd -> "&&"
  | OpEquals -> "=="

let rec expr0 () = function
  | EConst c -> sprintf "%a" constant2c c
  | EGetVar x -> sprintf "%s" x
  | EThis -> "###"
  | EMethodCall (o, c, es) -> sprintf "%a.%s(%a)" expr0 o c (seplist comma expr2c) es
  | EArrayGet (ea, ei) -> sprintf "%a[%a]" expr0 ea expr2c ei
  | EArrayLength e -> "###"
  | EObjectAlloc id -> "###"
  | e -> sprintf "(%a)" expr2c e

and expr1 () = function
  | EArrayAlloc e -> "###"
  | e -> expr0 () e

and expr2 () = function
  | EUnOp (UOpNot, e) -> sprintf "!%a" expr2 e
  | e -> expr1 () e

and expr3 () = function
  | EBinOp (OpMul as op, e1, e2) -> sprintf "%a %s %a" expr3 e1 (binop2c op) expr3 e2
  | e -> expr2 () e

and expr4 () = function
  | EBinOp (OpSub as op, e1, e2) -> sprintf "%a %s %a" expr4 e1 (binop2c op) expr3 e2
  | e -> expr3 () e

and expr5 () = function
  | EBinOp (OpAdd as op, e1, e2) -> sprintf "%a %s %a" expr5 e1 (binop2c op) expr5 e2
  | e -> expr4 () e

and expr6 () = function
  | EBinOp ((OpLt | OpAnd | OpEquals) as op, e1, e2) -> sprintf "%a %s %a" expr6 e1 (binop2c op) expr6 e2
  | e -> expr5 () e

and expr2c () e = expr6 () e

let rec instr2c () = function
| ISetVar (x, e) -> sprintf "%s = %a;" x expr2c e
| IArraySet (id, ei, ev) -> sprintf "%s[%a] = %a;" id expr2c ei expr2c ev
| IIf (c, i1, i2) ->
  sprintf "if (%a) %a%telse %a"
    expr2c c
    instr2c i1
    nl
    instr2c i2
| IIfNoElse (c, i1) ->
  sprintf "if (%a) %a"
    expr2c c
    instr2c i1
| IWhile (c, i) ->
  sprintf "while (%a) %a"
    expr2c c
    instr2c i
| IFor (e1, e2 , e3, i) ->
    sprintf "for (%a;%a;%a) %a"
      instr2c e1
      expr2c e2
      instr2c e3
      instr2c i
| IBlock is -> sprintf "{%a%t}" (seplist nl instr2c) is nl
| ISyso e -> sprintf "printf(%a);printf(\"\\n\");" expr2c e
| IIncrement i -> sprintf "%s++;" i

let program2c (p : MJ.program) : unit =
  Printf.fprintf stdout "#include <stdio.h>\n\
#include <stdlib.h>\n\
%s
int main(int argc, char *argv[]) {
%s
}\n"
(seplist nl classdef2c () (StringMap.to_association_list p.defs))
(instr2c () p.main)

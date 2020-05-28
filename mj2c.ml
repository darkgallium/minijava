open MJ
open Printf
open Print

(*
  Custom transpiler minijava -> C

  Limitations :
  - Does not work well with allocation + method call inline
  - No indentation
*)

let typ2c () = function
    | TypInt -> "int"
    | TypBool -> "int"
    | Typ id -> sprintf "struct %s*" id
    | TypIntArray -> "int*"

let constant2c () = function
    | ConstBool true -> "0"
    | ConstBool false -> "1"
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
  | EThis -> "this"
  | EMethodCall (o, c, es) -> sprintf "%a->%s(%a%a)" expr0 o c expr0 o (preclist comma expr2c) es
  | EArrayGet (ea, ei) -> sprintf "%a[%a]" expr0 ea expr2c ei
  | EArrayLength e -> sprintf "(sizeof( (%a) )/sizeof(int))" expr0 e
  | EObjectAlloc id -> sprintf "({
    struct %s* obj = malloc(sizeof(struct %s));\
    obj->vtable = %s_vtable;\
    (struct %s*) obj;\
    })" id id id id
  | e -> sprintf "(%a)" expr2c e

and expr1 () = function
  (* TODO: trick borrowed from Pascal's transpiler : store the array in a struct alongside with length *)
  | EArrayAlloc e -> sprintf "malloc(sizeof(int)*(%a))" expr0 e
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
| ISyso e -> sprintf "printf(\"%%d\", %a);printf(\"\\n\");" expr2c e
| IIncrement i -> sprintf "%s++;" i

let locals2c () (name, typ) =
  sprintf "%s %s;" (typ2c () typ) name

let formals2c () (name, typ) =
  sprintf "%s %s" (typ2c () typ) name

let method2c cn () (name, m) =
  sprintf "%s %s_%s(struct %s* this%s){\n%s%s\nreturn %s;\n}\n"
    (typ2c () m.result)
    cn
    name
    cn
    ((preclist comma formals2c) () m.formals)
    ((termlist nl locals2c) () (StringMap.to_association_list m.locals))
    (instr2c () m.body)
    (expr2c () m.return)

let classattr2c () (name, typ) =
  sprintf "%s %s;" (typ2c () typ) name

(* vtable stuff based of https://gist.github.com/michahoiting/1aec1c95881881add9a20e9839c35cec*)

let allocVtable cn methods =
  let enumFields cn () (name, _) =
    sprintf "Call_%s_%s" cn name
  in
  let vtableFields cn () (name, _) =
    sprintf "%s_%s" cn name
  in
  if not (StringMap.is_empty methods) then
  begin
    sprintf "enum {%s};\nvoid (*%s_vtable[])() = {%s};\n"
    ((seplist comma (enumFields cn)) () (StringMap.to_association_list methods))
    cn
    ((seplist comma (vtableFields cn)) () (StringMap.to_association_list methods))
  end
  else sprintf "void (*%s_vtable[])() = {};" cn

let classdef2c () (name, c) =
  sprintf "struct %s {\nvoid (**vtable)();\n%s};\n%s\n%s\n"
    name
    ((termlist semicolon classattr2c) () (StringMap.to_association_list c.attributes))
    ((termlist nl (method2c name)) () (StringMap.to_association_list c.methods))
    (allocVtable name c.methods)

let program2c (p : MJ.program) : unit =
  Printf.fprintf stdout "#include <stdio.h>\n\
#include <stdlib.h>\n\
#pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"\n\
#pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"\n\
%s
int main(int argc, char *argv[]) {
%s
%s
}\n"
(seplist nl classdef2c () (StringMap.to_association_list p.defs))
(termlist nl locals2c () p.main_locals)
(instr2c () p.main)

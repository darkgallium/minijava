open Printf
open Print
open MJ

let constant () = function
  | ConstBool true -> "true"
  | ConstBool false -> "false"
  | ConstInt i -> sprintf "%ld" i

let binop = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpLt  -> "<"
  | OpAnd -> "&&"
  | OpEquals -> "=="

let rec expr0 () = function
  | EConst c -> sprintf "%a" constant c
  | EGetVar x -> sprintf "%s" x
  | EGetMember (e, x) -> sprintf "%s.%s" (expr () e) x
  | EThis -> "this"
  | EMethodCall (o, c, es) -> sprintf "%a.%s(%a)" expr0 o c (seplist comma expr) es
  | EArrayGet (ea, ei) -> sprintf "%a[%a]" expr0 ea expr ei
  | EArrayLength e -> sprintf "%a.length" expr0 e
  | EObjectAlloc id -> sprintf "new %s()" id
  | e -> sprintf "(%a)" expr e

and expr1 () = function
  | EArrayAlloc e -> sprintf "new int[%a]" expr e
  | e -> expr0 () e

and expr2 () = function
  | EUnOp (UOpNot, e) -> sprintf "!%a" expr2 e
  | e -> expr1 () e

and expr3 () = function
  | EBinOp (OpMul as op, e1, e2) -> sprintf "%a %s %a" expr3 e1 (binop op) expr3 e2
  | e -> expr2 () e

and expr4 () = function
  | EBinOp (OpSub as op, e1, e2) -> sprintf "%a %s %a" expr4 e1 (binop op) expr3 e2
  | e -> expr3 () e

and expr5 () = function
  | EBinOp (OpAdd as op, e1, e2) -> sprintf "%a %s %a" expr5 e1 (binop op) expr5 e2
  | e -> expr4 () e

and expr6 () = function
  | EBinOp ((OpLt | OpAnd | OpEquals) as op, e1, e2) -> sprintf "%a %s %a" expr6 e1 (binop op) expr6 e2
  | e -> expr5 () e

and expr () e = expr6 () e

let indentation = 2

let rec for_instr () = function
  | ISetVar (x, e) -> sprintf "%s = %a" x expr e
  | IArraySet (id, ei, ev) -> sprintf "%s[%a] = %a" id expr ei expr ev
  | IIncrement i -> sprintf "%s++" i
  | _ -> ""

let rec instr () = function
  | ISetVar (x, e) -> sprintf "%s = %a;" x expr e
  | IThisSetVar (x, e) -> sprintf "this.%s = %a;" x expr e
  | IThisArraySet (id, ei, ev) -> sprintf "this.%s[%a] = %a;" id expr ei expr ev
  | IArraySet (id, ei, ev) -> sprintf "%s[%a] = %a;" id expr ei expr ev
  | IIf (c, i1, i2) ->
      sprintf "if (%a) %a%telse %a"
        expr c
        instr i1
        nl
        instr i2
  | IIfNoElse (c, i1) ->
      sprintf "if (%a) %a"
        expr c
        instr i1
  | IWhile (c, i) ->
      sprintf "while (%a) %a"
        expr c
        instr i
  | IFor (e1, e2 , e3, i) ->
      sprintf "for (%a; %a; %a) %a"
        for_instr e1
        expr e2
        for_instr e3
        instr i
  | IBlock is -> sprintf "{%a%t}" (indent indentation (seplist nl instr)) is nl
  | ISyso e -> sprintf "System.out.println(%a);" expr e
  | IIncrement i -> sprintf "%s++;" i

let typ () = function
  | TypInt -> "int"
  | TypBool -> "boolean"
  | TypIntArray -> "int[]"
  | Typ id -> id

let binding () (x, t) = sprintf "%a %s" typ t x

let metho () (name, m) =
  sprintf "public %a %s(%a) {%a%a%a%t}"
    typ m.result
    name
    (seplist comma binding) m.formals
    (termlist semicolon (indent indentation binding)) (StringMap.to_association_list m.locals)
    (list (indent indentation instr)) (match m.body with | IBlock is -> is | _ -> assert false)
    (indent indentation (fun () -> sprintf "return %a;" expr)) m.return
    nl

let clas () (name, c) =
  (match c.extends with
  | None -> sprintf "class %s {%a%a%t}" name
  | Some class_name -> sprintf "class %s extends %s {%a%a%t}" name class_name)
    (termlist semicolon (indent indentation binding)) (StringMap.to_association_list c.attributes)
    (list (indent indentation metho)) (StringMap.to_association_list c.methods)
    nl

let print_program p =
  Printf.fprintf stdout "%s\n%!"
    (
      sprintf "class %s {%a%t}%t%a"
        p.name
        (indent indentation (fun () -> sprintf "public static void main(String[] %s) {%a%a%t}"
          p.main_args
          (termlist semicolon (indent indentation binding)) p.main_locals
          (indent indentation instr) p.main
        )) nl
        nl
        nl
        (seplist nl clas) (StringMap.to_association_list p.defs)
    )

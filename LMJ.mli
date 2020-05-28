type identifier = string Location.t

type expression = raw_expression Location.t

and raw_expression =
| EConst of constant
| EGetVar of identifier
| EUnOp of unop * expression
| EBinOp of binop * expression * expression
| EMethodCall of expression * identifier * expression list
| EArrayGet of expression * expression
| EArrayAlloc of expression
| EArrayLength of expression
| EThis
| EObjectAlloc of identifier

and constant =
| ConstBool of bool
| ConstInt of int32

and binop =
| OpAdd
| OpSub
| OpMul
| OpLt
| OpAnd
| OpEquals

and unop = UOpNot

and instruction =
| IBlock of instruction list
| IIf of expression * instruction * instruction
| IIfNoElse of expression * instruction
| IWhile of expression * instruction
| IFor of instruction * expression * instruction * instruction
| ISyso of expression
| ISetVar of identifier * expression
| IArraySet of identifier * expression * expression
| IIncrement of identifier

and typ =
| TypInt
| TypBool
| TypIntArray
| Typ of identifier

and metho = {

  formals: (identifier * typ) list;

  result: typ;

  locals: (identifier * typ) list;

  body: instruction;

  return: expression

}

and clas = {

  extends: identifier option;

  attributes: (identifier * typ) list;

  methods: (identifier * metho) list

}

and program = {

  name: identifier;

  defs: (identifier * clas) list;

  main_args: identifier;

  main_locals: (identifier * typ) list;

  main: instruction

}

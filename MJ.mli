type identifier = string

type expression =
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

and constant = LMJ.constant =
| ConstBool of bool
| ConstInt of int32

and binop = LMJ.binop =
| OpAdd
| OpSub
| OpMul
| OpLt
| OpAnd

and unop = LMJ.unop = UOpNot

and instruction =
| IBlock of instruction list
| IIf of expression * instruction * instruction
| IWhile of expression * instruction
| ISyso of expression
| ISetVar of identifier * expression
| IArraySet of identifier * expression * expression

and typ =
| TypInt
| TypBool
| TypIntArray
| Typ of identifier

and metho = {

  formals: (string * typ) list;

  result: typ;

  locals: typ StringMap.t;

  body: instruction;

  return: expression

}

and clas = {

  extends: string option;

  attributes: typ StringMap.t;

  methods: metho StringMap.t;

}

and program = {

  name: string;

  defs: clas StringMap.t;

  main_args: string;

  main: instruction

}

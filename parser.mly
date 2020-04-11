%{
  open LMJ
  let swap = List.map ( fun (e1, e2) -> (e2, e1) )
%}

%token <int32> INT_CONST
%token <bool> BOOL_CONST
%token INTEGER BOOLEAN
%token <string Location.t> IDENT
%token CLASS PUBLIC STATIC VOID MAIN STRING EXTENDS RETURN
%token PLUS MINUS TIMES NOT LT AND
%token COMMA SEMICOLON
%token ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token THIS NEW DOT LENGTH
%token SYSO
%token IF ELSE WHILE
%token EOF

%nonassoc LT
%left PLUS MINUS
%left TIMES AND
%left NOT
%left LBRACKET DOT

%start program

%type <LMJ.program> program

%%

program:
| m = main_class d = defs EOF
   {
     let c, a, i = m in
     {
       name = c;
       defs = d;
       main_args = a;
       main = i
     }
   }

main_class:
| CLASS c = IDENT
   LBRACE
   PUBLIC STATIC VOID MAIN LPAREN STRING LBRACKET RBRACKET a = IDENT RPAREN
   LBRACE
   i = instruction
   RBRACE
   RBRACE
   { (c, a, i) }

defs:
| c = list(clas)
   { c }

clas:
| CLASS id1 = IDENT id2 = option(preceded(EXTENDS, IDENT)) LBRACE lvd = list(var_declaration) lmd = list(metho) RBRACE
   { let m = List.map (fun el -> (id1, el)) lmd in id1, { extends = id2 ; attributes = swap lvd ; methods = m } }

metho:
| PUBLIC typ1 = typ id1 = IDENT
   LPAREN
   lf = separated_list(COMMA, pair(typ, IDENT))
   RPAREN
   LBRACE
   ds = declarations_and_statements
   RETURN e = expression SEMICOLON
   RBRACE
   {
      let d, s = fst ds, snd ds in
      { formals = swap lf; result = typ1 ; locals = d ; body = IBlock(s) ; return = e }
   }

/*| PUBLIC typ1 = typ id = IDENT LPAREN
   RPAREN LBRACE lvd = list(var_declaration)
   i = instruction RETURN e = expression SEMICOLON RBRACE
   { { formals = []; result = typ1 ; locals = swap lvd ; body = i ; return = e } }*/


// retour : ( liste de tuples représentant les variables format (ident, type), liste d'instructions)
declarations_and_statements:
// variable declaration : type + ident + semicolon
| t = typ id = IDENT SEMICOLON r = declarations_and_statements
   {
     let d, s = r in
     ((id, t) :: d, s)
   }
// statements
| s = list(instruction)
   { ([], s) }

typ:
| INTEGER
   { TypInt }
| INTEGER LBRACKET RBRACKET
   { TypIntArray }
| BOOLEAN
   { TypBool }
| id = IDENT
   { Typ id }

var_declaration:
| t = typ id = IDENT SEMICOLON
   { (t, id) }

expression:
|  e = raw_expression
   { Location.make $startpos $endpos e }
| LPAREN e = expression RPAREN
   { e }

integer:
| i = INT_CONST
   { ConstInt i }

raw_expression:
| i = integer
   { EConst (i) }
| bool = BOOL_CONST
   { EConst(ConstBool bool) }
| THIS
   { EThis }
| id1 = IDENT
   { EGetVar id1 }
| NEW INTEGER LBRACKET e = expression RBRACKET
   { EArrayAlloc(e)}
| NEW id = IDENT LPAREN RPAREN
   { EObjectAlloc(id)}
| NOT e = expression
   { EUnOp(UOpNot, e) }
| e1 = expression AND AND e2 = expression
   { EBinOp(OpAnd, e1, e2) }
| e1 = expression LT e2 = expression
   { EBinOp(OpLt, e1, e2) }
| e1 = expression PLUS e2 = expression
   { EBinOp(OpAdd, e1, e2) }
| e1 = expression MINUS e2 = expression
   { EBinOp(OpSub, e1, e2) }
| e1 = expression TIMES e2 = expression
   { EBinOp(OpMul, e1, e2) }
| e1 = expression LBRACKET e2 = expression RBRACKET
   { EArrayGet(e1, e2) }
| e = expression DOT LENGTH
   { EArrayLength(e) }
| e = expression DOT id = IDENT LPAREN sl = separated_list(COMMA, expression) RPAREN
   { EMethodCall(e, id, sl) }

instruction:
| SYSO LPAREN e = expression RPAREN SEMICOLON
   { ISyso e }
| LBRACE l = list(instruction) RBRACE
   { IBlock l }
| IF LPAREN e = expression RPAREN i1 = instruction
   ELSE i2 = instruction
   { IIf (e, i1, i2) }
| IF LPAREN e = expression RPAREN i = instruction
   { IIfNoElse (e, i) }
| WHILE LPAREN e = expression RPAREN i = instruction
   { IWhile (e, i) }
| id = IDENT LBRACKET e1 = expression RBRACKET ASSIGN e2 = expression SEMICOLON
   { IArraySet (id, e1, e2) }
| id = IDENT ASSIGN e = expression SEMICOLON
   { ISetVar (id, e) }

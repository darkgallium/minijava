open Lexing

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let usage = "usage: mini-java file.java"

let () =
  Arg.parse [] (set_file ifile) usage;

  if !ifile = "" then begin Printf.printf "no input file\n"; exit 1 end;

  if not (Filename.check_suffix !ifile ".java") then begin
    Printf.printf "filename must have .java suffix.\n";
    Arg.usage [] usage;
    exit 1
  end;

  ofile := (Filename.chop_extension !ifile) ^ ".c";

  let f = open_in !ifile in

  let lexbuf = Lexing.from_channel f in
  lexbuf.Lexing.lex_curr_p <-
    {
      Lexing.pos_fname = !ifile;
      Lexing.pos_lnum  = 1;
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = 0
    };
  try
    let program = Parser.program Lexer.get_token lexbuf in
    Typechecking.typecheck_program program;
    let mj = Lmj2mj.translate_program program in
    Printf.printf "/*\n";
    PrintMJ.print_program mj;
    Printf.printf "*/\n";
    Mj2c.program2c mj;
    close_in f;
    exit 0
  with
    | Lexer.Error msg ->
        Printf.printf "Lexical error %s:\n%s.\n" (Error.position (Lexing.lexeme_start_p lexbuf)) msg;
        exit 1
    | Parser.Error ->
        Printf.printf "Syntax error %s.\n" (Error.position (Lexing.lexeme_start_p lexbuf));
        exit 1
    | Typechecking.Error msg ->
        Printf.printf "Type checking error %s.\n" msg;
        exit 1

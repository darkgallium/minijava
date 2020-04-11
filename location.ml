type 'a t = {
  startpos: Lexing.position;
  endpos: Lexing.position;
  content: 'a
}

let make startpos endpos content = {
  startpos = startpos;
  endpos = endpos;
  content = content
}

let content { content = c } = c

let startpos { startpos = pos } = pos

let endpos { endpos = pos } = pos

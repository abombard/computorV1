{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n']                               { token lexbuf }
| ['-' '+']? "X" '^' ['0'-'2']+ as var          { VAR (
    let (sign, index_cur) = match String.get var 0 with
        | '+' -> ("+", 1)
        | '-' -> ("-", 1)
        | _   -> ("+", 0)
    in
    let index_end = String.index var '^' in
    let degree = String.sub var (index_end+1) (String.length var - (index_end+1)) in
    (sign, degree)
)}
| ['-' '+']? ['0'-'9']+ '.'? ['0'-'9']* as coef { COEF ( coef ) }
| '+'                                           { PLUS }
| '-'                                           { MINUS }
| '*'                                           { TIMES }
| '='                                           { EQUAL }
| eof                                           { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }


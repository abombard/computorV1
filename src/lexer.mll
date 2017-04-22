{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n']                               { token lexbuf }
| ['-' '+']? 'X' ('^' ['0'-'2']+)? as var       { VAR (
    let (sign, index_cur) = match String.get var 0 with
        | '+' -> ("+", 1)
        | '-' -> ("-", 1)
        | _   -> ("+", 0)
    in
    let len = String.length var - 1 in
    let index_degree = try (String.index var '^') + 1 with _ -> len in
    let degree =
        if index_degree = len then "1"
        else String.sub var (index_degree) (len - (index_degree)) in
    (sign, degree)
)}
| ['-' '+']? ['0'-'9']+ '.'? ['0'-'9']* as coef { COEF ( coef ) }
| '+'                                           { PLUS }
| '-'                                           { MINUS }
| '*'                                           { TIMES }
| '='                                           { EQUAL }
| eof                                           { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }


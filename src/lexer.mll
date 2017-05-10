{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t' '\n']                               { token lexbuf }
| ['-' '+']? 'X' ('^' ['0'-'9']+)? as var       { VAR (
    let (sign, index_cur) = match String.get var 0 with
        | '+' -> ("+", 1)
        | '-' -> ("-", 1)
        | _   -> ("+", 0)
    in
    let len = String.length var in
    let degree = (
        try
            let index = (String.index var '^') + 1 in
            String.sub var index (len - index)
        with _ -> "1"
    ) in
    (sign, degree)
)}
| ['-' '+']? ['0'-'9']+ '.'? ['0'-'9']* as coef { COEF ( coef ) }
| '+'                                           { PLUS }
| '-'                                           { MINUS }
| '*'                                           { TIMES }
| '='                                           { EQUAL }
| eof                                           { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }


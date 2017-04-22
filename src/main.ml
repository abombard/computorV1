
let display eq =
  List.iter (fun t -> Printf.printf "%s + " (Eq.to_string t)) eq;
  print_endline " = 0"

let main () =
    let buf = Lexing.from_string Sys.argv.(1) in
    try
        let eq = (Parser.main Lexer.token buf) in
        (* display eq; *)
        display eq
    with
    | Lexer.Error msg -> Printf.eprintf "%s%!" msg
    | Parser.Error -> Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start buf)


let _ = main ()

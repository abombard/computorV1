
let rec display eq =
    let rec aux eq s = match eq with
        | [] -> ""
        | e :: [] -> s ^ Eq.Expr.to_string e ^ " = 0"
        | e :: tail -> aux tail (s ^ Eq.Expr.to_string e ^ " + ")
    in print_endline (aux eq "")

let solve eq = match eq with
    | (a, _) :: (b, _) :: (c, _) :: [] -> begin
        let delta = b *. b -. 4. *. a *. c in
        match delta with
        | delta when delta < 0. -> print_endline "Pas de solution"
        | 0.                    -> Printf.printf "une solution x=%.2g\n" (-.b /. (2. *. a))
        | delta                 -> begin
            let x1 = (-.b -. sqrt delta) /. (2. *. a) in
            let x2 = (-.b +. sqrt delta) /. (2. *. a) in
            Printf.printf "deux solutions x1=%.2g x2=%.2g\n" x1 x2
        end
    end
    | _ -> invalid_arg ""

let main () =
    let buf = Lexing.from_string Sys.argv.(1) in
    try
        let eq = (Parser.main Lexer.token buf) in
        (* display eq; *)
        let eq = Eq.reduce eq in
        display eq;
        solve eq
    with
    | Lexer.Error msg -> Printf.eprintf "%s%!" msg
    | Parser.Error -> Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start buf)

let _ = main ()

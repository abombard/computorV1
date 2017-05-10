let string_of_expr e = match e with
    | (0., _) -> ""
    | _ as e -> Eq.Expr.to_string e

let rec string_of_eq eq =
    let rec aux eq s = match eq with
        | [] -> ""
        | e :: [] -> s ^ string_of_expr e ^ " = 0"
        | e :: tail -> let se = string_of_expr e in
            if String.length se > 0
            then aux tail (s ^ se ^ " + ")
            else aux tail s
    in (aux eq "")

let solve2 eq = match List.rev eq with
    | (a, _) :: (b, _) :: (c, _) :: [] -> begin
        let delta = b *. b -. 4. *. a *. c in
        match delta with
        | delta when delta < 0. -> Printf.printf
            "Delta is strictly negative.\nTwo solutions complexes:\nx1=(-%.2g - i*sqrt(%.2g)) / %.2g\nx2=(-%.2g + i*sqrt(%.2g)) / %.2g\n"
             b (-.delta) (2. *. a) b (-.delta) (2. *. a)
        | 0.                    -> Printf.printf "Delta is Nul.\nOne solution: x=%.2g\n" (-.b /. (2. *. a))
        | delta                 -> begin
            let x1 = (-.b -. sqrt delta) /. (2. *. a) in
            let x2 = (-.b +. sqrt delta) /. (2. *. a) in
            Printf.printf "Delta is strictly positive.\nTwo solutions: x1=%.2g x2=%.2g\n" x1 x2
        end
    end
    | _ -> invalid_arg ""

let solve1 eq = match List.rev eq with
    | (a, _) :: (b, _) :: [] -> begin
        Printf.printf "The solution is x=%.2g\n" (-.b /. a)
    end
    | _ -> invalid_arg ""

let find_degree eq =
    let rec aux eq d = match eq with
        | [] -> d
        | (coef, degree) :: tail -> aux tail (if coef <> 0. && degree > d then degree else d)
    in aux eq 0

let main () =
    if Array.length Sys.argv = 2 then
        let buf = Lexing.from_string Sys.argv.(1) in
        try
            let eq = (Parser.main Lexer.token buf) in
            (* display eq; *)
            let eq = Eq.reduce eq in
            Printf.printf "Reduced form: %s\n" (string_of_eq eq);
            let degree = find_degree eq in
            Printf.printf "polynomial degree: %d\n" degree;
            match degree with
                | 0 -> Printf.printf "All real numbers are solution of the equation\n"
                | 1 -> solve1 eq
                | 2 -> solve2 eq
                | _ -> Printf.printf "Polynomial degree not handled\n"
        with
        | Lexer.Error msg -> Printf.eprintf "%s%!" msg
        | Parser.Error -> Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start buf)

let _ = main ()

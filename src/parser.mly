%token <string * string>VAR
%token <string>COEF
%token PLUS MINUS EQUAL
%token TIMES
%token EOF

%start <Eq.Expr.t list> main

%%

main:
| e = eq EOF { e }

eq:
| lhs = handside EQUAL rhs = handside {
    List.concat (lhs :: List.map (fun e -> Eq.Expr.revSign e) rhs :: [] )
}

handside:
| o = op { o :: [] }
| o = op m = handside { o :: m }

op:
| e = expr { e }
| PLUS e2 = expr { e2 }
| MINUS e2 = expr { Eq.Expr.revSign e2 }

expr:
| t = token { Eq.Expr.of_token t }
| t1 = token TIMES t2 = token { Eq.Expr.of_token2 t1 t2 }

token:
| coef = COEF {
    let c = float_of_string coef in
    Eq.Token.Coef (Eq.Sign.of_float c, abs_float c)
}
| var = VAR {
    let sign, degree = var in
    let i = int_of_string degree in
    Eq.Token.Var (Eq.Sign.of_string sign, i)
}

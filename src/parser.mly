%token <string * string>VAR
%token <string>COEF
%token PLUS MINUS EQUAL
%token TIMES
%token EOF

%left EQUAL
%left PLUS MINUS

%start <Eq.t list> main

%%

main:
| e = eq EOF { e }

eq:
| lhs = handside EQUAL rhs = handside {
    List.concat (lhs :: List.map (fun e -> Eq.revSign e) rhs :: [] )
}

handside:
| e1 = expr PLUS e2 = expr { e1 :: e2 :: [] }
| e1 = expr MINUS e2 = expr { e1 :: Eq.revSign e2 :: [] }

expr:
| t1 = token TIMES t2 = token { Eq.expr_of_coef_and_var t1 t2 }
| t = token { Eq.expr_of_coef_or_var t }

token:
| coef = COEF { Eq.coef coef }
| var = VAR { Eq.var var }


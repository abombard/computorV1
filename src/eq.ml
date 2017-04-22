module Sign =
    struct
        type t = Plus | Minus

        let of_float f =
            if f < 0. then Minus
            else Plus

        let to_float t = match t with
            | Plus -> 1.
            | Minus -> -.1.

        let of_string s = match s with
            | "" | "+" -> Plus
            | "-"      -> Minus
            | _ -> invalid_arg s

        let to_string t = match t with
            | Plus -> ""
            | Minus -> "-"

        let rev t = match t with
            | Plus -> Minus
            | Minus -> Plus

        let add t1 t2 = match t1, t2 with
            | Minus, Minus -> Plus
            | Minus, Plus  -> Minus
            | Plus, Minus  -> Minus
            | Plus, Plus   -> Plus

        let solve l =
            let rec aux l sign = match l with
                | [] -> sign
                | s :: tail -> aux tail (add sign s)
            in aux l Plus
    end

type t = Coef of Sign.t * float
       | Var of Sign.t * int
       | Expr of Sign.t * float * int
       | Equal

let rec to_string t = match t with
    | Coef ( sign, f )             -> string_of_float (Sign.to_float sign *. f)
    | Var ( sign, degree )         -> Sign.to_string sign ^ "X" ^ (if degree <> 1 then "^" ^ string_of_int degree else "")
    | Expr ( sign, coef, degree )  -> to_string (Coef (sign, coef)) ^ " * " ^ to_string (Var (Sign.Plus, degree ))
    | Equal                        -> "="

let coef s = let f = float_of_string s in Coef (Sign.of_float f, abs_float f)
let var (sign, degree) = Var ( Sign.of_string sign, int_of_string degree )

let expr_of_coef_and_var t1 t2 = match t1, t2 with
    | Coef (s1, coef), Var (s2, degree)   -> Expr (Sign.add s1 s2, coef, degree)
    | Var (s2, degree), Coef (s1, coef) -> Expr (Sign.add s1 s2, coef, degree)
    | _ -> invalid_arg (to_string t1 ^ " && " ^ to_string t2)

let expr_of_coef_or_var t = match t with
    | Coef (sign, f)     -> Expr ( sign, f, 0 )
    | Var (sign, degree) -> Expr ( sign, 1., degree )
    | _ as e             -> e

let revSign t = match t with
    | Coef (s, f) -> Coef (Sign.rev s, f)
    | Var (s, d) -> Var (Sign.rev s, d)
    | Expr (s, f, d) -> Expr (Sign.rev s, f, d)
    | _ as e -> e

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

module Token =
    struct
        type t = Coef of Sign.t * float
               | Var of Sign.t * int
    end

module Expr =
    struct
        type t = Sign.t * float * int

        let to_string t = match t with
            | sign, coef, degree -> Sign.to_string sign ^ string_of_float coef ^ if degree = 0 then "" else " * X" ^ if degree = 1 then "" else "^" ^ string_of_int degree

        let of_token t = match t with
            | Token.Coef (sign, f)     -> ( sign, f, 0 )
            | Token.Var (sign, degree) -> ( sign, 1., degree )

        let of_token2 t1 t2 =  match t1, t2 with
            | Token.Coef (s1, coef), Token.Var (s2, degree) -> (Sign.add s1 s2, coef, degree)
            | Token.Var (s2, degree), Token.Coef (s1, coef) -> (Sign.add s1 s2, coef, degree)
            | _ -> invalid_arg "Expr.of_tokens"

        let revSign t = match t with
            | (s, f, d) -> (Sign.rev s, f, d)
    end

type t = Expr.t list

let reduce t =
    let aux i = List.fold_left
        (fun acc e -> match acc, e with
            | (s1, c1, d1), (s2, c2, d2) when d1 = d2 -> (Sign.add s1 s2, c1 +. c2, d1)
            | _ -> acc
        ) (Sign.Plus, 0., i) t
    in
    aux 2 :: aux 1 :: aux 0 :: []


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
        type t = Coef of float
               | Var of Sign.t * int
    end

module Expr =
    struct
        type t = float * int

        let to_string (coef, degree) =
            let coef_to_string coef = match coef with
                | 1.  when degree <> 0 -> ""
                | -1. when degree <> 0 -> "-"
                | coef when coef = float_of_int (int_of_float coef) -> string_of_int (int_of_float coef)
                | _    -> string_of_float coef
            in
            let degree_to_string degree = match degree with
                | 0 -> ""
                | 1 -> "X"
                | _ -> "X^" ^ string_of_int degree
            in
            let link =
                if coef <> 1. && coef <> -1. && degree <> 0
                then " * "
                else ""
            in
            coef_to_string coef ^ link ^ degree_to_string degree

        let of_token t = match t with
            | Token.Coef f             -> (f                 , 0     )
            | Token.Var (sign, degree) -> (Sign.to_float sign, degree)

        let of_token2 t1 t2 = match t1, t2 with
            | Token.Coef coef, Token.Var (sign, degree)
            | Token.Var (sign, degree), Token.Coef coef -> (Sign.to_float sign *. coef, degree)
            | _ -> invalid_arg "Expr.of_tokens"

        let revSign (coef, degree) = (-.coef, degree)
    end

type t = Expr.t list

let reduce t =
    let collect i = List.fold_left
        (fun acc e -> match acc, e with
            | (c1, d1), (c2, d2) when d1 = d2 -> (c1 +. c2, d1)
            | _ -> acc
        ) (0., i) t
    in
    let degree_max l =
        List.fold_left (fun acc (coef, degree) -> if degree > acc then degree else acc) 0 t
    in
    let aux l =
        let rec new_list n nl =
            if n < 0 then nl
            else new_list (n-1) (collect n :: nl)
       in new_list (degree_max l) []
    in aux t

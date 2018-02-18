module Data

type value = 
    String of string 
  | Bool of bool
  | Int of int

type var = string

type expr = 
  | And of expr * expr
  | Or of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Concat of expr * expr
  | Const of value
  | Var of var

let boolv = function
  | Bool b -> b
  | Int i -> i <> 0
  | String str -> str <> ""
   
let rec stringv = function
  | String s -> s
  | Int i -> string i
  | Bool b -> string b

let boolf opr x y = (opr <| boolv x <| boolv y) |> Bool

let rec eval t = function
  | And (e, f) -> boolf (&&) <| eval t e <| eval t f
  | Or (e, f)  -> boolf (||) <| eval t e <| eval t f
  | Lt (e, f)  -> 
      match eval t e, eval t f with
      | String s1, String s2 -> s1 < s2 |> Bool
      | Int i1, Int i2 -> i1 < i2 |> Bool
      | x, y -> D.error 0037 "Cannot compare values '%A' and '%A'" x y 
  | Gt (e, f) -> eval t <| Lt (f, e)
  | Eq (e, f) -> eval t e = eval t f |> Bool
  | Neq (e, f) -> eval t e <> eval t f |> Bool
  | Concat (e, f) -> (stringv (eval t e)) + (stringv (eval t f)) |> String
  | Const x -> x
  | Var id -> 
      match Map.tryFind id t with
      | None -> D.error 0019 "Unbound variable '%s'" id
      | Some x -> x

let rec subst t = function 
  | And (e, f) -> And (subst t e, subst t f)
  | Or  (e, f) -> Or  (subst t e, subst t f)
  | Lt  (e, f) -> Lt  (subst t e, subst t f)
  | Gt  (e, f) -> Gt  (subst t e, subst t f)
  | Eq  (e, f) -> Eq  (subst t e, subst t f)
  | Neq (e, f) -> Neq  (subst t e, subst t f)
  | Concat (e, f) -> Concat (subst t e, subst t f)
  | Const b as e -> e
  | Var id -> 
      match Map.tryFind id t with 
      | Some x -> Const x
      | None -> Var id

let ppval = stringv

let rec ppexpr = function 
  | And (e, f) -> "& " + ppexpr e + " " + ppexpr f
  | Or (e, f)  -> "| " + ppexpr e + " " + ppexpr f
  | Lt (e, f)  -> "< " + ppexpr e + " " + ppexpr f
  | Gt (e, f)  -> "> " + ppexpr e + " " + ppexpr f
  | Eq (e, f)  -> "= " + ppexpr e + " " + ppexpr f
  | Neq (e, f) -> "<> " + ppexpr e + " " + ppexpr f
  | Concat (e, f) -> "++ " + ppexpr e + " " + ppexpr f
  | Const x -> ppval x
  | Var id -> id

let ppI = ppval


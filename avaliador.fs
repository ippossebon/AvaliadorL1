module avaliador
(* Isadora Possebon - 00228551 Victória Portella - 00225886 *)
(*
Gramática:
        e ∈ Terms
        e ::= n
        | b
        | e1 op e2
        | if e1 then e2 else e3
        | x
        | e1 e2
        | fn x:T ⇒ e
        | let x:T = e1 in e2
        | let rec f:T1 → T2 = (fn y:T1 ⇒ e1) in e2
        | nil
        | e1 :: e2
        | isempty e
        | hd e
        | tl e
        | raise
        | try e1 with e2

T ::= X | int | bool | T1 → T2
        onde
        n ∈ conjunto de numerais inteiros
        b ∈ {true, false}
        x ∈ Ident
        op ∈ {+, −, ∗, div, <, ≤, ==, ! =, ≥, >}
        X ∈ VarType
*)

type Type =
    Boolean
    | Int
    | List
    | Func

exception NoRuleAppliesException

type Operator =
      Sum
      | Diff
      | Mult
      | Div
      | Equal
      | NotEqual
      | Less
      | Greater
      | LessOrEqual
      | GreaterOrEqual

type Expression =
      Num of int                                                    (* Num refere-se a linguagem aqui implementada, int a F# *)
    | Bool of bool                                                  (* Bool refere-se a linguagem aqui implementada, bool a F# *)
    | Var of string (* Identificador *)
    | BinOp of Expression * Operator * Expression                   (* 2 + 3 -- nao tenho certeza. pode ser op * exp * exp*)
    | If of Expression * Expression * Expression                    (* if e1 then e2 else e3 *)
    | Applic of Expression * Expression                             (* Aplicação: eval e1 *)
    | Function of string * Type * Expression                        (* (fn string : T -> x + 1) e1  >>> Confirmar *)
    | Let of Expression * Expression * Expression                   (* let e1 = 5 *)
    | LetRec of string * Type * Type * Expression * Expression
    | Nil                                                           (* Lista vazia *)
    | Cons of Expression * Expression                               (* cons(e1, e2) Construção de listas *)
    | IsEmpty of Expression
    | Hd of Expression                                              (* Primeiro elemento da lista: hd(1 2 4) -> 1 *)
    | Tl of Expression                                              (* Lista - primeiro elemento tl(1 2 4) - > (2 4)*)
    | Raise
    | TryWith of Expression * Expression

let rec isValue (e:Expression) : bool =   (* bool de F# *) (* isValue *)
    match e with
    Bool _ -> true (* Valor Bool *)
    | Num _ -> true (* Valor numérico *)
    | Function(_) -> true (* fn x:T ⇒ e *)
    | Nil -> true (* a confirmar *)
    (*| Cons(e1, e2) -> Cons(, isValue e2) *)(* isValue e1 e isValue e2*)
    | e -> false (* Não está pronto *)


(* Excecao a ser ativada quando o termo for uma forma normal.
    Isso significa que:
    - term pode ser um VALOR, ou
    - term pode ser um ERRO de execucao *)

(* Substitui ocorrencias de var em body por value. {value/var} body *)
let rec replace (body:Expression) (var:Expression) (value:Expression) : Expression =
    match body with
    If(e1, e2, e3) -> If(replace e1 var value, replace e2 var value, replace e3 var value)
    | BinOp(e1, operator, e2) -> BinOp(replace e1 var value, operator, replace e2 var value)
    | Applic(e1, e2) -> Applic(replace e1 var value, replace e2 var value)
    | Let(id, e1, e2) when (id = var) -> Let(id, replace e1 var value, e2)
    | Let(id, e1, e2) -> Let(id, replace e1 var value, replace e2 var value)
    | LetRec(name, T1, T2, func, e) -> LetRec(name, T1, T2, func, (replace e var value))
    | _ -> if body = var then value else body (* Let x: T => x  *)


let rec replaceInLetRec (body:Expression) (var:string) (t1:Type) (t2:Type) (func:Expression) : Expression =
    match body with
    Applic(e1, e2) when (e1 = (Var var)) -> LetRec(var, t1, t2, func, Applic(Var var, e2))
    | If(e1, e2, e3) -> If((replaceInLetRec e1 var t1 t2 func), (replaceInLetRec e2 var t1 t2 func), (replaceInLetRec e3 var t1 t2 func))
    | BinOp(e1, op, e2) -> BinOp((replaceInLetRec e1 var t1 t2 func), op, (replaceInLetRec e2 var t1 t2 func))
    | Applic(e1, e2) -> Applic((replaceInLetRec e1 var t1 t2 func), (replaceInLetRec e2 var t1 t2 func))
    | Function(v, tp, f) -> Function(v, tp, (replaceInLetRec f var t1 t2 func))
    | _ -> body


(* Small Step *)
let rec step (e:Expression) : Expression =
  match e with
        (* Caso IF(t1, t2, t3)*)
        If(Bool true, e2, e3) -> e2 (* IF TRUE *)
        | If(Bool false, e2, e3) -> e3 (* IF FALSE *)
        | If(e1, e2, e3) -> let e1' = step e1 in If(e1', e2, e3)

        | BinOp(Num n1, Operator, Num n2) ->
            (match Operator with
                Sum -> Num(n1+n2)
                | Diff -> Num(n1-n2)
                | Mult -> Num(n1*n2)
                | Div -> Num(n1/n2)
                | Equal -> Bool(n1 = n2)
                | NotEqual -> Bool(n1 <> n2)
                | Less -> Bool(n1 < n2)
                | LessOrEqual -> Bool(n1 <= n2)
                | Greater -> Bool(n1 > n2)
                | GreaterOrEqual -> Bool(n1 >= n2))
        | BinOp (Num e1, Operator, e2) -> let e2' = step e2 in (BinOp(Num e1, Operator, e2'))
        | BinOp (e1, Operator, e2) -> let e1' = step e1 in (BinOp(e1', Operator, e2))

        | Applic(Function(identifier, tp, expression), value) -> replace expression (Var identifier) (value)
        | Applic(v, e2) when (isValue v) && not(isValue e2) -> let e2' = step e2 in Applic(v, e2')
        | Applic(e1, e2) when not(isValue e1) -> let e1' = step e1 in Applic(e1', e2)

        (* let x = 2 in escopo --> Ainda nao funciona *)
        | Let(Var identifier, Num n, e2) -> replace e2 (Var identifier) (Num n)
        | Let(Var identifier, Bool e1, e2) -> replace e2 (Var identifier) (Bool e1)
        | Let(Var identifier, e1, e2) -> let e1' = step e1 in Let(Var identifier, e1', e2)

        | LetRec(nm, t1, t2, f, e) -> replaceInLetRec(replace e (Var nm) (f)) (nm) (t1) (t2) (f)

        | _ -> raise NoRuleAppliesException (* termos prontos (incluindo Nil) retornam NoRuleAppliesException *)

let rec eval e =
    try let e' = step e
        in eval e'
    with NoRuleAppliesException -> e


(* Testes BinOp *)
(*let verdade = eval(If(Bool true, Num 2, Num 3))
let falso = eval(If(Bool true, Num 2, Num 3))
let doisMaisCinco = eval(BinOp(Num 2, Sum, Num 5))
let cincoMenosQuatro = eval(BinOp(Num 5, Diff, Num 4))
let doisVezesTres = eval(BinOp(Num 2, Mult, Num 3))
let seisDivDois = eval(BinOp(Num 6, Div, Num 2))*)
(*let x = eval(Applic(Function("x", Int, BinOp(Var "x", Sum, BinOp(Var "x", Mult, Num 2))), Num 6)) *)(*x + 2x para x=6*)
(*
let funcaoDobro = eval(Applic(Function("x", Int, BinOp(Var "x", Mult, Num 2)), Num 30))
let funcaoMax = If(BinOp(Var "in1", Greater, Var "in2"), Var "in1", Var "in2")
let variavel = Let(Var "x", 5, ...)

let testeLet = eval(Let(Var "oito", Num 8, Function("y", Int, BinOp(Var "y", Mult, Num 2))));;
let testeLerRec = eval(LetRec("foo", Int, Int, Function("x", Int, (If ((BinOp(Var "x", GreaterOrEqual, Num 5)),(Var "x"),(Applic(Var "foo", BinOp(Var "x", Sum, Num 1)))))), Applic(Var "foo", Num 3)));;

*)

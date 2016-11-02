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
    | Var of string
    | BinOp of Expression * Operator * Expression                   (* 2 + 3 -- nao tenho certeza. pode ser op * exp * exp*)
    | If of Expression * Expression * Expression                    (* if e1 then e2 else e3 *)
    | Applic of Expression * Expression                             (* Aplicação: eval e1 *)
    | Function of string * Type * Expression                        (* (fn string : T -> x + 1) e1  >>> Confirmar *)
    | Let of Expression * Expression * Expression                   (* let e1 = 5 *)
    | LetRec of string * Type * Type * Expression * Expression


let rec isReady (e:Expression) : bool =   (* bool de F# *)
    match e with
    Bool true -> true (* Valor Bool *)
    | Bool false -> true (* Valor Bool *)
    | Num e -> true (* Valor numérico *)
    (*Func e -> ...*) (* fn é um termo pronto *)
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




(* Small Step *)
let rec step (e:Expression) : Expression =
  match e with
        (* Caso IF(t1, t2, t3)*)
        If(Bool true, e2, e3) -> e2 (* IF TRUE *)
        | If(Bool false, e2, e3) -> e3 (* IF FALSE *)
        | If(e1, e2, e3) -> let e1' = step e1 in If(e1', e2, e3)

        (* Caso BINARY OPERATOR (BinOp)*)
        (* nv op nv -> nv
            e1 op e2 -> e1' op e2
            nv op e2 -> nv op e2'

            *)
        (* e1 e e2 estão prontos. *)
        | BinOp (Num e1, Sum, Num e2) -> Num(e1+e2) (* Num(e1 + e2)*)
        (* e2 não está pronto e precisa ser avaliado. *)
        | BinOp (Num e1, Sum, e2) -> let e2' = step e2 in (BinOp(Num e1, Sum, e2'))
        (* e1 não está pronto e precisa ser avaliado. *)
        | BinOp (e1, Sum, e2) -> let e1' = step e1 in (BinOp(e1', Sum, e2))


        | BinOp (Num e1, Diff, Num e2) -> Num(e1-e2)
        | BinOp (Num e1, Diff, e2) -> let e2' = step e2 in (BinOp(Num e1, Diff, e2'))
        | BinOp (e1, Diff, e2) -> let e1' = step e1 in (BinOp(e1', Diff, e2))

        | BinOp (Num e1, Mult, Num e2) -> Num(e1*e2)
        | BinOp (Num e1, Mult, e2) -> let e2' = step e2 in (BinOp(Num e1, Mult, e2'))
        | BinOp (e1, Mult, e2) -> let e1' = step e1 in (BinOp(e1', Mult, e2))

        | BinOp (Num e1, Div, Num e2) -> Num(e1/e2) (* Devemos tratar se e2 = 0 *)
        | BinOp (Num e1, Div, e2) -> let e2' = step e2 in (BinOp(Num e1, Div, e2'))
        | BinOp (e1, Div, e2) -> let e1' = step e1 in (BinOp(e1', Div, e2))

        | BinOp (Num e1, Equal, Num e2) -> Bool(e1=e2)
        | BinOp (Num e1, Equal, e2) -> let e2' = step e2 in (BinOp(Num e1, Equal, e2'))
        | BinOp (e1, Equal, e2) -> let e1' = step e1 in (BinOp(e1', Equal, e2))

        | BinOp (Num e1, NotEqual, Num e2) -> Bool(e1<>e2)
        | BinOp (Num e1, NotEqual, e2) -> let e2' = step e2 in (BinOp(Num e1, NotEqual, e2'))
        | BinOp (e1, NotEqual, e2) -> let e1' = step e1 in (BinOp(e1', NotEqual, e2))

        | BinOp (Num e1, Less, Num e2) -> Bool(e1<e2)
        | BinOp (Num e1, Less, e2) -> let e2' = step e2 in (BinOp(Num e1, Less, e2'))
        | BinOp (e1, Less, e2) -> let e1' = step e1 in (BinOp(e1', Less, e2))

        | BinOp (Num e1, LessOrEqual, Num e2) -> Bool(e1<=e2)
        | BinOp (Num e1, LessOrEqual, e2) -> let e2' = step e2 in (BinOp(Num e1, LessOrEqual, e2'))
        | BinOp (e1, LessOrEqual, e2) -> let e1' = step e1 in (BinOp(e1', LessOrEqual, e2))

        | BinOp (Num e1, Greater, Num e2) -> Bool(e1>e2)
        | BinOp (Num e1, Greater, e2) -> let e2' = step e2 in (BinOp(Num e1, Greater, e2'))
        | BinOp (e1, Greater, e2) -> let e1' = step e1 in (BinOp(e1', Greater, e2))

        | BinOp (Num e1, GreaterOrEqual, Num e2) -> Bool(e1>=e2)
        | BinOp (Num e1, GreaterOrEqual, e2) -> let e2' = step e2 in (BinOp(Num e1, GreaterOrEqual, e2'))
        | BinOp (e1, GreaterOrEqual, e2) -> let e1' = step e1 in (BinOp(e1', GreaterOrEqual, e2))

        | Applic(e1, e2) when not(isReady e1) -> let e1' = step e1 in Applic(e1', e2)
        | Applic(v, e2) when (isReady v) && not(isReady e2) -> let e2' = step e2 in Applic(v, e2')
        | Applic(Function(identifier, tp, expression), value) -> replace expression (Var identifier) (value)

        | _ -> raise NoRuleAppliesException

let rec eval e =
    printfn "estou no eval"
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

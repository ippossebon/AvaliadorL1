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

exception NoRuleApplies

type identifier = string    (* Tipo string de F# *)

type operator =
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

type expression =
      Num of int                (* Num refere-se a linguagem aqui implementada, int a F# *)
    | Bool of bool              (* Bool refere-se a linguagem aqui implementada, bool a F# *)
    | Var of identifier         (* identifier : string *)
    | BinOp of expression * operator * expression (* 2 + 3 -- nao tenho certeza. pode ser op * exp * exp*)
    | If of expression * expression * expression (* if e1 then e2 else e3 *)
    | Applic of expression * expression (* Aplicação: eval e1 *)
    | Function of identifier * Type * expression (* (fn identifier : T -> x + 1) e1  >>> Confirmar *)
    | Let of expression * expression * expression (* let e1 = 5 *)
    | LetRec of identifier * Type * Type * expression * expression(* letrec identifier:Type1 → Type2 = (fn y:Type1 ⇒ e1) in e2*)

(* Função auxiliar que verifica se um dado termo está pronto.
O termo está pronto quando o termo é um valor. *)
let rec is_ready (e:expression) : bool = (* bool de F# *)
    match e with
    | Bool true -> true (* Valor Bool *)
    | Bool false -> true (* Valor Bool *)
    | Num e -> true (* Valor numérico *)
    (*Func e -> ...*) (* fn é um termo pronto *)
    | e -> false (* Não está pronto *)


(* Excecao a ser ativada quando o termo for uma forma normal.
    Isso significa que:
    - term pode ser um VALOR, ou
    - term pode ser um ERRO de execucao *)

(* Funcao auxiliar: determinar se um termo eh um VALOR NUMERICO*)
//let rec is_numerical_value(e: expression) : bool =
//    match e with
//        Num e -> true;
//        | _ -> false


(* Small Step *)
let rec step (e:expression) : expression =
    match e with
        (* Caso IF(t1, t2, t3)*)
        If(Bool true, e2, e3) -> e2 (* IF TRUE *)
        | If(Bool false, e2, e3) -> e3 (* IF FALSE *)
        | If(e1, e2, e3) -> let e1' = step e1 in If(e1', e2, e3)

        (* Caso BINARY OPERATOR (BinOp)*)
        (* nv op nv -> nv
            e1 op e2 -> e1' op e2
            nv op e2 -> nv op e2'
            VICK: binOp sempre recebe expression, não tem como passar valor. Parece que não
            mexemos com valor no step.
            *)
            (* e1 e e2 estão prontos. *)
        | BinOp (Num e1, Sum, Num e2) -> Num(e1+e2) (* Num(e1 + e2)*)
        (* Antes estava:  BinOp (e1, Sum, e2) -> let e1' = step(BinOp(e1', Sum, e2))
            e dando erro, acho que agora está certo ne? -- Acho que sim! *)
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

        //| _ -> raise NoRuleApplies

let rec eval e =
    printfn "estou no eval"
    try let e' = step e
        in eval e'
    with NoRuleApplies -> e


(* Testes BinOp *)
let doisMaisCinco = BinOp(Num 2, Sum, Num 5);;
let cincoMenosQuatro = BinOp(Num 5, Diff, Num 4);;
let doisVezesTres = BinOp(Num 2, Mult, Num 3);;
let seisDivDois = BinOp(Num 6, Div, Num 2);;

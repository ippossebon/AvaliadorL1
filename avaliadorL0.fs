(* Isadora Possebon - 00228551
    Victória Portella - 00225886 *)

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
      Num of int                (* int F# *)
    | Bool of bool              (* bool F# *)
    | Var of identifier         (* identifier : string *)
    | BinOp of expression * operator * expression (* 2 + 3 -- nao tenho certeza. pode ser op * exp * exp*)
    | If of expression * expression * expression (* if e1 then e2 else e3 *)
    | Applic of expression * expression (* eval e1*)
    | Function of identifier * Type * expression (* (fn identifier : T -> x + 1) e1  >>> Confirmar *)
    | Let of expression * expression * expression (* let e1 = 5 *)
    | LetRec of identifier * Type * Type * expression * expression(* letrec identifier:Type1 → Type2 = (fn y:Type1 ⇒ e1) in e2*)


let rec is_ready (e:expression) : bool = (* bool de F# *)
    match e with
    | Bool true -> true
    | Bool false -> true
    | Num e -> true
    (*Func e -> ...*)
    | e -> false



step -> is_value || step(e)

(* Excecao a ser ativada quando o termo for uma forma normal.
    Isso significa que:
    - term pode ser um VALOR, ou
    - term pode ser um ERRO de execucao *)

(* Funcao auxiliar: determinar se um termo eh um VALOR NUMERICO*)
let rec is_numerical_value(e: expression) : bool =
    match e with
        Num(a) -> true;
        | Bool

(* Funcao STEP: -> avaliacao em um passo
                  VICK: retorna uma expressão: 1) parece que tem q ter esse retorno
                                        2) não tem como retornar um valor???!!!! ou não se retorna valores
                                        3) um valor também é uma expressão? *)
let rec step (e:expression) : expression =
    match e with (* e = 2 + 3*)
        (* Caso VALOR *)
        //  Num(e) -> VNum(e) (* Num(2) -> VNum(2)*)
        //| Bool(e) -> VBool(e) (* Bool(true) -> VBool(true) *)

        (* Caso IF(t1, t2, t3)*)
        | If(Bool true, e2, e3) -> e2 (* IF TRUE *)
        | If(Bool false, e2, e3) -> e3 (* IF FALSE *)
        | If(e1, e2, e3) -> let e1' = step e1 in If(e1', e2, e3)

        (* Caso BINARY OPERATOR*)
        (* nv op nv -> nv
            e1 op e2 -> e1' op e2
            nv op e2 -> nv op e2'
            VICK: binOp sempre recebe expression, não tem como passar valor. Parece que não
            mexemos com valor no step.
            *)
        | BinOp (Num e1, Sum, Num e2) -> Num(e1+e2) (* Num(e1 + e2)*)
        (* Antes estava:  BinOp (e1, Sum, e2) -> let e1' = step(BinOp(e1', Sum, e2))
            e dando erro, acho que agora está certo ne?*)
        | BinOp (e1, Sum, e2) -> let e1' = step e1 in (BinOp(e1', Sum, e2))
        | BinOp (Num e1, Sum, e2) -> let e2' = step e2 in (BinOp(Num e1, Sum, e2'))


        | _ -> raise NoRuleApplies

(* let rec eval t =
    printfn "estou no eval"
    try let t' = step t
        in eval t'
    with NoRuleApplies -> t *)

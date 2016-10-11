type term =
    TTrue
    | TFalse
    | TIf of term * term * term
    | TZero 
    | TSucc of term
    | TPred of term
    | TIsZero of term

(* Excecao a ser ativada quando o termo for uma forma normal.
    Isso significa que:
    - term pode ser um VALOR, ou
    - term pode ser um ERRO de execucao *)
exception NoRuloApplies

(* Funcao auxiliar: determinar se um termo eh um VALOR NUMERICO*)
let rec is_numerical_value t = match t with
      TZero -> true
    | TSucc(t1) -> is_numerical_value t1
    | _ -> false

(* Funcao STEP: -> avaliacao em um passo *)
let rec step t = match t with
    (* Caso IF(t1, t2, t3)*)
      TIf(TTrue, t2, t3) -> t2 (* IF TRUE *)
    | TIf(TFalse, t2, t3) -> t3 (* IF FALSE *)
    | TIf(t1, t2, t3) -> let t1' = step t1 in TIf(t1' , t2, t3)

    | TSucc(t1) -> let t1' = step t1 in TSucc(t1')

    | TPred(TZero) -> TZero
    | TPred(TSucc (nv1)) when (is_numerical_value nv1) -> nv1
    | TPred(t1) -> let t1' = step t1 in TPred(t1')

    | TIsZero(TZero) -> TTrue
    | TIsZero(TSucc (nv1)) when (is_numerical_value nv1) -> TFalse
    | TIsZero(t1) -> let t1' = step t1 in TIsZero(t1')
    | _ -> raise NoRuloApplies


(* Implementacao de EVAL *)
let rec eval t =
    printfn "estou no eval"
    try let t' = step t
        in eval t'
    with NoRuleApplies -> t


[<EntryPoint>]
    let main argv =
        let t1 = TIsZero(TZero)
        let t2 = TZero
        let t3 = TSucc(TZero)
        let t4 = TIsZero(TSucc(TZero))
        let t5 = TIsZero(TFalse)

        let resp = eval t4




        0

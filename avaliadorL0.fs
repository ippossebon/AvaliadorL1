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
    | TIf(t1, t2, t3) -> let t1' in TIf(t1', t2, t3)

    | TSucc(t1) -> let t1' = step t1 in TSucc(t1')

    | TPred(TZero) -> TZero
    | TPred(Tsucc(nv1)) when (is_numerical_value nv1) -> nv1
    | TPred(nv1) -> let t1' = step t1 in TPred(t1')

    | TIsZero(TZero) -> TTrue
    | TIsZero(TSucc(nv1)) when (is_numerical_value nv1) -> TFalse
    | TIsZero(t1) -> let t1' = step t1 in TIsZero(t1')

    | _ -> raise NoRuloApplies


(* Implementacao de EVAL *)
let rec eval t =
    try let t' = step t
        in eval t'
    with NoRuleApplies -> t

(*Tipos de L1*)
type tipo = 
  | TyInt                   (* int *)      (*Tipo numero inteiro*)
  | TyBool                  (* bool *)     (*Tipo booleano*)
  | TyFn    of tipo * tipo  (* T1-->T2 *)  (*Tipo funcao*)
  | TyPar   of tipo * tipo  (* T1*T2 *)    (*Tipo par*)
  | TyList  of tipo         (* T list *)   (*Tipo Lista*)
  | TyMaybe of tipo         (* maybe T *)  (*Tipo maybe*)

(*Tipos de operacoes binarias*)
type bop = 
  | Soma                (* + *)
  | Subtracao           (* - *)
  | Multiplicacao       (* * *)
  | Divisao             (* / *)
  | Igual               (* == *)
  | MaiorQue            (* > *)
  | MenorQue            (* < *)
  | MaiorOuIgual        (* >= *)
  | MenorOuIgual        (* <= *)
  | And                 (* && *)
  | Or                  (* || *)

(*Gramatica de L1*)
type expr =
  | ExNum           of int                           (*Numero inteiro*)
  | ExVar           of string                        (*Variavel*)
  | ExTrue                                           (*Booleano true*)
  | ExFalse                                          (*Booleano false*)
  | ExIf            of expr * expr * expr            (*If then else*)
  | ExFn            of string * tipo * expr          (*Funcao anonima*)
  | ExApp           of expr * expr                   (*Aplicacao*)
  | ExLet           of string * tipo * expr * expr   (*Funcao declarada*)
  | ExLetRec        of string * tipo * expr * expr   (*Funcao declarada recursiva*)
  | ExBinop         of bop * expr * expr             (*Operacao binaria*)
  | ExPar           of expr * expr                   (*Par*)
  | ExFst           of expr                          (*Primeiro elemento de um par*)
  | ExSnd           of expr                          (*Segundo elemento de um par*)
  | ExNil           of tipo                          (*Lista vazia*)
  | ExList          of expr * expr                   (*Lista nao vazia*)
  | ExHead          of expr                          (*Retorna o primeiro elemento de uma lista*)
  | ExTail          of expr                          (*Retorna uma lista sem o primeiro elemento dela*)
  | ExMatchList     of expr * expr * expr            (*Retorna expressoes diferentes dependendo se a lista for vazia ou nao*)
  | ExJust          of expr                          (*Expressao Just de tipo maybe alguma coisa*)
  | ExNothing       of tipo                          (*Expressao Nothing de tipo maybe alguma coisa*)
  | ExMatchMaybe    of expr * expr * expr            (*Retorna expressoes diferentes dependendo se a expressao de tipo maybe for just ou nothing*)

(*Declaracao dos ambiente das expressoes*)
type ambiente_tipo = (string * tipo) list

(*Valores*)
type valor =
    VNum               of int                                        (* Numero inteiro *)
  | VTrue                                                            (* Booleano true *)
  | VFalse                                                           (* Booleano false *)
  | VPar               of valor * valor                              (* Par *)
  | VClosure           of string * expr * ambiente_valor             (* Funcao anonima *)
  | VClosureRecursivo  of string * string * expr * ambiente_valor    (* Funcao declarada recursiva *)
  | VNil               of tipo                                       (* Lista vazia *)
  | VList              of valor * valor                              (* Lista nao vazia *)
  | VJust              of valor                                      (* Maybe com valor *)
  | VNothing           of tipo                                       (* Maybe vazio *)
and 
  ambiente_valor = (string * valor) list (*Declaracao dos ambiente de valores das expressoes*)

(*Funcao polimorfica que ajuda a varre um ambiente em busca de uma variável de tipo ou de valor*)
let rec lookup ambiente identificador0 = match ambiente with
  | [] -> None
  | (identificador1,valor_ou_tipo) :: tail -> if (identificador1=identificador0) then Some valor_ou_tipo else lookup tail identificador0

(*Funcao polimorfica que permite atualizar um ambiente com uma variável de tipo ou de valor*)
let rec update tail identificador tipo_ou_valor = (identificador,tipo_ou_valor) :: tail

(*Erro acionado caso haja um problema na inferencia de tipo da expressao*) 
exception Erro_Typeinfer of string

(*Erro acionado caso haja um problema na avaliação da expressao*) 
exception Erro_Eval of string



(**************************************************************************************************************)
(**************************************************Typeinfer***************************************************)
(**************************************************************************************************************)

(*Funcao que faz a inferencia de tipo de uma expressao da linguagem L1*)
let rec typeinfer (gamma: ambiente_tipo) (e:expr) : tipo  = match e with

  (*Numero Inteiro*)
  | ExNum _ -> TyInt

  (*Variavel*) 
  | ExVar(x) -> (match lookup gamma x with
      | Some t -> t
      | None -> raise (Erro_Typeinfer ("Variavel nao declarada" ^ x)))

  (*Booleano True*)
  | ExTrue  -> TyBool

  (*Booleano False*)
  | ExFalse -> TyBool

  (*If Then Else*) 
  | ExIf(e1,e2,e3) -> (match typeinfer gamma e1 with
      | TyBool -> 
          if (typeinfer gamma e2) = (typeinfer gamma e3)
          then typeinfer gamma e2
          else raise (Erro_Typeinfer "Em ExIf(e1,e2,e3), e2 e e3 tem tipos diferentes um do outro")
      | _ -> raise (Erro_Typeinfer "Em ExIf(e1,e2,e3), e1 tem tipo diferente de booleano"))

  (*Funcao Anonima*) 
  | ExFn(x,t,e1) -> TyFn(t, (typeinfer(update gamma x t) e1))
  
  (*Aplicacao*)
  | ExApp(e1,e2) -> (match typeinfer gamma e1 with
      | TyFn(t1, t2) ->  if (typeinfer gamma e2) = t1 then t2 else raise (Erro_Typeinfer "Em ExApp(ExFn(x,t,e1),e2) o tipo declarado t eh diferente do tipo da expressao e2")
      | _ -> raise (Erro_Typeinfer "Em ExApp(e1,e2) a expressao e1 nao eh uma funcao"))

  (*Funcao Declarada*)
  | ExLet(x,t,e1,e2) -> 
      if (typeinfer gamma e1) = t
      then typeinfer (update gamma x t) e2
      else raise (Erro_Typeinfer "Em ExLet(x,t,e1,e2) o tipo da expressao e1 eh diferente do tipo declarado t")

  (*Funcao Declarada Recursiva*)
  | ExLetRec(f, TyFn(t1,t2), ExFn(x,t3,e1), e2) when t1 = t3 -> (*t3 deve ser igual a t1*)
      if (typeinfer (update (update gamma f (TyFn(t1,t2))) x t3) e1) = t2
      then typeinfer (update gamma f (TyFn(t1,t2))) e2
      else raise (Erro_Typeinfer "ExLetRec(f, TyFn(t1,t2), ExFn(x,t3,e1), e2) o tipo declarado t3 eh diferente do tipo declarado t1") 

  (*Operacao Binaria*)
  | ExBinop(op,e1,e2) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      (if (t1 = TyInt && t2 = TyInt) then (match op with
          | Soma | Subtracao | Multiplicacao | Divisao -> TyInt
          | Igual | MenorQue | MaiorQue | MaiorOuIgual | MenorOuIgual -> TyBool
          | _ -> raise (Erro_Typeinfer "Em ExBinop(op,e1,e2) o operador op eh invalido"))
      else if t1 = TyBool && t2 = TyBool then (match op with
          | And | Or -> TyBool
          | _ -> raise (Erro_Typeinfer "Em ExBinop(op,e1,e2) o operador op eh invalido"))
      else raise (Erro_Typeinfer "Em ExBinop(op,e1,e2) os tipos das expressoes e1 e e2 sao diferentes um do outro"))

  (*Par*)
  | ExPar(e1,e2) -> TyPar(typeinfer gamma e1, typeinfer gamma e2) 

  (*Primeiro Elemento de um Par*)
  | ExFst(e1) -> (match typeinfer gamma e1 with
      | TyPar(t1,t2) -> t1
      | _ -> raise (Erro_Typeinfer "Em ExFst(e1) a expressao e1 nao eh do tipo par ordenado"))

  (*Segundo Elemento de um Par*)
  | ExSnd(e1) -> (match typeinfer gamma e1 with
      | TyPar(t1,t2) -> t2
      | _ -> raise (Erro_Typeinfer "Em ExSnd(e1) a expressao e1 nao eh do tipo par ordenado"))

  (*Lista Vazia*)
  | ExNil(t0) -> (match t0 with
      | TyList(t1) -> TyList(t1)
      | _ -> raise (Erro_Typeinfer "Em ExNil(t0) o tipo t0 nao eh do tipo lista"))

  (*Lista com Elementos*)
  | ExList(e1, e2) -> (match typeinfer gamma e2 with
      | TyList(t2) -> if (typeinfer gamma e1) = t2 then TyList(t2) else raise (Erro_Typeinfer "Em ExList(e1, e2) o tipo da expressao e1 eh diferente do tipo da lista da expressao e2")
      | _ -> raise (Erro_Typeinfer "Em ExList(e1, e2) a expressao e2 nao eh do tipo lista"))

  (*Primeiro Elemento de uma Lista*)
  | ExHead(e0) -> (match typeinfer gamma e0 with
      | TyList(t0) -> t0
      | _ -> raise (Erro_Typeinfer "Em ExHead(e0) a expressao e0 nao eh do tipo lista"))

  (*Segundo Elemento de uma Lista*)
  | ExTail(e0) -> (match typeinfer gamma e0 with
      | TyList(t0) -> TyList(t0)
      | _ -> raise (Erro_Typeinfer "Em ExTail(e0) a expressao e0 nao eh do tipo lista"))

  (*Match List*)
  | ExMatchList(e1,e2,e3) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in (match t1 with
        | TyList(t4) -> if t2 = t3 then t2 else raise (Erro_Typeinfer "Em ExMatchList(e1,e2,e3) as expressoes e2 e e3 nao tem o mesmo tipo")
        | _ -> raise (Erro_Typeinfer "Em ExMatchList(e1,e2,e3) a expressao e1 nao eh do tipo lista"))

  (*Just*)
  | ExJust(e0) -> TyMaybe(typeinfer gamma e0)

  (*Nothing*)
  | ExNothing(t) -> TyMaybe(t)

  (*Match Maybe*)
  | ExMatchMaybe(e1,e2,e3) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in (match t1 with
        | TyMaybe(t4) -> if t2 = t3 then t2 else (raise (Erro_Typeinfer "Em ExMatchMaybe(e1,e2,e3) as expressoes e2 e e3 nao tem o mesmo tipo"))
        | _ -> raise (Erro_Typeinfer "Em ExMatchMaybe(e1,e2,e3) a expressao e1 nao eh do tipo maybe"))

  (*Excecoes retornam erros*)
  | _ -> raise (Erro_Typeinfer "Expressao nao eh do tipo esperado")


(**************************************************************************************************************)
(**********************************************Avaliador Big Step**********************************************)
(**************************************************************************************************************)

(*Funcao que realiza as operacoes binarias*)
let faz_operacao (oper: bop) (v1: valor) (v2: valor) = match (oper, v1, v2) with
  | (Soma, VNum(n1), VNum(n2)) -> VNum(n1 + n2)
  | (Subtracao, VNum(n1), VNum(n2)) -> VNum(n1 - n2)
  | (Multiplicacao, VNum(n1), VNum(n2)) -> VNum(n1 * n2)
  | (Divisao, VNum(n1), VNum(n2)) -> if n2 = 0 then raise (Erro_Eval "A divisao por zero eh ilegal") else VNum(n1 / n2)
  | (Igual, VNum(n1), VNum(n2))  -> if (n1 = n2)  then VTrue else VFalse
  | (MaiorQue, VNum(n1), VNum(n2))  -> if (n1 > n2)  then VTrue else VFalse
  | (MenorQue, VNum(n1), VNum(n2))  -> if (n1 < n2)  then VTrue else VFalse
  | (MaiorOuIgual, VNum(n1), VNum(n2)) -> if (n1 >= n2) then VTrue else VFalse
  | (MenorOuIgual, VNum(n1), VNum(n2)) -> if (n1 <= n2) then VTrue else VFalse
  | (And, VTrue, VTrue) -> VTrue
  | (And, VTrue, VFalse) -> VFalse
  | (And, VFalse, VTrue) -> VFalse
  | (And, VFalse, VFalse) -> VFalse
  | (Or, VTrue, VTrue) -> VTrue
  | (Or, VTrue, VFalse) -> VTrue
  | (Or, VFalse, VTrue) -> VTrue
  | (Or, VFalse, VFalse) -> VFalse
  | _ -> raise (Erro_Eval "Operacao invalida")


(*Funcao que faz o passo big step*)
let rec eval (gamma:ambiente_valor) (e:expr) : valor = match e with

  (*Numero Inteiro*)
  | ExNum(n) -> VNum(n)

  (*Variavel*) 
  | ExVar(x) -> (match lookup gamma x with
      | Some v -> v
      | None -> raise (Erro_Eval "Variavel nao declarada"))

  (*Booleano True*)
  | ExTrue -> VTrue

  (*Booleano False*)
  | ExFalse -> VFalse

  (*If Then Else*)
  | ExIf(e1,e2,e3) -> (match eval gamma e1 with
      | VTrue -> eval gamma e2
      | VFalse -> eval gamma e3
      | _ -> raise (Erro_Eval "Em ExIf(e1,e2,e3) a expressao e1 nao avalia para um valor booleano"))

  (*Funcao Anonima*) 
  | ExFn (x,t,e1) ->  VClosure(x,e1,gamma)

  (*Aplicacao*)
  | ExApp(e1,e2) -> (match eval gamma e1 with
        | VClosure(x,e3,novo_gamma) -> eval (update novo_gamma x (eval gamma e2)) e3
        | VClosureRecursivo(f,x,e3,novo_gamma) -> eval (update (update novo_gamma x (eval gamma e2)) f (eval gamma e1)) e3
        | _ -> raise (Erro_Eval "Em ExApp(e1,e2) a expressao e1 nao avalia para um valor closure"))

  (*Funcao Declarada*)
  | ExLet(x,t,e1,e2) -> eval (update gamma x (eval gamma e1)) e2 

  (*Funcao Declarada Recursiva*)
  | ExLetRec(f,TyFn(t1,t2),ExFn(x,t3,e1), e2) when t1 = t3 -> eval (update gamma f (VClosureRecursivo(f,x,e1,gamma))) e2

  (*Operacao Binaria*)
  | ExBinop(op,e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      faz_operacao op v1 v2

  (*Par*)
  | ExPar(e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VPar(v1,v2)

  (*Primeiro Elemento de um Par*)
  | ExFst(e0) -> (match e0 with
      | ExPar(e1,_) -> eval gamma e1
      | _ -> raise (Erro_Eval "Em ExFst(e0) a expressao e0 nao avalia para um valor par ordenado"))

  (*Segundo Elemento de um Par*)
  | ExSnd(e0) -> (match e0 with
      | ExPar(_,e2) -> eval gamma e2
      | _ -> raise (Erro_Eval "Em ExSnd(e0) a expressao e0 nao avalia para um valor par ordenado"))

  (*Lista Vazia*)
  | ExNil(t0) -> VNil(t0)

  (*Lista com Elementos*)
  | ExList(e1, e2) -> 
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VList(v1,v2)

  (*Primeiro Elemento de uma Lista*)
  | ExHead(e0) -> (match e0 with
      | ExList(e1,_) -> eval gamma e1
      | _ -> raise (Erro_Eval "Em ExHead(e0) a expressao e0 nao avalia para um valor lista"))

  (*Segundo Elemento de uma Lista*)
  | ExTail(e0) -> (match e0 with
      | ExList(_,e2) -> eval gamma e2
      | _ -> raise (Erro_Eval "Em ExTail(e0) a expressao e0 nao avalia para um valor lista"))

  (*Verifica se a Lista e Vazia ou Nao*)
  | ExMatchList(e0, e1, e2) -> (match eval gamma e0 with
      | VNil(_) -> eval gamma e1
      | VList(_,_) -> eval gamma e2
      | _ -> raise (Erro_Eval "Em ExMatchList(e0, e1, e2) a expressao e0 nao avalia para um valor lista"))

  (*Just*)
  | ExJust(e0) -> VJust(eval gamma e0)

  (*Nothing*)
  | ExNothing (t0) -> VNothing(t0)

  (*Match Maybe*)
  | ExMatchMaybe(e0, e1, e2) -> (match eval gamma e0 with
      | VJust(_) -> eval gamma e1
      | VNothing(_) -> eval gamma e2
      | _ -> raise (Erro_Eval "Em ExMatchMaybe(e0, e1, e2) a expressao e0 nao avalia para um valor maybe"))

  (*Caso haja uma excecao, eh retornado um erro*)
  | _ -> raise (Erro_Eval "Expressao nao implementada")


(**************************************************************************************************************)
(************************************************ INTERPRETADOR ***********************************************)
(**************************************************************************************************************)

(*Função Auxiliar que Converte Tipo para String *)
let rec tipo_para_string (t:tipo) : string = match t with
    | TyInt  -> "int"
    | TyBool -> "bool"
    | TyFn(t1,t2)   ->  (tipo_para_string t1) ^ " --> " ^ (tipo_para_string t2)
    | TyPar(t1,t2) ->  (tipo_para_string t1) ^ " * "   ^ (tipo_para_string t2)
    | TyList(t0) ->  (tipo_para_string t0) ^ " list"
    | TyMaybe(t0) ->  "maybe "  ^ (tipo_para_string t0)
  
(*Função Auxiliar que Converte Valor para String *)
let rec valor_para_string (v: valor) : string = match v with
    | VNum n -> string_of_int n
    | VTrue -> "true"
    | VFalse -> "false"
    | VPar(v1, v2) -> "(" ^ valor_para_string v1 ^ "," ^ valor_para_string v1 ^ ")"
    | VClosure _ ->  "fn"
    | VClosureRecursivo _ -> "fn"
    | VNil _ -> "[]"
    | VList(v1, v2) -> "[" ^ valor_para_string v1 ^ "," ^ valor_para_string v2 ^ "]"
    | VJust(v0) -> "Just " ^ valor_para_string v0
    | VNothing _ -> "Nothing"

(*Função Principal do Interpretador Sem Ambientes*)
let interpretador (e:expr) : unit =
    try 
        let t = typeinfer [] e in
        let v = eval [] e in
        print_string ((valor_para_string v) ^ " : " ^ (tipo_para_string t))
    with _ ->  print_string ("erro ")
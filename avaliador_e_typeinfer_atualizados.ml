(*Tipos de L1*)
type tipo = 
  | TyInt                   (* int *)      (* Tipo numero inteiro *)
  | TyBool                  (* bool *)     (* Tipo booleano *)
  | TyFn    of tipo * tipo  (* T1-->T2 *)  (* Tipo funcao *)
  | TyPair  of tipo * tipo  (* T1*T2 *)    (* Tipo par *)
  | TyList  of tipo         (* T list *)   (* Tipo Lista *)
  | TyMaybe of tipo         (* maybe T*)   (* Tipo maybe*)  (* POSSIVELMENTE ERRADO*)

(*Tipo ident utilizado por variaveis declaradas*)
type ident = string

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
  | ExNum           of int                           (* Numero inteiro *)
  | ExVar           of ident                         (* Variavel *)
  | ExTrue                                           (* Booleano true *)
  | ExFalse                                          (* Booleano false *)
  | ExIf            of expr * expr * expr            (* If then else *)
  | ExFn            of ident * tipo * expr           (* Funcao anonima *)
  | ExApp           of expr * expr                   (* Aplicacao *)
  | ExLet           of ident * tipo * expr * expr    (* Funcao declarada *)
  | ExLetRec        of ident * tipo * expr * expr    (* Funcao declarada recursiva *)
  | ExBinop         of bop * expr * expr             (* Operacao binaria *)
  | ExPair          of expr * expr                   (* Par *)
  | ExFst           of expr                          (* Primeiro elemento de um par *)
  | ExSnd           of expr                          (* Segundo elemento de um par *)
  | ExNil           of tipo                          (* Lista vazia *)
  | ExList          of expr * expr                   (* Lista nao vazia *)
  | ExHead          of expr                          (* Retorna o primeiro elemento de uma lista *)
  | ExTail          of expr                          (* Retorna uma lista sem o primeiro elemento dela *)
  | ExMatchList     of expr * expr * expr            (* Retorna expressoes diferentes dependendo se a lista for vazia ou nao*)
  | ExJust          of expr                          (* *) (* POSSIVELMENTE ERRADO*)
  | ExNothing       of tipo                          (* *) (* POSSIVELMENTE ERRADO*)
  | ExMatchNothing  of expr * expr * expr            (* *) (* POSSIVELMENTE ERRADO*)

(*Declaracao dos ambiente das expressoes*)
type tenv = (ident * tipo) list

(* Valores *) (* ADICIONAR O QUE ESTIVER FALTANDO*)
type valor =
    VNum of int
  | VTrue
  | VFalse
  | VPair of valor * valor
  | VNil
  | VList of valor * valor
  | VMaybe of valor
  | VClos  of ident * expr * renv
  | VRclos of ident * ident * expr * renv
and 
  renv = (ident * valor) list

(*Funcao polimorfica que ajuda a verificar todo o ambiente de uma expressao*)
let rec lookup a k = match a with
  | [] -> None
  | (y,i) :: tl -> if (y=k) then Some i else lookup tl k 

(*Funcao polimorfica que permite atualizar um ambiente de uma expressao*)
let rec update a k i = (k,i) :: a   

(* Erro acionado caso uma expressao seja mal tipada*) 
exception TypeError

(* Erro acionado caso o parser deixe passar uma expressao com erro de sintaxe*) 
exception BugParser

(* Erro acionado caso o tipeinfer tenha deixado passar algum erro*) 
exception BugTypeInfer

(* Erro acionado caso tente-se fazer uma divisao por zero*)
exception ErroDivisaoZero

(*Erro acionado quando algo nao tiver sido implementado*)
exception NotImplemented


(**************************************************************************************************************)
(**************************************************Typeinfer***************************************************)
(**************************************************************************************************************)

(*Funcao que faz a inferencia de tipo de uma expressao da linguagem L1*)
let rec typeinfer (gamma: tenv) (e:expr) : tipo  = match e with

  (* Numero Inteiro *)
  | ExNum _ -> TyInt

  (* Variavel *)
  | ExVar x -> (match lookup gamma x with
      | Some t -> t
      | None -> raise TypeError)

  (* Booleano True *)
  | ExTrue  -> TyBool

  (* Booleano False *)
  | ExFalse -> TyBool

  (* If then else *)
  | ExIf(e1,e2,e3) -> (match typeinfer gamma e1 with
      | TyBool -> 
          let t2 = typeinfer gamma e2 in
          let t3 = typeinfer gamma e3 in
          if t2 = t3 then t2 
          else raise TypeError 
      | _ -> raise TypeError) 

  (* Funcao Anonima *)
  | ExFn(x,t,e1) -> let t1 = typeinfer (update gamma x t) e1 in TyFn(t,t1)
  
  (* Aplicacao *)
  | ExApp(e1,e2) -> (match typeinfer gamma e1 with
      | TyFn(t, t') ->  if (typeinfer gamma e2) = t then t' else raise TypeError
      | _ -> raise TypeError)

  (* Funcao Declarada *)
  | ExLet(x,t,e1,e2) -> 
      if (typeinfer gamma e1) = t then typeinfer (update gamma x t) e2 else raise TypeError

  (* Funcao Declarada Recursiva *)
  | ExLetRec(f,(TyFn(t1,t2) as tf), ExFn(x,tx,e1), e2) ->
      let gamma_tf = update gamma f tf in
      let gamma_tf_tx = update gamma_tf x tx in
      (if (typeinfer gamma_tf_tx e1) = t2 then typeinfer gamma_tf e2
      else raise TypeError)

  (* Funcao Declarada Recursiva *)
  | ExLetRec _ -> raise BugParser

  (* Operacao Binaria *)
  | ExBinop(oper,e1,e2) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      (if t1 = TyInt && t2 = TyInt then (match oper with
          | Soma | Subtracao | Multiplicacao | Divisao -> TyInt
          | Igual | MenorQue | MaiorQue | MaiorOuIgual | MenorOuIgual -> TyBool
          | _ -> raise TypeError)
      else if t1 = TyBool && t2 = TyBool then (match oper with
          | And | Or -> TyBool
          | _ -> raise TypeError)
      else raise TypeError)

  (* Par *)
  | ExPair(e1,e2) -> TyPair(typeinfer gamma e1, typeinfer gamma e2) 

  (* Primeiro Elemento de um Par *)
  | ExFst e1 -> (match typeinfer gamma e1 with
      | TyPair(t1,_) -> t1
      | _ -> raise TypeError)

  (* Segundo Elemento de um Par *)
  | ExSnd e1 -> (match typeinfer gamma e1 with
      | TyPair(_,t2) -> t2
      | _ -> raise TypeError)

  (* Lista Vazia *)
  | ExNil(t) -> t

  (* Lista com Elementos *)
  | ExList(e1, e2) ->
      let t1 = typeinfer gamma e1 in 
      let t2 = typeinfer gamma e2 in (match e2 with
          | ExNil(_) | ExList(_) -> if t1 = t2 then t2 else raise TypeError 
          | _ -> raise TypeError)

  (* Primeiro Elemento de uma Lista*)
  | ExHead(e0) -> (match e0 with
      | ExList(e1, e2) -> typeinfer gamma e0
      | _ -> raise TypeError)

  (* Segundo Elemento de uma Lista*)
  | ExTail(e0) -> (match e with
      | ExList(e1, e2) -> typeinfer gamma e0
      | _ -> raise TypeError)

  (* Verifica se a Lista e Vazia ou Nao*)
  | ExMatchList(e1,e2,e3) ->
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in
      (if t2 = t3 then t2 else raise TypeError)

  (* POSSIVELMENTE ERRADO*)
  | ExJust(e0) -> typeinfer gamma e0 

  (* POSSIVELMENTE ERRADO*)
  | ExNothing(t) -> t

  (* POSSIVELMENTE ERRADO*)
  | ExMatchNothing(e1,e2,e3) ->
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in
      (if t2 = t3 then t2 else raise TypeError)


(**************************************************************************************************************)
(**********************************************Avaliador Big Step**********************************************)
(**************************************************************************************************************)

(* Funcao que realiza as operacoes binarias*)
let faz_operacao (oper: bop) (v1: valor) (v2: valor) = match (oper, v1, v2) with
  | (Soma, VNum(n1), VNum(n2)) -> VNum(n1 + n2)
  | (Subtracao, VNum(n1), VNum(n2)) -> VNum(n1 - n2)
  | (Multiplicacao, VNum(n1), VNum(n2)) -> VNum(n1 * n2)
  | (Divisao, VNum(n1), VNum(n2)) -> if n2 = 0 then raise ErroDivisaoZero else VNum(n1 / n2)
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
  | _ -> raise BugTypeInfer


(* Funcao que faz apenas o passo big step*)
let rec eval (gamma:renv) (e:expr) : valor = match e with

  (* Numero Inteiro *)
  | ExNum n -> VNum n

  (* Variavel *)
  | ExVar x -> (match lookup gamma x with
      | Some v -> v
      | None -> raise BugTypeInfer)

  (* Booleano True *)
  | ExTrue -> VTrue

  (* Booleano False *)
  | ExFalse -> VFalse

  (* If then else *)
  | ExIf(e1,e2,e3) -> (match eval gamma e1 with
      | VTrue -> eval gamma e2
      | VFalse -> eval gamma e3
      | _ -> raise BugTypeInfer)

  (* Funcao Anonima *)
  | ExFn (x,_,e1) ->  VClos(x,e1,gamma)

  (* Aplicacao *)
  | ExApp(e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      (match v1 with
        | VClos(x,ebdy,gamma') ->
            let gamma'' = update gamma' x v2
            in eval gamma'' ebdy
        | VRclos(f,x,ebdy,gamma') ->
            let gamma''  = update gamma' x v2 in
            let gamma''' = update gamma'' f v1
            in eval gamma''' ebdy
        | _ -> raise BugTypeInfer)

  (* Funcao Declarada *)
  | ExLet(x,_,e1,e2) -> 
      let v1 = eval gamma e1 in
      eval (update gamma x v1) e2

  (* Funcao Declarada Recursiva *)
  | ExLetRec(f,TyFn(t1,t2),ExFn(x,tx,e1), e2) when t1 = tx ->
      let gamma'= update gamma f (VRclos(f,x,e1,gamma))
      in eval gamma' e2

  (* Funcao Declarada Recursiva *)
  | ExLetRec _ -> raise BugParser

  (* Operacao Binaria *)
  | ExBinop(oper,e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      faz_operacao oper v1 v2

  (* Par *)
  | ExPair(e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VPair(v1,v2)

  (* Primeiro Elemento de um Par *)
  | ExFst e -> (match eval gamma e with
      | VPair(v1,_) -> v1
      | _ -> raise BugTypeInfer)

  (* Segundo Elemento de um Par *)
  | ExSnd e -> (match eval gamma e with
      | VPair(_,v2) -> v2
      | _ -> raise BugTypeInfer)

  (* Lista Vazia *)
  | ExNil _ -> VNil

  (* Lista com Elementos *)
  | ExList(e1, e2) -> 
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VList(v1,v2)

  (* Primeiro Elemento de uma Lista*)
  | ExHead _ -> raise NotImplemented

  (* Segundo Elemento de uma Lista*)
  | ExTail _ -> raise NotImplemented

  (* Verifica se a Lista e Vazia ou Nao*)
  | ExMatchList _ -> raise NotImplemented

  (* POSSIVELMENTE ERRADO*)
  | ExJust _ -> raise NotImplemented

  (* POSSIVELMENTE ERRADO*)
  | ExNothing _ -> raise NotImplemented

  (* POSSIVELMENTE ERRADO*)
  | ExMatchNothing _ -> raise NotImplemented


(*
let interpretador (e:expr) : unit = 
*)
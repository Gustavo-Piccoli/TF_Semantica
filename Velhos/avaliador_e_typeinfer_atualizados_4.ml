(*Tipos de L1*)
type tipo = 
  | TyInt                   (* int *)      (* Tipo numero inteiro *)
  | TyBool                  (* bool *)     (* Tipo booleano *)
  | TyFn    of tipo * tipo  (* T1-->T2 *)  (* Tipo funcao *)
  | TyPar   of tipo * tipo  (* T1*T2 *)    (* Tipo par *)
  | TyList  of tipo         (* T list *)   (* Tipo Lista *)
  | TyMaybe of tipo         (* maybe T*)   (* Tipo maybe*)

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
  | ExVar           of string                        (* Variavel *)
  | ExTrue                                           (* Booleano true *)
  | ExFalse                                          (* Booleano false *)
  | ExIf            of expr * expr * expr            (* If then else *)
  | ExFn            of string * tipo * expr          (* Funcao anonima *)
  | ExApp           of expr * expr                   (* Aplicacao *)
  | ExLet           of string * tipo * expr * expr   (* Funcao declarada *)
  | ExLetRec        of string * tipo * expr * expr   (* Funcao declarada recursiva *)
  | ExBinop         of bop * expr * expr             (* Operacao binaria *)
  | ExPar           of expr * expr                   (* Par *)
  | ExFst           of expr                          (* Primeiro elemento de um par *)
  | ExSnd           of expr                          (* Segundo elemento de um par *)
  | ExNil           of tipo                          (* Lista vazia *)
  | ExList          of expr * expr                   (* Lista nao vazia *)
  | ExHead          of expr                          (* Retorna o primeiro elemento de uma lista *)
  | ExTail          of expr                          (* Retorna uma lista sem o primeiro elemento dela *)
  | ExMatchList     of expr * expr * expr            (* Retorna expressoes diferentes dependendo se a lista for vazia ou nao*)
  | ExJust          of expr                          (* *)
  | ExNothing       of tipo                          (* *)
  | ExMatchMaybe    of expr * expr * expr            (* *)

(*Declaracao dos ambiente das expressoes*)
type ambiente_tipo = (string * tipo) list

(* Valores *)
type valor =
    VNum      of int                                        (* Numero inteiro *)
  | VTrue                                                   (* Booleano true *)
  | VFalse                                                  (* Booleano false *)
  | VPar      of valor * valor                              (* Par *)
  | VClos     of string * expr * ambiente_valor             (* Funcao anonima *)
  | VRclos    of string * string * expr * ambiente_valor    (* Funcao declarada recursiva *)
  | VNil      of tipo                                       (* Lista vazia *)
  | VList     of valor * valor                              (* Lista nao vazia *)
  | VJust     of valor                                      (* Maybe com valor *)
  | VNothing  of tipo                                       (* Maybe vazio *)
and 
  ambiente_valor = (string * valor) list

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
let rec typeinfer (gamma: ambiente_tipo) (e:expr) : tipo  = match e with

  (* Numero Inteiro *)
  | ExNum _ -> TyInt

  (* Variavel *)
  | ExVar(x) -> (match lookup gamma x with
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
  | ExPar(e1,e2) -> TyPar(typeinfer gamma e1, typeinfer gamma e2) 

  (* Primeiro Elemento de um Par *)
  | ExFst(e1) -> (match typeinfer gamma e1 with
      | TyPar(t1,_) -> t1
      | _ -> raise TypeError)

  (* Segundo Elemento de um Par *)
  | ExSnd(e1) -> (match typeinfer gamma e1 with
      | TyPar(_,t2) -> t2
      | _ -> raise TypeError)

  (* Lista Vazia *)
  | ExNil(t0) -> (match t0 with
      | TyList(t1) -> TyList(t1)
      | _ -> raise TypeError)

  (* Lista com Elementos *)
  | ExList(e1, e2) ->
      let t1 = typeinfer gamma e1 in 
      let t2 = typeinfer gamma e2 in (match t2 with
          | TyList(t3) -> if t1 = t3 then TyList(t1) else raise TypeError
          | _ -> raise TypeError)

  (* Primeiro Elemento de uma Lista*)
  | ExHead(e0) -> (match e0 with
      | ExList(e1, e2) -> let t0 = typeinfer gamma e0 in (match t0 with
          | TyList(t1) -> t1
          | _ -> raise TypeError)
      | _ -> raise TypeError)

  (* Segundo Elemento de uma Lista*)
  | ExTail(e0) -> (match e0 with
      | ExList(e1, e2) -> let t0 = typeinfer gamma e0 in (match t0 with
          | TyList(t1) -> TyList(t1)
          | _ -> raise TypeError)
      | _ -> raise TypeError)

  (* Verifica se a Lista e Vazia ou Nao*)
  | ExMatchList(e1,e2,e3) ->
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in
      (if t2 = t3 then t2 else raise TypeError)

  (* POSSIVELMENTE ERRADO*)
  | ExJust(e0) -> TyMaybe(typeinfer gamma e0)
    

  (* POSSIVELMENTE ERRADO*)
  | ExNothing(t) -> TyMaybe(t)

  (* POSSIVELMENTE ERRADO*)
  | ExMatchMaybe(e1,e2,e3) ->
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
let rec eval (gamma:ambiente_valor) (e:expr) : valor = match e with

  (* Numero Inteiro *)
  | ExNum(n) -> VNum(n)

  (* Variavel *)
  | ExVar(x) -> (match lookup gamma x with
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
  | ExBinop(operador,e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      faz_operacao operador v1 v2

  (* Par *)
  | ExPar(e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VPar(v1,v2)

  (* Primeiro Elemento de um Par *)
  | ExFst(e0) -> (match e0 with
      | ExPar(e1,_) -> eval gamma e1
      | _ -> raise BugTypeInfer)

  (* Segundo Elemento de um Par *)
  | ExSnd(e0) -> (match e0 with
      | ExPar(_,e2) -> eval gamma e2
      | _ -> raise BugTypeInfer)

  (* Lista Vazia *)
  | ExNil(t0) -> VNil(t0)

  (* Lista com Elementos *)
  | ExList(e1, e2) -> 
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VList(v1,v2)

  (* Primeiro Elemento de uma Lista*)
  | ExHead(e0) -> (match e0 with
      | ExList(e1,_) -> eval gamma e1
      | _ -> raise BugTypeInfer)

  (* Segundo Elemento de uma Lista*)
  | ExTail(e0) -> (match e0 with
      | ExList(_,e2) -> eval gamma e2
      | _ -> raise BugTypeInfer)

  (* Verifica se a Lista e Vazia ou Nao*)
  | ExMatchList(e0, e1, e2) -> (match eval gamma e0 with
      | VNil(_) -> eval gamma e1
      | VList(_,_) -> eval gamma e2
      | _ -> raise BugTypeInfer)

  (* Uma expressão just retorna um valor just *)
  | ExJust(e0) -> VJust(eval gamma e0)

  (* Uma expressão nothing retona um valor nothing *)
  | ExNothing (t0) -> VNothing(t0)

  (* Verifica se um tipo maybe é just ou nothing, e1 para just e e2 para nothing *)
  | ExMatchMaybe(e0, e1, e2) -> (match eval gamma e0 with
      | VJust(_) -> eval gamma e1
      | VNothing(_) -> eval gamma e2
      | _ -> raise BugTypeInfer)



(**************************************************************************************************************)
(************************************************ INTERPRETADOR ***********************************************)
(**************************************************************************************************************)

(* Função Auxiliar que Converte Tipo para String *)
let rec tipo_para_string (t:tipo) : string = match t with
    | TyInt  -> "int"
    | TyBool -> "bool"
    | TyFn(t1,t2)   ->  "("  ^ (tipo_para_string t1) ^ " --> " ^ (tipo_para_string t2) ^ ")"
    | TyPar(t1,t2) ->  "("  ^ (tipo_para_string t1) ^ " * "   ^ (tipo_para_string t2) ^ ")"
    | TyList(t0) ->  "["  ^ (tipo_para_string t0) ^ "]"
    | TyMaybe(t0) ->  "["  ^ (tipo_para_string t0) ^ "]"
  
(* Função Auxiliar que Converte Valor para String *)
let rec valor_para_string (v: valor) : string = match v with
    | VNum n -> string_of_int n
    | VTrue -> "true"
    | VFalse -> "false"
    | VPar(v1, v2) -> "(" ^ valor_para_string v1 ^ "," ^ valor_para_string v1 ^ ")"
    | VClos _ ->  "fn"
    | VRclos _ -> "fn"
    | VNil _ -> "[]"
    | VList(v1, v2) -> "[" ^ valor_para_string v1 ^ "," ^ valor_para_string v2 ^ "]"
    | VJust(v0) -> "Just " ^ valor_para_string v0
    | VNothing _ -> "Nothing"

(* Função Principal do Interpretador *)
let interpretador (e:expr) : unit =
    try 
        let t = typeinfer [] e in
        let v = eval [] e in
        print_string ((valor_para_string v) ^ " : " ^ (tipo_para_string t))
    with _ ->  print_string ("erro ")
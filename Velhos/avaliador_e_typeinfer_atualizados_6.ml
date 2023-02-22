(*Tipos de L1*)
type tipo = 
  | TyInt                   (* int *)      (*Tipo numero inteiro*)
  | TyBool                  (* bool *)     (*Tipo booleano*)
  | TyFn    of tipo * tipo  (* T1-->T2 *)  (*Tipo funcao*)
  | TyPar   of tipo * tipo  (* T1*T2 *)    (*Tipo par*)
  | TyList  of tipo         (* T list *)   (*Tipo Lista*)
  | TyMaybe of tipo         (* maybe T *)   (*Tipo maybe*)

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

(*Declaracao dos ambiente de tipos das expressoes*)
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
exception Erro_Typeinfer

(*Erro acionado caso haja um problema na avaliação da expressao*) 
exception Erro_Eval

(*Erro acionado caso tente-se fazer uma divisao por zero*)
exception Erro_Divisao_Zero


(**************************************************************************************************************)
(**************************************************Typeinfer***************************************************)
(**************************************************************************************************************)

(*Funcao que faz a inferencia de tipo de uma expressao da linguagem L1*)
let rec typeinfer (gamma: ambiente_tipo) (e:expr) : tipo  = match e with

  (*Numero Inteiro*)
  (*Recebe uma expressao com um numero inteiro e retorna o tipo inteiro*)
  | ExNum _ -> TyInt

  (*Variavel*) 
  (*Chama a funcao lookup para varrer o ambiente e procurar se ha o identificador requerida 
  nele, caso haja, retorna o tipo associado ao identificador*)
  | ExVar(x) -> (match lookup gamma x with
      | Some t -> t
      | None -> raise Erro_Typeinfer)

  (*Booleano True*)
  (*Recebe uma expressao booleano true e retorna o tipo booleano*)
  | ExTrue  -> TyBool

  (*Booleano False*)
  (*Recebe uma expressao booleano false e retorna o tipo booleano*)
  | ExFalse -> TyBool

  (*If Then Else*) 
  (*Recebe tres expressoes e1, e2 e e3. Se o e1 for do tipo booleano e ambas e2 e e3 forem do mesmo tipo, retorna o tipo de e2 e e3*)
  | ExIf(e1,e2,e3) -> (match typeinfer gamma e1 with
      | TyBool -> 
          if (typeinfer gamma e2) = (typeinfer gamma e3)
          then typeinfer gamma e2
          else raise Erro_Typeinfer
      | _ -> raise Erro_Typeinfer)

  (*Funcao Anonima*) 
  (*Recebe uma expressão de funcao nao declarada (que eh composta por um identificador x, um tipo t e uma expressao e1) 
  e retorna um tipo fn (que eh composta por um tipo de entrada, que eh o t de antes e um tipo de saida).
  O tipo da saida eh definido apos atualizarmos o ambiente com o identificador x da entrada associado ao seu tipo t.
  Então fazemos a inferencia de tipo da expressao e1 com esse ambiente atualizado. Esse eh o tipo da saida do tipo fn.*)
  | ExFn(x,t,e1) -> TyFn(t, (typeinfer(update gamma x t) e1))
  
  (*Aplicacao*)
  (*Recebemos uma expressao de aplicacao que possui duas expressoes internas e1 e e2. e1 tem que ter ser do tipo (fn(t1,t2)), caso contrario eh um 
  erro. Assim ela tem um tipo de entrada t1 e um tipo de saida t2. e2 tem que ter o seu tipo igual ao tipo t1, desse modo eh retornado o tipo t2*)
  | ExApp(e1,e2) -> (match typeinfer gamma e1 with
      | TyFn(t1, t2) ->  if (typeinfer gamma e2) = t1 then t2 else raise Erro_Typeinfer
      | _ -> raise Erro_Typeinfer)

  (*Funcao Declarada*)
  (*A expressao let eh composta por um identificador x, um tipo t e as expressoes e1 e e2. Se o tipo de e1 for igual a t 
  declarado, entao eh retorna a inferencia de tipo de e2 com o ambiente atualizado com o identificador x do tipo t*)
  | ExLet(x,t,e1,e2) -> 
      if (typeinfer gamma e1) = t
      then typeinfer (update gamma x t) e2
      else raise Erro_Typeinfer

  (*Funcao Declarada Recursiva*)
  (*A expressao let rec eh composta por um indentificador f, um tipo fn (que possui um tipo de entrada t1 e um tipo de saida t2),uma expressao fn 
  (que eh composta por um identificador x, um tipo t3 e uma expressao e1) e uma expressao e2. Fazemos um if then else, onde fazemos a inferencia 
  de tipo de e1 com um ambiente atualizado com o identificador f associado ao tipo fn(t1,t2) e com o identificador x associado ao tipo t3 da expressao fn. 
  Caso essa inferencia de e1 com esse ambiente atualizado seja igual ao tipo t2, fazemos a inferencia de tipo de e2 com o ambiente original, mas atualizado com o
  identificador f associado ao tipo fn(t1,t2)*)
  | ExLetRec(f, TyFn(t1,t2), ExFn(x,t3,e1), e2) ->
      if (typeinfer (update (update gamma f (TyFn(t1,t2))) x t3) e1) = t2
      then typeinfer (update gamma f (TyFn(t1,t2))) e2
      else raise Erro_Typeinfer

  (*Operacao Binaria*)
  (*A expressao binop recebe um operador e duas expressoes, se ambas expressoes forem do tipo int e o operador for um dos operadores validos,
  sera retornado o tipo int. Se ambas forem do tipo booleano e o operador for um dos operadores validos, sera retornado o tipo booleano*)
  | ExBinop(op,e1,e2) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      (if (t1 = TyInt && t2 = TyInt) then (match op with
          | Soma | Subtracao | Multiplicacao | Divisao -> TyInt
          | Igual | MenorQue | MaiorQue | MaiorOuIgual | MenorOuIgual -> TyBool
          | _ -> raise Erro_Typeinfer)
      else if t1 = TyBool && t2 = TyBool then (match op with
          | And | Or -> TyBool
          | _ -> raise Erro_Typeinfer)
      else raise Erro_Typeinfer)

  (*Par*)
  (*Na expressao par e fazemos a inferencia de tipo das duas expressoes de dentro dela. Assim retornamos o tipo par composto de dois tipos, caso ambas sejam bem tipadas*)
  | ExPar(e1,e2) -> TyPar(typeinfer gamma e1, typeinfer gamma e2) 

  (*Primeiro Elemento de um Par*)
  (*A expressao first recebebe uma expressao par e verifica se eh bem tipada, caso seja, retorna o primeiro tipo do tipo par*)
  | ExFst(e1) -> (match typeinfer gamma e1 with
      | TyPar(t1,t2) -> t1
      | _ -> raise Erro_Typeinfer)

  (*Segundo Elemento de um Par*)
  (*A expressao second recebe uma expressao par e verifica se eh bem tipada, caso seja, retorna o segundo tipo do tipo par*)
  | ExSnd(e1) -> (match typeinfer gamma e1 with
      | TyPar(t1,t2) -> t2
      | _ -> raise Erro_Typeinfer)

  (*Lista Vazia*)
  (*Recebe uma expressao nil que possui um tipo. Se esse tipo for um tipo lista de um tipo t, eh retornado esse tipo lista de um tipo t*)
  | ExNil(t0) -> (match t0 with
      | TyList(t1) -> TyList(t1)
      | _ -> raise Erro_Typeinfer)

  (*Lista com Elementos*)
  (*Recebe uma expressao list que possui duas expressoes. A primeira expressao tem que ter o mesmo tipo do tipo lista da segunda expressao,
  caso contrario, eh um erro. Se for bem tipada, eh retornado o tipo lista(tipo) da segunda expressao*)
  | ExList(e1, e2) -> (match typeinfer gamma e2 with
      | TyList(t2) -> if (typeinfer gamma e1) = t2 then TyList(t2) else raise Erro_Typeinfer
      | _ -> raise Erro_Typeinfer)

  (*Primeiro Elemento de uma Lista*)
  (*Recebe uma lista, verifica se eh bem tipada, caso sim, retorna o tipo do head da lista. Caso nao, retorna erro*)
  | ExHead(e0) -> (match typeinfer gamma e0 with
      | TyList(t0) -> t0
      | _ -> raise Erro_Typeinfer)

  (*Segundo Elemento de uma Lista*)
  (*Recebe uma lista, verifica se eh bem tipada, caso sim, retorna o tipo do tail da lista, que eh uma lista de um tipo t. Caso nao, retorna erro*)
  | ExTail(e0) -> (match typeinfer gamma e0 with
      | TyList(t0) -> TyList(t0)
      | _ -> raise Erro_Typeinfer)

  (*Match List*)
  (*Recebe tres expressoes, a primeira deve ser do tipo lista de um tipo t e as duas outras devem ter o mesmo tipo entre si. 
  Assim, retorna o tipo das expressoes 2 e 3. Caso contrário, retorna erro*)
  | ExMatchList(e1,e2,e3) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in (match t1 with
        | TyList(t4) -> if t2 = t3 then t2 else raise Erro_Typeinfer
        | _ -> raise Erro_Typeinfer)

  (*Just*)
  (*Recebe uma expressao just que tem uma expressao interna, faz a inferencia de tipo dessa expressao interna, caso seja bem tipada, 
  retorna um tipo maybe com essa inferencia. Caso retorna um erro*)
  | ExJust(e0) -> TyMaybe(typeinfer gamma e0)

  (*Nothing*)
  (*Recebe uma expressao nothing que tem uma tipo interno e retorna um tipo maybe com esse tipo*)
  | ExNothing(t) -> TyMaybe(t)

  (*Match Maybe*)
  (*Recebe tres expressoes, a primeira deve ser do tipo maybe e as duas outras devem ter o mesmo tipo entre si. 
   Assim, retorna o tipo das expressoes 2 e 3. Caso contrário, retorna erro*)
  | ExMatchMaybe(e1,e2,e3) ->
      let t1 = typeinfer gamma e1 in
      let t2 = typeinfer gamma e2 in
      let t3 = typeinfer gamma e3 in (match t1 with
        | TyMaybe(t4) -> if t2 = t3 then t2 else raise Erro_Typeinfer
        | _ -> raise Erro_Typeinfer)

  (*Excecoes retornam erros*)
  | _ -> raise Erro_Typeinfer


(**************************************************************************************************************)
(**********************************************Avaliador Big Step**********************************************)
(**************************************************************************************************************)

(*Funcao que realiza as operacoes binarias*)
(*Recebe dois valores e uma operacao. A funcao faz a operacao sobre esses dois valores*)
let faz_operacao (oper: bop) (v1: valor) (v2: valor) = match (oper, v1, v2) with
  | (Soma, VNum(n1), VNum(n2)) -> VNum(n1 + n2)
  | (Subtracao, VNum(n1), VNum(n2)) -> VNum(n1 - n2)
  | (Multiplicacao, VNum(n1), VNum(n2)) -> VNum(n1 * n2)
  | (Divisao, VNum(n1), VNum(n2)) -> if n2 = 0 then raise Erro_Divisao_Zero else VNum(n1 / n2)
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
  | _ -> raise Erro_Eval


(*Funcao que faz o passo big step*)
let rec eval (gamma:ambiente_valor) (e:expr) : valor = match e with

  (*Numero Inteiro*)
  (*Uma expressao numero retorna um valor numero*)
  | ExNum(n) -> VNum(n)

  (*Variavel*) 
  (*Chama a funcao lookup para varrer o ambiente e procurar se ha o identificador requerida nele, caso haja, retorna o valor associado ao identificador*)
  | ExVar(x) -> (match lookup gamma x with
      | Some v -> v
      | None -> raise Erro_Eval)

  (*Booleano True*)
  (*Uma expressao true retorna o valor true*)
  | ExTrue -> VTrue

  (*Booleano False*)
  (*Uma expressao false retorna o valor false*)
  | ExFalse -> VFalse

  (*If Then Else*)
  (*Recebe tres expressoes e1, e2 e e3. Avalia e1, se tiver o valor true, retorna a avaliacao de e2.
  Caso a avaliacao de e1 seja o valor false, retorna a avaliacao de e3*)
  | ExIf(e1,e2,e3) -> (match eval gamma e1 with
      | VTrue -> eval gamma e2
      | VFalse -> eval gamma e3
      | _ -> raise Erro_Eval)

  (*Funcao Anonima*) 
  (*Recebe uma expressão de funcao nao declarada (que eh composta por um identificador x, um tipo t e uma expressao e1) 
  e retorna uma closure para a funcao. Essa closure eh um valor que eh composto pelo identificador x, a expressao e1
  e o ambiente recebid anteriormente (que nesse caso, representa o escopo onde a expressao e1 pode ser usada)*)
  | ExFn (x,_,e1) ->  VClosure(x,e1,gamma)

  (*Aplicacao*)
  (*Recebemos uma expressao de aplicacao que possui duas expressoes internas e1 e e2. e1 tem que ter ser do tipo (fn(t1,t2)). Assim eh feita a avaliacao de e1,
  que retorna um valor closure caso e1 nao seja recursiva, ou um valor closure recursivo caso e1 seja recursiva. Caso seja um closure nao recursivo (composto por x, e3 e novo_gama), 
  retornamos a avaliacao de e3 com o ambiente novo_gamma atualizado com o valor (eval gamma e2) associado ao identificador x. Caso o valor cloresure seja recursivo 
  (composto por f, x, e3, novo_gamma), retornamos uma avaliacao de e3 com o ambiente atualizado de acorodo com o código abaixo*)
  | ExApp(e1,e2) -> (match eval gamma e1 with
        | VClosure(x,e3,novo_gamma) -> eval (update novo_gamma x (eval gamma e2)) e3
        | VClosureRecursivo(f,x,e3,novo_gamma) -> eval (update (update novo_gamma x (eval gamma e2)) f (eval gamma e1)) e3
        | _ -> raise Erro_Eval)

  (*Funcao Declarada*)
  (*A expressao let eh composta por um identificador x, um tipo t e as expressoes e1 e e2. Eh retornada a avaliacao de e2 com o ambiente atualizado com o identificador x
  associado a valor da avaliacao de e1*)
  | ExLet(x,_,e1,e2) -> eval (update gamma x (eval gamma e1)) e2 

  (*Funcao Declarada Recursiva*)
  (*A expressao let rec eh composta por um indentificador f, um tipo fn (que possui um tipo de entrada t1 e um tipo de saida t2),uma expressao fn 
  (que eh composta por um identificador x, um tipo t3 e uma expressao e1) e uma expressao e2. Se o t1 for igual a t3, fazemos a avaliacao de e2
  com um ambiente atualizado com um closure recursivo composto por f, x, e1 e gamma, associado ao identificador f*)
  | ExLetRec(f,TyFn(t1,t2),ExFn(x,t3,e1), e2) when t1 = t3 -> eval (update gamma f (VClosureRecursivo(f,x,e1,gamma))) e2

  (*Operacao Binaria*)
  (*A expressao binip eh composta por um operador e duas expressoes. Eh feita a avaliacao das duas expressoes e acionada a funcao faz_operacao,
  que recebe o operando com as duas avaliacoes das expressoes. Assim eh retornado o resultado das operacao sobre os dois valores*)
  | ExBinop(op,e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      faz_operacao op v1 v2

  (*Par*)
  (*A expressao para eh composta por duas expressoes. Eh retornado um valor par composto com as avaliacoes das duas expressoes da expressao par*)
  | ExPar(e1,e2) ->
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VPar(v1,v2)

  (*Primeiro Elemento de um Par*)
  (*A expressao first recebe uma expressao par e retorna a avaliacao da primeira expressao do tipo par*)
  | ExFst(e0) -> (match e0 with
      | ExPar(e1,_) -> eval gamma e1
      | _ -> raise Erro_Eval)

  (*Segundo Elemento de um Par*)
  (*A expressao second recebe uma expressao par e retorna a avaliacao da segunda expressao do tipo par*)
  | ExSnd(e0) -> (match e0 with
      | ExPar(_,e2) -> eval gamma e2
      | _ -> raise Erro_Eval)

  (*Lista Vazia*)
  (*Uma expressao nil possui um tipo internto t. A avaliação dela retona um valor nil com o tipo t*)
  | ExNil(t0) -> VNil(t0)

  (*Lista com Elementos*)
  (*A expressao list eh composta por duas expressoes. A avaliacao dela retorna um valor list composto com as avaliacoes das duas expressoes*)
  | ExList(e1, e2) -> 
      let v1 = eval gamma e1 in
      let v2 = eval gamma e2 in
      VList(v1,v2)

  (*Primeiro Elemento de uma Lista*)
  (*A expressao head espera uma expressao list, que eh composto por duas expressoes. A avaliacao dela retorna a avaliacao da primeira expressao da expressao list*)
  | ExHead(e0) -> (match e0 with
      | ExList(e1,_) -> eval gamma e1
      | _ -> raise Erro_Eval)

  (*Segundo Elemento de uma Lista*)
  (*A expressao tail espera uma expressao list, que eh composto por duas expressoes. A avaliacao dela retorna a avaliacao da segunda expressao da expressao list*)
  | ExTail(e0) -> (match e0 with
      | ExList(_,e2) -> eval gamma e2
      | _ -> raise Erro_Eval)

  (*Verifica se a Lista e Vazia ou Nao*)
  (*A expressao match list espera uma expressao list e0, e duas expressoes e1 e e2. A avaliacao da expressao matchlist retorna um valor nil ou um valor list.
  Se a avaliacao da expressao e0 for nil, a avaliacao da expressao e1 eh retornada. Se o valor for list, a avaliacao da expressao e2 eh retornada*)
  | ExMatchList(e0, e1, e2) -> (match eval gamma e0 with
      | VNil(_) -> eval gamma e1
      | VList(_,_) -> eval gamma e2
      | _ -> raise Erro_Eval)

  (*Just*)
  (*Uma expressao just eh composta por uma expressao e0. A avaliacao de um expressao just retorna um valor just com a avaliacao de e0*)
  | ExJust(e0) -> VJust(eval gamma e0)

  (*Nothing*)
  (*Uma expressao nothing eh composta por um tipo t0. A avaliacao de um expressao nothing retorna um valor nothing com o tipo t0*)
  | ExNothing (t0) -> VNothing(t0)

  (*Match Maybe*)
  (*Uma expressao match maybe espera uma expressao maybe e0, e duas expressoes e1 e e2. A avaliacao da expressao e0 retorna um valor nothing ou um valor just.
  Se a avaliacao da expressao e0 for nothing, a avaliacao da expressao e1 eh retornada. Se o valor for just, a avaliacao da expressao e2 eh retornada*)
  | ExMatchMaybe(e0, e1, e2) -> (match eval gamma e0 with
      | VJust(_) -> eval gamma e1
      | VNothing(_) -> eval gamma e2
      | _ -> raise Erro_Eval)

  (*Caso haja uma excecao, eh retornado um erro*)
  | _ -> raise Erro_Eval


(**************************************************************************************************************)
(************************************************ INTERPRETADOR ***********************************************)
(**************************************************************************************************************)

(*Funcao que transforma um valor em um tipo*)
let rec valor_para_tipo (v: valor) : tipo = match v with
  | VNum(n) -> TyInt
  | VTrue -> TyBool
  | VFalse -> TyBool
  | VPar(v1,v2) -> TyPar(valor_para_tipo v1, valor_para_tipo v2)
  | VNil(t) -> TyList(t)
  | VList(v1,v2) -> (if TyList(valor_para_tipo v1) = (valor_para_tipo v2) then TyList(valor_para_tipo v1) else raise Erro_Typeinfer)
  | VJust(v) -> TyMaybe(valor_para_tipo v)
  | VNothing(t) -> TyMaybe(t)
  | VClosure(x,e,gamma) -> TyFn(valor_para_tipo(eval gamma e), valor_para_tipo(eval gamma (ExVar(x)))) (*ERRADO*)
  | VClosureRecursivo(f,x,e,gamma) -> TyFn(valor_para_tipo (eval gamma e), valor_para_tipo (eval gamma (ExVar(x)))) (*ERRADO*)

(*Funcao que transforma um abiente de valores em uma ambiente de tipo*)
let rec ambiente_valor_para_ambiente_tipo (gamma: ambiente_valor) : ambiente_tipo = match gamma with
  | [] -> []
  | (x,v)::t -> (x,valor_para_tipo v)::(ambiente_valor_para_ambiente_tipo t)

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
let interpretador_sem_ambientes (e:expr) : unit =
    try 
        let t = typeinfer [] e in
        let v = eval [] e in
        print_string ((valor_para_string v) ^ " : " ^ (tipo_para_string t))
    with _ ->  print_string ("erro ")

(*Função Principal do Interpretador Com Ambientes*)
let interpretador_com_ambientes (a1:ambiente_valor) (e:expr) : unit =
    let a2 = ambiente_valor_para_ambiente_tipo a1 in (
    try 
        let t = typeinfer a2 e in
        let v = eval a1 e in
        print_string ((valor_para_string v) ^ " : " ^ (tipo_para_string t))
    with _ ->  print_string ("erro "))
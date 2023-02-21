(* Testes de inferencia de tipo: *)
(*Sem ambientes*)
typeinfer [] (ExIf(ExTrue,ExFalse,ExTrue));;
typeinfer [] (ExIf(ExTrue,(ExNum 3),(ExNum 4)));;
typeinfer [] (ExBinop(Soma, ExNum(3), ExNum(4)));;
typeinfer [] (ExBinop(Soma, ExNum(3), ExTrue));; (*Erro pois usa booleano em operacao de inteiros*)
typeinfer [] (ExBinop(Divisao, ExNum(3), ExNum(4)));;
typeinfer [] (ExBinop(Divisao, ExNum(3), ExNum(0)));; (* não da erro, pois a div por zero e verificada no avaliador*)
typeinfer [] (ExBinop(And, ExTrue, ExFalse));;
typeinfer [] (ExBinop(And, ExTrue, ExNum(3)));; (*Erro pois usa inteiro em operacao de booleanos*)
typeinfer [] (ExPair(ExTrue, ExFalse));;
typeinfer [] (ExPair(ExTrue, ExNum(2)));;
typeinfer [] (ExFst(ExPair(ExTrue, ExNum(2))));;
typeinfer [] (ExSnd(ExPair(ExTrue, ExNum(2))));;
(*Com Ambientes*) (*Não esquecer de colocar o valor da variável entre parênteses*)
typeinfer (update [] "x" TyInt) (ExTrue);;
typeinfer (update [] "x" TyInt) (ExVar "x");;
typeinfer (update [] "x" TyBool) (ExIf (ExVar "x", ExTrue, ExFalse));;
typeinfer (update(update(update [] "x" TyInt) "y" TyBool) "z" TyBool) (ExVar "x");;
typeinfer (update(update(update [] "x" TyBool) "y" TyInt) "z" TyInt) (ExIf(ExVar "x", ExVar "y", ExVar "z"));;
typeinfer (update(update(update [] "x" TyBool) "y" TyInt) "z" TyBool) (ExIf(ExVar "x", ExVar "y", ExVar "z"));; (*Erro*)
(*Funções não declaradas*)
typeinfer [] (ExFn("x", TyInt, ExVar("x")));;
typeinfer (update [] "x" TyInt) (ExFn("y", TyInt, ExVar("x")));;
typeinfer (update [] "x" TyBool) (ExFn("y", TyInt, ExVar("x")));;
typeinfer [] (ExFn("x", TyInt, ExTrue));;
typeinfer [] (ExFn("x", TyBool, (ExIf(ExVar("x"),ExFalse,ExTrue))));;
typeinfer [] (ExFn("x", TyBool, ExIf(ExFalse,ExVar("x"),ExTrue)));;
typeinfer (update [] "x" (TyFn(TyInt, TyInt))) (ExFn("y", TyBool, ExVar("x")));;
(*Aplicações*)
typeinfer [] (ExApp(ExFn("x", TyInt, ExVar("x")), ExNum(3)));;
typeinfer [] (ExApp(ExFn("x", TyBool, ExIf(ExVar("x"),ExTrue,ExFalse)), ExTrue));;
typeinfer (update [] "x" TyInt) (ExApp(ExFn("y", TyInt, ExBinop(Multiplicacao, ExVar("x"), ExVar("y"))), ExNum(3)));;
(*Funções declaradas*)
typeinfer [] (ExLet("x", TyInt, ExNum(3), ExNum(4)));;
typeinfer [] (ExLet("x", TyInt, ExNum(3), ExVar("x")));;
typeinfer (update [] "x" TyInt) (ExLet("y", TyInt, ExNum(3), ExBinop(Multiplicacao, ExVar("x"), ExVar("y"))));;


(*Listas*)
typeinfer [] (ExNil(TyList(TyBool)));;
typeinfer [] (ExNil(TyBool));; (*Erro pois Nil tem que receber um tipo lista de um tipo*)
typeinfer [] (ExList(ExTrue, ExNil(TyList(TyBool))));;
typeinfer [] (ExList(ExTrue, ExNil(TyList(TyInt))));; (* Erro pois o tipo da lista de nil é diferente do tipo do head *)
typeinfer [] (ExList(ExTrue, ExList(ExFalse, ExNil(TyList(TyBool)))));;
typeinfer [] (ExList(ExTrue, ExList(ExFalse, ExNil(TyList(TyInt)))));; (* Erro pois o tipo da lista de nil é diferente do tipo do head *)
typeinfer [] (ExList(ExNum(4), ExList(ExNum(3), ExNil(TyList(TyInt)))));; 
typeinfer [] (ExList(ExTrue, ExList(ExNum(3), ExNil(TyList(TyInt)))));;  (* Erro pois a lista não tem consistencia de tipo *)
(*Maybe*)
typeinfer [] (ExJust(ExTrue));;
typeinfer [] (ExJust(ExNil(TyList(TyBool))));;
typeinfer [] (ExNothing(TyBool));;




(* Testes de avaliacao: *)
eval [] (ExNum(3));;
eval [] (ExTrue);;
eval [] (VTrue);; (*Erro pois eval espera uma expressão e não um valor *)
eval [] (ExFalse);;
eval [] (ExVar("x"));; (*Erro pois a variável não foi declarada no ambiente*)
eval (update [] "x" (ExTrue)) (ExVar("x"));; (*Erro pois a variável foi declarada como uma expressão e não um valor*)
eval (update [] "x" (VTrue)) (ExVar("x"));;
eval (update (update [] "x" (VTrue)) "y" (VFalse)) (ExVar("x"));;
eval (update (update [] "x" (VTrue)) "y" (VFalse)) (ExVar("y"));;
eval (update [] "x" (VNum(2))) (ExVar("x"));;
eval [] (ExIf(ExTrue, ExNum(3), ExNum(4)));;
eval (update [] "x" (VNum(3))) (ExIf(ExTrue, ExVar("x"), ExNum(4)));;
eval [] (ExIf(ExTrue, (ExBinop(And, ExTrue, ExFalse)), ExNum(4)));;
eval [] (ExBinop(And, ExTrue, ExFalse));;
eval [] (ExBinop(Soma, ExNum(3), ExNum(4)));;
eval (update [] "x" (VTrue)) (ExBinop(And, ExVar("x"), ExFalse));;
eval [] (ExBinop(Multiplicacao, ExNum(3), ExNum(4)));;
eval (update [] "x" (VNum(5))) (ExBinop(Multiplicacao, ExVar("x"), ExNum(4)));;
eval [] (ExBinop(Divisao, ExNum(3), ExNum(4)));;
eval [] (ExBinop(Divisao, ExNum(3), ExNum(0)));; (*Erro divisisão por zero *)
eval [] (ExPar(ExTrue, ExFalse));;
eval (update(update [] "x" (VTrue)) "y" (VFalse)) (ExPar(ExVar("x"), ExVar("y")));;
(*Funções não declaradas*)
eval [] (ExFn("x", TyInt, ExVar("x")));;
eval (update [] "x" (VNum(3))) (ExFn("y", TyInt, ExVar("x")));;
eval (update [] "x" (VTrue)) (ExFn("y", TyInt, ExVar("x")));;
eval [] (ExFn("x", TyInt, ExTrue));;
eval [] (ExFn("x", TyBool, ExIf(ExVar("x"),ExFalse,ExTrue)));;
eval [] (ExFn("x", TyBool, ExIf(ExFalse,ExVar("x"),ExTrue)));;
(*Aplicações*)
eval [] (ExApp(ExFn("x", TyInt, ExVar("x")), ExNum(3)));;
eval [] (ExApp(ExFn("x", TyBool, ExIf(ExVar("x"),ExTrue,ExFalse)), ExTrue));;
eval (update [] "x" (VNum(4))) (ExApp(ExFn("y", TyInt, ExBinop(Multiplicacao, ExVar("x"), ExVar("y"))), ExNum(3)));;
(*!!!Testar app com função recursiva!!!*)
eval [] (ExLet("x", TyInt, ExNum(3), ExNum(4)));;
eval [] (ExLet("x", TyInt, ExNum(3), ExVar("x")));;
eval (update [] "x" (VNum(10))) (ExLet("y", TyInt, ExNum(2), ExBinop(Multiplicacao, ExVar("x"), ExVar("y"))));;

(* eval [] (ExLetRec())*)

(*Listas*)
eval [] (ExList(ExTrue, ExNil(TyList(TyBool))));;
eval [] (ExList(ExTrue, ExNil(TyList(TyInt))));; (* Não dá erro aqui, poiso erro ocorre no typeinfer. Mas está errado, pois o tipo da lista de nil é diferente do tipo do head *)
eval [] (ExList(ExTrue, ExList(ExFalse, ExNil(TyList(TyBool)))));;
eval [] (ExList(ExNum(4), ExList(ExNum(3), ExList(ExNum(5), ExNil(TyList(TyInt))))));;

(*Testes ambiente_valor_para_ambiente_tipo*)
(update [] "x" (VNum(5)))
ambiente_valor_para_ambiente_tipo (update [] "x" (VNum(5)))
(update(update [] "x" (VTrue)) "y" (VFalse)) (*  [("y", VFalse); ("x", VTrue)]  *)
ambiente_valor_para_ambiente_tipo (update(update [] "x" (VTrue)) "y" (VFalse)) (*  [("y", VFalse); ("x", VTrue)] vira  [("y", TyBool); ("x", TyBool)]*)
(update [] "x" (VPar(VTrue, VFalse)))
ambiente_valor_para_ambiente_tipo (update [] "x" (VPar(VTrue, VFalse)))
ambiente_valor_para_ambiente_tipo (update [] "x" (VList(VTrue, VNil(TyList(TyBool)))))
ambiente_valor_para_ambiente_tipo (update [] "x" (VList(VTrue, VNil(TyList(TyInt)))))

(*Testes interpretador com ambientes*)
interpretador_com_ambientes [] (ExTrue);;
interpretador_com_ambientes [] (ExList(ExTrue, ExNil(TyList(TyInt))));;
interpretador_com_ambientes [] (ExList(ExTrue, ExNil(TyList(TyBool))));;
interpretador_com_ambientes [] (ExFn("x", TyInt, ExVar("x")));;
interpretador_com_ambientes [] (ExApp(ExFn("x", TyInt, ExVar("x")), ExNum(3)));;
interpretador_com_ambientes [] (ExLet("x", ExNum(3), ExVar("x")));; (*Erro pois falta argumentos *)
interpretador_com_ambientes (update [] "x" (VList(VTrue, VNil(TyList(TyInt))))) (ExVar("x"));;
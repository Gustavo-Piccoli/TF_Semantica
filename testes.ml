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

(*Com Ambientes*)
typeinfer (update [] "x" TyInt) ExTrue;;
typeinfer (update [] "x" TyInt) (ExVar "x");;
typeinfer (update [] "x" TyBool) (ExIf (ExVar "x", ExTrue, ExFalse));;
typeinfer (update(update(update [] "x" TyInt) "y" TyBool) "z" TyBool) (ExVar "x")
typeinfer (update(update(update [] "x" TyBool) "y" TyInt) "z" TyInt) (ExIf (ExVar "x", ExVar "y", ExVar "z"))
typeinfer (update(update(update [] "x" TyBool) "y" TyInt) "z" TyBool) (ExIf (ExVar "x", ExVar "y", ExVar "z")) (*Erro*)

(*Listas*)
typeinfer [] (ExNil(TyBool))
typeinfer [] (ExNil(ExTrue)) (*Erro*)
typeinfer [] (ExList(ExTrue, ExNil(TyBool)))
typeinfer [] (ExList(ExTrue, ExNil(TyInt))) (*Erro*)
typeinfer [] (ExList(ExTrue, ExList(ExFalse, ExNil(TyBool))))
typeinfer [] (ExList(ExTrue, ExList(ExFalse, ExNil(TyInt)))) (*Erro*)
typeinfer [] (ExList(ExTrue, ExList(ExNum(2), ExNil(TyInt)))) (*Erro*)
typeinfer [] (ExList(ExNum(1), ExList(ExNum(2), ExNil(TyInt))))
typeinfer [] (ExList(ExNum(1), ExList(ExNum(2), ExNum(4)))) (*Erro*)






(* Testes de avaliacao: *)
eval (ExNume(3))
eval (ExVar("x")) (*Erro*)
eval (ExTrue)
eval (ExIf(ExTrue, ExNum(3), ExNum(4)))
eval (ExBinop(And, ExTrue, ExFalse))
eval (ExIf(ExTrue, (ExBinop(And, ExTrue, ExFalse)), ExNum(4)))

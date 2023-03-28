TESTE 1:
=======
let x:int = 2 in
let foo: int --> int = fn y:int => x + y in
let x: int = 5
in   foo 10

valor 12 do tipo int

interpretador (ExLet("x", TyInt, ExNum(2), ExLet("foo", TyFn(TyInt, TyInt), ExFn("y", TyInt, ExBinop(Soma, ExVar("x"), ExVar("y"))), ExLet("x", TyInt, ExNum(5), ExApp(ExVar("foo"), ExNum(10))))))




TESTE 2: (só difere do anterior na ultima linha)
=======
let x:int = 2 in
let foo: int --> int = fn y:int => x + y in
let x: int = 5
in   foo

valor função    (cujo closure é closure <y, x+y, [(x,2)]> )
tipo int -> int


interpretador (ExLet("x", TyInt, ExNum(2), ExLet("foo", TyFn(TyInt, TyInt), ExFn("y", TyInt, ExBinop(Soma, ExVar("x"), ExVar("y"))), ExLet("x", TyInt, ExNum(5), ExVar("foo")))))




TESTE 3:
========

let rec lookup: (int x int) list -> int -> maybe int =
          fn l: (int x int) list => fn key: int =>
              match l with
                nil => nothing
              | x :: xs => if (fst x) = key
                           then Just (snd x)
                           else (lookup xs key)
in lookup [(1,10),(2,20), (3,30)]  2

valor - Just 20
tipo - maybe int


interpretador (ExLetRec("lookup", 
                        TyFn(TyList(TyPar(TyInt, TyInt)), TyFn(TyInt, TyMaybe(TyInt))), 
                        ExFn("l", 
                             TyList(TyPar(TyInt, TyInt)), 
                             ExFn("key", 
                                  TyInt, 
                                  ExMatchList(ExVar("l"),
                                              ExNothing(TyInt),
                                              "x",
                                              "xs",
                                              ExIf(ExBinop(Igual, ExFst(ExVar("x")), ExVar("key")), 
                                                  ExJust(ExSnd(ExVar("x"))), 
                                                  ExApp(ExApp(ExVar("lookup"), ExVar("xs")), ExVar("key")))))),
                        ExApp(ExApp(ExVar("lookup"), ExList(ExPar(ExNum(1), ExNum(10)), 
                                                            ExList(ExPar(ExNum(2), ExNum(20)), 
                                                                   ExList(ExPar(ExNum(3), ExNum(30)),
                                                                          ExNil(TyList(TyPar(TyInt,TyInt))))))), ExNum(2))))





TESTE 4:
========

let rec map: (int -> int) -> int list -> int list =
       fn f: int->int => fn l: int list =>
            match l with
             nil -> nil: int list
           | x :: xs -> (f x) :: (map f xs)
in
      map (fn x:int => x + x) [10,20,30]

valor [20,40,60]
tipo int list


interpretador (ExLetRec("map", 
         TyFn(TyFn(TyInt, TyInt), TyFn(TyList(TyInt), TyList(TyInt))), 
         ExFn("f", 
              TyFn(TyInt, TyInt), 
              ExFn("l", 
                   TyList(TyInt), 
                   ExMatchList(ExVar("l"), 
                               ExNil(TyList(TyInt)), 
                               "x", 
                               "xs", 
                               ExList(ExApp(ExVar("f"), ExVar("x")), 
                                      ExApp(ExApp(ExVar("map"), ExVar("f")), ExVar("xs")))))),
         ExApp(ExApp(ExVar("map"), ExFn("x", TyInt, ExBinop(Soma, ExVar("x"), ExVar("x")))), ExList(ExNum(10), ExList(ExNum(20), ExList(ExNum(30), ExNil(TyList(TyInt))))))))




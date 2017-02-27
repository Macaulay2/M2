-- A first attempt to write code that will take a specification for boiler-plate d-code, and
-- write it for us.

-- e.g. 

celltype = new HashTable from {
    "Matrix" => hashTable {
        DType => "RawMatrixCell",
        Synonym => "a raw matrix", 
        Suffix => ".p"
        },
    "MatrixOrNull" => hashTable {
        DType => "RawMatrixOrNull",
        Synonym => "a raw matrix or null", 
        Suffix => ".p"
        }
    }

indentStr = "  ";
genTitle = method()
genNumArgs = method()
genArg = method()
genFunctionCall = method()

genTitle(String) := (fcnName) -> (innards) -> (
    "export "|fcnName|"(e:Expr):Expr := (\n"|
      innards|"\n"|
      indentStr|");\n"|
      "setupfun(\""|fcnName|"\","|fcnName|");"
    )
genNumArgs ZZ := (N) -> innards -> (
    indentStr|"when e is s:Sequence do (\n"|
    indentStr|"if length(s) == "|N|" then (\n"|
    innards|"\n"|
    indentStr|") else wrongNumArgs("|N|")\n"|
    indentStr|") else wrongNumArgs("|N|")"
 )

genArg(ZZ,String,String) := (argnum, argtype, argname) -> (innards) -> (
    suffix := celltype#argtype#2;
    indentStr|"when s."|argnum|" is wrapped"|argname|":"|celltype#argtype#0|" do ("|argname|" := wrapped"|argname|suffix|";\n"|
    innards|"\n"|
    indentStr|") else WrongArg("|argnum|",\""|celltype#argtype#1|"\")"
    )


genFunctionCall(String, String, Sequence) := (fcnname, returntype, args) -> (innards) -> (
    innards := "toExpr(Ccode("|celltype#returntype#3, XXX
    str := indentStr | indentStr | innards;
    scan(#args, i -> (
        argnum := #args-1-i;
        argtype := args#argnum#1;
        argname := args#argnum#0;
        str = (genArg(argnum, argtype,argname)) str;
        ));
    (genTitle fcnname) (genNumArgs (#args)) str
    )
end--
restart
load "generateD.m2"
print ((genFunctionCall("rawHomogenizeMatrix", "Matrix", ("a"=>"Matrix", "b"=>"Matrix", "c"=>"Matrix"))) "innards")

(genArg(1, "Matrix", "M")) (genArg(2, "Matrix", "N")) (indentStr | "innards")
(genTitle "rawHomogenizeMatrix") "innards"
(genTitle "rawHomogenizeMatrix") 

  (genFunctionCall("rawHomogenizeMatrix", MatrixOrNull, (a=>Matrix, b=>int, c=>M2arrayint)))

print ((genTitle "rawSaturate") 
print ((genTitle "rawZZ") ((genNumArgs 0) "  toExpr(Ccode(RawRing,IM2_Ring_ZZ()))"))
(genArg(2,"Matrix")) ""

(genTitle "rawSaturate") "innards"



rawSaturate MonomialIdealOrNull (I,MonomialIdeal) (m,Monomial)
rawSaturate MonomialIdealOrNull (I,MonomialIdeal) (J,MonomialIdeal)

(rawSaturate, (I,MonomialIdeal), (m,Monomial)) => MonomialIdealOrNull
rawSaturate MonomialIdealOrNull (I,MonomialIdeal) (J,MonomialIdeal)

when e is s:Sequence do
  if length(s) != N then wrongNumArgs(N) else
  XXX
  else wrongNumArgs(N)


when s.ARGNUM is p:ZZcell do
  if isInt(p) then (
      P := toInt(p);
      sub(innards, ARG=>P)
      )
  else WrongArgSmallInteger(ARGNUM)
  else WrongArgSmallInteger(ARGNUM)

Begin:
when s.ARGNUM is pARGNUM:ZZcell do
  if isInt(pARGNUM) then

Internal:
  sub(ARG with toInt(pARGNUM))

End:
  else WrongArgSmallInteger(ARGNUM)
  else WrongArgSmallInteger(ARGNUM)

-- 
generate("rawQR", MutableMatrix, MutableMatrix, MutableMatrix, Boolean,
    Returns => PossibleError Boolean)
 -- generate D-functions, and a C header.
 



genArg(ZZ,String) := (argnum, argtype) -> (innards) -> (
    argname := "p"|argnum;
    "when s."|argnum|" is "|argname|":"|celltype#argtype#0|" do\n"|
    "else WrongArg("|argnum|",\""|celltype#argtype#1|"\")\n"
    )

-- this next one is not written yet.
genSmallIntArg = method()
genSmallIntArg ZZ := (argnum) -> (innards) -> (
    argname := "p"|argnum;
    "if toInt(s.argnum ) then\n"|innards|"\n"|
    "else"
    )

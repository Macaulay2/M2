-- A first attempt to write code that will take a specification for boiler-plate d-code, and
-- write it for us.

-- e.g. 

celltype = new HashTable from {
    "Monomial" => hashTable {
        DType => "RawMonomialCell",
        Synonym => "a raw monomial", 
        Suffix => ".p"
        },
    "MonomialOrNull" => hashTable {
        DType => "RawMonomialOrNull",
        Synonym => "a raw monomial or null", 
        Suffix => ".p"
        },
    "Ring" => hashTable {
        DType => "RawRingCell",
        Synonym => "a raw ring", 
        Suffix => ".p"
        },
    "RingOrNull" => hashTable {
        DType => "RawRingOrNull",
        Synonym => "a raw ring or null", 
        Suffix => ".p"
        },
    "RingElement" => hashTable {
        DType => "RawRingElementCell",
        Synonym => "a raw ring element", 
        Suffix => ".p"
        },
    "RingElementOrNull" => hashTable {
        DType => "RawRingElementOrNull",
        Synonym => "a raw ring element", 
        Suffix => ".p"
        },
    "Matrix" => hashTable {
        DType => "RawMatrixCell",
        Synonym => "a raw matrix", 
        Suffix => ".p"
        },
    "MatrixOrNull" => hashTable {
        DType => "RawMatrixOrNull",
        Synonym => "a raw matrix or null", 
        Prefix => "&",
        Suffix => ""
        }
    }

indentStr = "  ";
genTitle = method()
genNumArgs = method()
genOneArg = method()
genArg = method()
genFunctionCall = method()

indent = method()
indent String := (str) -> indentStr | str
indent List := (L) -> L/indent
indent(ZZ, String) := (n,str) -> concatenate(n:indentStr) | str
indent(ZZ, List) := (n,L) -> L/(str -> indent(n,str))

str = method()
str String := (str) -> str | "\n"
str List := (L) -> concatenate(L/str)

genTitle(String) := (fcnName) -> (innards) ->
    {
        "export "|fcnName|"(e:Expr):Expr := (",
        indent innards,
        indent ");",
        "setupfun(\""|fcnName|"\","|fcnName|");"
    }
genNumArgs ZZ := (N) -> innards ->
    {
      "when e is s:Sequence do (",
      "if length(s) == "|N|" then (",
      innards,
      ") else WrongNumArgs("|N|")",
      ") else WrongNumArgs("|N|")"
    }

genNumArgs ZZ := (N) -> innards -> (
    if N === 1 then (
        {
            "when e"
        }
        )
    else {
      "when e is s:Sequence do (",
      "if length(s) == "|N|" then (",
      innards,
      ") else WrongNumArgs("|N|")",
      ") else WrongNumArgs("|N|")"
    })

genOneArg(String,String) := (argtype, argname) -> (innards) -> (
    suffix := celltype#argtype#Suffix;
    {
      "when e is wrapped"|argname|":"|celltype#argtype#DType|" do ("|argname|" := wrapped"|argname|suffix|";",
      innards,
      ///) else WrongArg("///|celltype#argtype#Synonym|///")///
      }
  )

genArg(ZZ,String,String) := (argnum, argtype, argname) -> (innards) -> (
    suffix := celltype#argtype#Suffix;
    {
      "when s."|argnum|" is wrapped"|argname|":"|celltype#argtype#DType|" do ("|argname|" := wrapped"|argname|suffix|";",
      innards,
      ") else WrongArg("|argnum|",\""|celltype#argtype#Synonym|"\")"
      }
  )
        
genFunctionCall(String, String, Thing) := (fcnname, returntype, args) -> (
    if not instance(args,Sequence) then args = 1:args;
    argnames := (toList args)/(x -> x#0);
    cargs := (concatenate between(", \",\", ", argnames)) | ", ";
    innards := {
        "toExpr(Ccode("|celltype#returntype#DType|",",
        indent ///"///|fcnname|///(",///,
        indent indent cargs,
        indent ///")"///,
        "))"
        };
    L := indent innards;
    if #args === 1 then 
       (genTitle fcnname) (genOneArg (args#0#1, args#0#0)) L
    else if #args > 1 then (
      scan(#args, i -> (
              argnum := #args-1-i;
              argtype := args#argnum#1;
              argname := args#argnum#0;
              L = (genArg(argnum, argtype,argname)) L;
              ));
      (genTitle fcnname) (genNumArgs (#args)) L
      )
    )

parseTemplate = method()
parseTemplate List := (strs) -> (
    -- 
    netList strs
    )
readTemplateFile = method()
readTemplateFile String := (filename) -> (
    -- return a hash table of all function templates found
    L := lines get filename;
    L = select(L, s -> #s > 0);
    pos := positions(L, s -> s#0 != " ");
    pos = append(pos, #L);
    hashTable for i from 0 to #pos-2 list L_(pos#i) => parseTemplate L_{pos#i+1..pos#(i+1)-1}
    )

{*
debug needsPackage "SimpleDoc"

SplitByNameFcns = new HashTable from {
    "Node" => (textlines, keylinenum) -> << textlines << " " << keylinenum << endl;
    }

errorDepth=0
toDoc(SplitByNameFcns, get "templates.txt")
*}

generateType = method(Option=>{Dryrun => true})
generateType String := opts -> (typename) -> (
    -- This function, depending on optional argument, will either
    -- display what it would do, or actually write the files.
    -- actors4.d
    -- basic.d
    -- classes.dd
    -- engine.dd
    -- equality.dd
    -- expr.dd
    -- parse.dd
    -- util.dd
    -- engine.h (need to include x-typename.hpp?)
    -- x-typename.hpp
    -- x-typename.cpp
    -- Makefile.files
    )

--removeType String

end--
restart
load "generateD.m2"
readTemplateFile "templates.txt"


(genTitle "foo") "innards"
str oo

str (genFunctionCall(
        "rawHomogenizeMatrix", 
        "MatrixOrNull", 
        ("a"=>"Matrix", "b"=>"Matrix", "c"=>"Matrix")
        ))
str (genFunctionCall(
        "rawRingElementAntipode",
        "RingElementOrNull", 
        "a"=>"RingElement"
        ))
str (genFunctionCall(
        "rawTerm",
        "RingElementOrNull",
        ("a"=>"Ring", "b"=>"RingElement", "c" => "Monomial")
        ))

genFunctionCall hashTable {
    Name => "rawHomogenizeMatrix",
    ReturnValue => "MatrixOrNull",
    Arguments => list of arguments ("M" => "Matrix")
    Doc => String, -- or list of strings
    Code => String -- or list of strings
    }
-- creates 3 strings:
-- (1) d function
-- (2) engine.h extern line
-- (3) x-files interface c++ function.

rawHomogenizeMatrix
  returns MatrixOrNull
  arg a Matrix
  arg b Matrix
  arg c Matrix
  doc Homogenize a matrix

  
str oo
print (genFunctionCall("rawHomogenizeMatrix", "MatrixOrNull", ("a"=>"Matrix", "b"=>"Matrix", "c"=>"Matrix")))

    toExpr(Ccode(RawMatrixOrNull,
      "rawHomogenizeMatrix(",
        a,",",
        b,","
        c,")"
    ))


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

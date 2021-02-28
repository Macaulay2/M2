newPackage(
        "GenerateD",
        Version => "0.1", 
        Date => "3 Oct 2017",
        Authors => {
            {Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://www.math.cornell.edu/~mike"}
            },
        Headline => "helper code for writing boiler-plate code for new engine routines and types"
        )

export {
    "indent",
    "genTitle",
    "genNumArgs",
    "genOneArg",
    "genArg",
    "genFunctionCall",
    "PossibleEngineError"
    }

protect DType
protect Suffix
protect Synonym

celltype = new HashTable from {
    "bool" => hashTable {
        DType => "bool",
        Synonym => "a small integer", 
        Suffix => ""
        },
    "int" => hashTable {
        DType => "ZZCell",
        Synonym => "a small integer", 
        Suffix => "",
        Array => (
            "isInt",
            "toInt",
            "a small integer"
            )
        },
    "long" => hashTable {
        DType => "ZZCell",
        Synonym => "a small integer", 
        Suffix => ""
        },
    "ulong" => hashTable {
        DType => "ZZCell",
        Synonym => "a small long integer", 
        Suffix => ""
        },
    "ComputationOrNull" => hashTable {
        DType => "RawComputationCell",
        Synonym => "an engine computation",
        Suffix => ".p"
        },
    "RingElementArrayOrNull" => hashTable {
        Synonym => "a sequence of raw ring elements",
        Suffix => ""
        },
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
    "Monoid" => hashTable {
        DType => "RawMonoidCell",
        Synonym => "a raw monoid", 
        Suffix => ".p"
        },
    "MonoidOrNull" => hashTable {
        DType => "RawMonoidOrNull",
        Synonym => "a raw monoid or null", 
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
        },
    "MutableMatrix" => hashTable {
        DType => "RawMutableMatrixCell",
        Synonym => "a raw mutable matrix", 
        Suffix => ".p"
        },
    "MutableComplex" => hashTable {
        DType => "RawMutableComplexCell",
        Synonym => "a raw mutable complex", 
        Suffix => ".p"
        },
    "MutableComplexOrNull" => hashTable {
        DType => "RawMutableComplexOrNull",
        Synonym => "a raw mutable complex", 
        Suffix => ".p"
        }
    
    }

indentStr = "  ";
genTitle = method()
genNumArgs = method()
genOneArg = method()
genArg = method()
genFunctionCall = method(Options=>{PossibleEngineError=>false})

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
      "when e is w"|argname|":"|celltype#argtype#DType|" do ("|argname|" := w"|argname|suffix|";",
      innards,
      ///) else WrongArg("///|celltype#argtype#Synonym|///")///
      }
  )

genArg(ZZ,String,String) := (argnum, argtype, argname) -> (innards) -> (
    suffix := celltype#argtype#Suffix;
    {
      "when s."|argnum|" is w"|argname|":"|celltype#argtype#DType|" do ("|argname|" := w"|argname|suffix|";",
      innards,
      ") else WrongArg("|argnum|",\""|celltype#argtype#Synonym|"\")"
      }
  )

genArg(ZZ,String,String) := (argnum, argtype, argname) -> (innards) -> (
-*    
    if celltype#argtype#?Array then (
        {
            (f1, f2, err) := celltype#argtype#Array;
            "when s."|argnum|" is w"|argname|":ZZcell do (", 
            "if "|f1|"(w"|argname|") then ("|argname|" := "|f2|"(w"|argname|");",
            innards,
            ") else WrongArg("|argnum|",\""|err|")",
            ") else WrongArgZZ("|argnum|")"
            }
        )
*-
    if argtype == "int" then (
        {
            "when s."|argnum|" is w"|argname|":ZZcell do (", 
            "if isInt(w"|argname|") then ("|argname|" := toInt(w"|argname|");",
            innards,
            ") else WrongArgSmallInteger("|argnum|")",
            ") else WrongArgZZ("|argnum|")"
            }
        )
    else if argtype == "ulong" then (
        {
            "when s."|argnum|" is w"|argname|":ZZcell do (", 
            "if isULong(w"|argname|") then ("|argname|" := toULong(w"|argname|");",
            innards,
            ") else WrongArgSmallInteger("|argnum|")",
            ") else WrongArgZZ("|argnum|")"
            }
        )
    else (
        suffix := celltype#argtype#Suffix;
        {
            "when s."|argnum|" is w"|argname|":"|celltype#argtype#DType|" do ("|argname|" := w"|argname|suffix|";",
            innards,
            ") else WrongArg("|argnum|",\""|celltype#argtype#Synonym|"\")"
            }
        )
  )

genFunctionCall(String, String, Thing) := opts -> (fcnname, returntype, args) -> (
    toexpr := if opts.PossibleEngineError then "possibleEngineError" else "toExpr";
    if not instance(args,Sequence) then args = 1:args;
    argnames := (toList args)/(x -> x#0);
    cargs := (concatenate between(", \",\", ", argnames)) | ", ";
    innards := {
        toexpr|"(Ccode("|celltype#returntype#DType|",",
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

beginDocumentation()

ans1 = ///export rawHomogenizeMatrix(e:Expr):Expr := (
  when e is s:Sequence do (
  if length(s) == 3 then (
  when s.0 is wa:RawMatrixCell do (a := wa.p;
  when s.1 is wb:RawMatrixCell do (b := wb.p;
  when s.2 is wc:RawMatrixCell do (c := wc.p;
    toExpr(Ccode(RawMatrixOrNull,
      "rawHomogenizeMatrix(",
        a, ",", b, ",", c, 
      ")"
    ))
  ) else WrongArg(2,"a raw matrix")
  ) else WrongArg(1,"a raw matrix")
  ) else WrongArg(0,"a raw matrix")
  ) else WrongNumArgs(3)
  ) else WrongNumArgs(3)
  );
setupfun("rawHomogenizeMatrix",rawHomogenizeMatrix);
///

ans2 = ///export rawMatrixRowSwap(e:Expr):Expr := (
  when e is s:Sequence do (
  if length(s) == 3 then (
  when s.0 is wa:RawMutableMatrixCell do (a := wa.p;
  when s.1 is wb:ZZcell do (
  if isInt(wb) then (b := toInt(wb);
  when s.2 is wc:ZZcell do (
  if isInt(wc) then (c := toInt(wc);
    possibleEngineError(Ccode(bool,
      "rawMatrixRowSwap(",
        a, ",", b, ",", c, 
      ")"
    ))
  ) else WrongArgSmallInteger(2)
  ) else WrongArgZZ(2)
  ) else WrongArgSmallInteger(1)
  ) else WrongArgZZ(1)
  ) else WrongArg(0,"a raw mutable matrix")
  ) else WrongNumArgs(3)
  ) else WrongNumArgs(3)
  );
setupfun("rawMatrixRowSwap",rawMatrixRowSwap);
///

TEST ///
debug needsPackage "GenerateD"
result = str (genFunctionCall(
        "rawHomogenizeMatrix", 
        "MatrixOrNull", 
        ("a"=>"Matrix", "b"=>"Matrix", "c"=>"Matrix")
        ))
assert(result == ans1)
///

TEST ///
--restart
debug needsPackage "GenerateD"
result = str (genFunctionCall(
        "rawPruneComplex", 
        "MutableComplexOrNull", 
        toSequence{"C"=>"MutableComplex","nsteps"=>"int","flags"=>"int"}
        ));
print result
///

TEST ///
debug needsPackage "GenerateD"
result = str (genFunctionCall(
        "rawMatrixRowSwap", 
        "bool",
        ("a"=>"MutableMatrix", "b"=>"int", "c"=>"int"),
        PossibleEngineError => true
        ))
assert(result == ans2)
///

ans3 = ///export rawARingZZpFlint(e:Expr):Expr := (
  when e is wa:ZZCell do (a := wa;
    toExpr(Ccode(RawRingOrNull,
      "rawARingZZpFlint(",
        a, 
      ")"
    ))
  ) else WrongArg("a small long integer")
  );
setupfun("rawARingZZpFlint",rawARingZZpFlint);
///

TEST ///
debug needsPackage "GenerateD"
result = str (genFunctionCall(
        "rawARingZZpFlint", 
        "RingOrNull",
        toSequence {"a"=>"ulong"}
        ))
assert(result == ans3)
///

TEST ///
debug needsPackage "GenerateD"
fcns = {
    (
        "rawARingZZpFlint", 
        "RingOrNull",
        toSequence {"a"=>"ulong"}
        ),
    (
        "rawCharacteristic",
        "long",
        toSequence{"a"=>"Ring"}
        ),
    (
        "rawPolynomialRing",
        "RingOrNull",
        ("K"=>"Ring", "M"=>"Monoid")
        ),
    (
        "rawIsDense",
        "bool",
        toSequence{"a" => "Matrix"}
        )
    }        
str between("",for args in fcns list genFunctionCall args)
///

TEST ///
restart
  debug needsPackage "GenerateD"
  result = str (genFunctionCall(
        "rawGBForce", 
        "ComputationOrNull", 
        ("gens"=>"Matrix", "gb"=>"Matrix", "change"=>"Matrix","syz"=>"Matrix","flags"=>"int")
        ))
  print result
///

TEST ///
  debug needsPackage "GenerateD"
  result = str (genFunctionCall(
        "rawNCGroebnerBasisTwoSided", 
        "MatrixOrNull", 
        ("a"=>"Matrix", "b"=>"int")
        ))

  result2 = str (genFunctionCall(
        "rawNCReductionTwoSided", 
        "MatrixOrNull", 
        ("a"=>"Matrix", "b"=>"Matrix")
        ))

  result = str (genFunctionCall(
        "rawRingM2FreeAlgebraQuotient", 
        "RingOrNull", 
        ("a"=>"Matrix", "b"=>"int")
        ))

  print result
  print result2
///

end--

restart
loadPackage "GenerateD"
check oo

///
name
  rawARingZZpFlint
returns
  RingOrNull
args
  p:ulong
doc
  returns a basic coefficient ring of characteristic p,
  where $p$ is a prime number that fits in 63 bits.
///

        "rawCharacteristic",
        "long",
        toSequence{"a"=>"Ring"}
        ),
    (
        "rawPolynomialRing",
        "RingOrNull",
        ("K"=>"Ring", "M"=>"Monoid")
        )

///

doc ///
Key
  GenerateD
Headline
Description
  Text
  Example
Caveat
SeeAlso
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end--
-- A first attempt to write code that will take a specification for boiler-plate d-code, and
-- write it for us.

-- e.g. 



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

-*
debug needsPackage "SimpleDoc"

SplitByNameFcns = new HashTable from {
    "Node" => (textlines, keylinenum) -> << textlines << " " << keylinenum << endl;
    }

errorDepth=0
toDoc(SplitByNameFcns, get "templates.txt")
*-

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
debug needsPackage "GenerateD"
readTemplateFile "templates.txt"


(genTitle "foo") "innards"
str oo



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

print str (genFunctionCall(
        "rawPruneComplex", 
        "MatrixOrNull",
        ("a"=>"MutableComplex", "b"=>"int")
        ))

str (genFunctionCall(
        "rawMatrixRowSwap", 
        "bool",
        ("a"=>"MutableMatrix", "b"=>"int", "c"=>"int"),
        PossibleEngineError => true
        ))

genFunctionCall hashTable {
    Name => "rawHomogenizeMatrix",
    ReturnValue => "MatrixOrNull",
    Arguments => {"M" => "Matrix"},
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
(genFunctionCall("rawHomogenizeMatrix", "MatrixOrNull", ("a"=>"Matrix", "b"=>"Matrix", "c"=>"Matrix")))

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


rawSaturate MonomialIdealOrNull (I,MonomialIdeal) (m,Monomial)
rawSaturate MonomialIdealOrNull (I,MonomialIdeal) (J,MonomialIdeal)

(rawSaturate, (I,MonomialIdeal), (m,Monomial)) => MonomialIdealOrNull
rawSaturate MonomialIdealOrNull (I,MonomialIdeal) (J,MonomialIdeal)

-- 
generate("rawQR", MutableMatrix, MutableMatrix, MutableMatrix, Boolean,
    Returns => PossibleError Boolean)
 -- generate D-functions, and a C header.
 
generate("rawNCGroebnerTwoSided", Matrix, ,
    Returns => PossibleError Boolean)

-* Moved from e-includes/doc-generateD on 4/11/2020
Want to automate the process of writing d functions.

For example, want to have, e.g.

(rawHomogenizeMatrix, Matrix, int, intList), MatrixOrNull

This would generate the following 3 pieces of code:

export rawSaturate(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is x:RawMonomialCell do
     when s.1 is y:RawMonomialCell do toExpr(rawSaturateMonomial(x.p,y.p))
     else WrongArg(2,"a raw monomial")
     else when s.0 is I:RawMonomialIdealCell do 
     when s.1
     is y:RawMonomialCell do toExpr( Ccode(RawMonomialIdealOrNull, "rawSaturateMonomialIdeal1(", I.p, ",", y.p, ")" ))
     is J:RawMonomialIdealCell do toExpr( Ccode(RawMonomialIdealOrNull, "rawSaturateMonomialIdeal2(", I.p, ",", J.p, ")" ))
     else WrongArg(2,"a raw monomial or monomial ideal")
     else WrongArg(1,"a raw monomial or monomial ideal")
     else WrongNumArgs(2));
setupfun("rawSaturate",rawSaturate);

rawHomogenizeMatrix MatrixOrNull M:Matrix varindex:int varweights:M2_arrayint

    if isSmallInt(s.1) then (arg1 := getSmallInt(s.1);
        innards
    ) else

// Piece of code #1
export rawHomogenizeMatrix(e:Expr):Expr := (
    when e is s:Sequence do (
    if length(s) == 3 then (
    when s.0 is wM:RawMatrixCell do (M := wM.p;
    if isSmallInt(s.1) then (varindex := getSmallInt(s.1);
    if isSequenceOfSmallIntegers(s.2) then (varweights := getSequenceOfSmallIntegers(s.2);
         toExpr(Ccode(RawMatrixOrNull,
                 "IM2_Matrix_homogenize(",
                 M, ",",
                 varindex, ",",
                 weights,
                 ")"
                 ))
    ) else WrongArg(3, "a sequence of small integers")
    ) else WrongArgSmallInteger(2)
    ) else WrongArg(1, "a raw matrix")
    ) else WrongNumArgs(3)
    ) else WrongNumArgs(3)
)
setupfun("rawHomogenizeMatrix",rawHomogenizeMatrix);

// Piece of code #2 (into engine.h, or similar)
const Matrix* /* or null */ rawHomogenizeMatrix(const Matrix* M, int varindex, M2_arrayint varweights);

// Piece of code #3
const Matrix* /* or null */ rawHomogenizeMatrix(const Matrix* name1, int name2, M2_arrayint name3)
{
    try {
        /* place code here */
        }
    catch (exc::engine_error e) {
        ERROR(e.what());
        return nullptr;
        }
}

// code to place into the placeholder:
return name1->homogenize(name2, name3);

// one option:
rawHomogenizeMatrix 
  ReturnValue    MatrixOrNull 
  Arguments
    M          Matrix 
    varindex   int 
    varweights M2_arrayint
  Doc
    Return the homogenization of M with respect to the variable with index 'varindex'
    where we assume that 'varweights' contains the weights of each variable.
  Code
    return name1->homogenize(name2, name3);

// another option from e-includes/templates.txt
Node
  Name
    rawHomogenizeMatrix 
  ReturnValue
    MatrixOrNull 
  Arguments
    M:Matrix 
    varindex:int 
    varweights:M2_arrayint
  Doc
    Return the homogenization of M with respect to the variable with index 'varindex'
    where we assume that 'varweights' contains the weights of each variable.
  Code
    return name1->homogenize(name2, name3);
*-


-- -*- coding: utf-8 -*-
newPackage(
        "MergeTeX",
        Version => "0.6",
        Date => "April 15, 2022", -- "March 16, 2022",
        Authors => {{Name => "Paul Zinn-Justin",
                  Email => "pzinn@unimelb.edu.au",
                  HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}},
        Headline => "Run Macaulay2 code inside a LaTeX file",
	Keywords => {"System"},
        DebuggingMode => false,
	AuxiliaryFiles => true
        )

export {"mergeTeX", "mergeTeXFile", "Path", "TeX"}

debug Core

codeBegin = "\\begin{lstlisting}"; -- the environment used in the TeX file for M2 code
codeEnd = "\\end{lstlisting}";

inputCmd = "\\lstinputlisting"; -- same thing but inputs the code from an external file

inputLanguage = "Macaulay2"; -- option [language=Macaulay2] to the commands above
outputLanguage = "Macaulay2output"; -- really just an alias to avoid rerunning run code

escapeChar = "`"; -- code to tell listings package to go out of verbatim mode. may need to use more obscure character

codeComment := "-* start code *- "; -- added at the start of every code chunk to split correctly. internal use only

outputCmd = "\\macoutput"; -- for direct access to output

--fmt = x -> replace("\n","@\n",x);

local outputs;

mergeTeXFile = (fn,outfn) -> (
    r := regex("/[^/]*$",fn);
    s := get fn;
    outfn << mergeTeX(get fn, Path => if r === null then null else substring(fn,0,r#0#0+1)) << close
    )

-* ex of use
mergeTeXFile("ex.tex","ex-parsed.tex")
*-

mergeTeX = { Path => null } >> o -> s -> (
    langRegex := "(\\[[^\\]]*language=)"|inputLanguage|"(\\]|,[\\s\\S]*?\\])";
    codeRegex := "(?:" | regexQuote codeBegin | langRegex | "\n([\\s\\S]*?)"|regexQuote codeEnd
    | "|" | regexQuote inputCmd | langRegex | "\\{(.*?)\\}" | ")"; -- phew
    codes := select(codeRegex,s);
    rest := separate(codeRegex,s); -- seems silly to do the regex twice
    --print(fmt\codes,fmt\rest);
    if o.Path =!= null then (oldPath:=path; path = append(path,o.Path));
    codes = apply(codes, x -> (
	    r := regex(codeRegex,x); -- ... and once more ...
	    if r#1#1 != 0 then (
		substring(r#1,x) | outputLanguage | substring(r#2,x) | "\n", -- option
		substring(r#3,x) -- code
		) else (
		substring(r#4,x) | outputLanguage | substring(r#5,x) | "\n",
		try get substring(r#6,x) else ""
	       )
	   ));
    outputs = new MutableHashTable;
    saveMode := topLevelMode; -- not thread-safe
    topLevelMode = TeX;
    s = capture apply(codes, x -> codeComment | x#1);
    topLevelMode = saveMode;
    if o.Path =!= null then path = oldPath;
    if s#0 then print ("warning: running the code produced an error"|s#1);
    --print (fmt s#1);
    --print peek outputs;
    s = last s;
    if last s == "\n" then s=substring(s,0,#s-1);
    s = separate(regexQuote codeComment,s);
    prev := s#0;
    s = for i from 1 to #s-1 list (
	codeBegin | codes#(i-1)#0 | prev | (
	    l := regex("(\n).*(\n).*\\'",s#i);
	    (a,b) := if l===null then (0,0) else (l#1#0,l#2#0);
	    prev = substring(s#i,a+1,b-a-1);
	    --print ("prev=",fmt prev);
	    substring(s#i,0,a)
	    ) | "\n" | codeEnd
	);
    --print (fmt\s);
    if #rest != #s + 1 then print "warning: code/noncode mismatch";
    s = concatenate mingle(rest,s);
    -- final step: replace outputCmd with actual output
    outputRegex := "(?<!%)" | regexQuote outputCmd | "\\{\\d+\\}";
    codes = select(outputRegex,s);
    rest = separate(outputRegex,s); -- seems silly to do the regex twice
    concatenate mingle(rest, apply(codes, x -> (
		i := value substring(x,#outputCmd+1,#x-#outputCmd-2);
		(if outputs#?i then outputs#i else "") | "%" | x | "\n" )))
    )

-* ex of use (assuming ex.tex is in the current directory)
"ex-parsed.tex" << mergeTeX get "ex.tex" << close
*-

-----------------
lastprompt:=""; -- borrowed from startup.m2.in

ZZ#{TeX,InputPrompt} = lineno -> concatenate(
    escapeChar,
    "\\underline{\\tt ",
    lastprompt = concatenate(interpreterDepth:"i",toString lineno),
    "}",
    escapeChar,
    " : "
    )

ZZ#{TeX,InputContinuationPrompt} = lineno -> #lastprompt+3

Nothing#{TeX,Print} = identity

printFunc := x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    << escapeChar | y | escapeChar << endl;
    y
    )

Thing#{TeX,print} = x -> (printFunc x;)

on := () -> concatenate(escapeChar,"\\underline{\\tt ",interpreterDepth:"o", toString lineNumber,"}",escapeChar)

Thing#{TeX,Print} = x -> (
    << on() | " = ";
    outputs#lineNumber = printFunc x; -- print and store output
    )

InexactNumber#{TeX,Print} = x ->  withFullPrecision ( () -> Thing#{TeX,Print} x )

netPrintFunc := x -> (
    y := tex x; -- we compute the tex now (in case it produces an error)
    if class y =!= String then error "invalid TeX output";
    << escapeChar | "\\ttfamily " | y | escapeChar << endl;
    y
    )

Net#{TeX,print} = x -> (netPrintFunc x;)

Net#{TeX,Print} = x -> (
    << on() | " = ";
    outputs#lineNumber = netPrintFunc x; -- print and store output
    )

-- afterprint

texAfterPrint :=  x -> (
    if class x === Sequence then x = RowExpression deepSplice { x };
    << on() | " : ";
    printFunc x;
    )

Thing#{TeX,AfterNoPrint} = identity

-- all that's below would go if afterprint was expressionified

Thing#{TeX,AfterPrint} = x -> texAfterPrint class x;

Boolean#{TeX,AfterPrint} = identity

Expression#{TeX,AfterPrint} = x -> texAfterPrint (Expression," of class ",class x)

Describe#{TeX,AfterPrint} = identity

Ideal#{TeX,AfterPrint} = Ideal#{TeX,AfterNoPrint} = (I) -> texAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{TeX,AfterPrint} = MonomialIdeal#{TeX,AfterNoPrint} = (I) -> texAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{TeX,AfterPrint} = x -> texAfterPrint (class x," (of precision ",precision x,")")

Module#{TeX,AfterPrint} = M -> texAfterPrint(
    ring M,"-module",
    if M.?generators then
    if M.?relations then (", subquotient of ",ambient M)
    else (", submodule of ",ambient M)
    else if M.?relations then (", quotient of ",ambient M)
    else if rank ambient M > 0 then
    (", free",
	if not all(degrees M, d -> all(d, zero))
	then (", degrees ",runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M)
	)
    )

Net#{TeX,AfterPrint} = identity

Nothing#{TeX,AfterPrint} = identity

Matrix#{TeX,AfterPrint} = Matrix#{TeX,AfterNoPrint} =
RingMap#{TeX,AfterPrint} = RingMap#{TeX,AfterNoPrint} = f -> texAfterPrint (class f, " ", new MapExpression from {target f,source f})

-- Sequence#{TeX,AfterPrint} = Sequence#{TeX,AfterNoPrint} = identity

CoherentSheaf#{TeX,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     texAfterPrint("coherent sheaf on ",X,
     if M.?generators then
     if M.?relations then (", subquotient of ", ambient F)
     else (", subsheaf of ", ambient F)
     else if M.?relations then (", quotient of ", ambient F)
     else if n > 0 then (
	  ", free"
	  -- if not all(degrees M, d -> all(d, zero))
	  -- then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  )
     )
 )

ZZ#{TeX,AfterPrint} = identity

beginDocumentation()
multidoc ///
 Node
  Key
   MergeTeX
   mergeTeX
   [mergeTeX,Path]
   Path
   mergeTeXFile
  Headline
   Run Macaulay2 code inside a LaTeX file
  Description
   Text
    This packages parses a LaTeX file, looking for Macaulay2 code. The latter must follow the syntax of the LaTeX package "listings", namely,
    if must be enclosed in @TT "\\begin{lstlisting}[language=Macaulay2]...\\end{lstlisting}"@, or must be taken from an external M2 file via
    @TT "\\begin{lstinputlisting}[language=Macaulay2]{filename.m2}"@;
    one can also insert inline code with @TT "\\lstinline[language=Macaulay2]!...!"@, but it is not parsed.
    See the example file @TT "ex.tex"@.

    It then produces a new file where the output of the code has been inserted in place of the input.
    The LaTeX file should contain @TT "\\usepackage{listings}"@ and should include a definition of the Macaulay2 language;
    this can be found in the auxiliary file @TT "lst-Macaulay2.tex"@.

    It also replaces automatically commands of the form @TT "\\macoutput{n}"@ with the nth output of Macaulay2.

    @TT "mergeTeX"@ takes as input the TeX source as a string and returns the output as a string,
    whereas @TT "mergeTeXFile"@ takes as arguments the names of the source LaTeX file and of the target LaTeX file (the two can be identical;
    @TT "mergeTeXFile"@ acts idempotently).
///

-- Paul Zinn-Justin 2018-2020

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags := apply((17,18,19,20,28,29,30,(18,36),(36,17)),ascii);
    (webAppEndTag,            -- closing tag ~ </span> or </p>
	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppUrlTag,         -- used internally to follow URLs
	webAppTextTag,        -- other text ~ <span class='M2Text'>
	webAppTexTag,         -- effectively deprecated, ~ <span class='M2Html'> $
	webAppTexEndTag       -- effectively deprecated, ~ $ </span>
	)=webAppTags;

webAppTagsRegex := concatenate("[",drop(webAppTags,-2),"]")

-- output routines for WebApp mode

ZZ#{WebApp,InputPrompt} = lineno -> concatenate(
    webAppEndTag, -- close previous cell
    webAppCellTag,
    interpreterDepth:"i",
    toString lineno,
    " : ",
    webAppInputTag)

ZZ#{WebApp,InputContinuationPrompt} = lineno -> webAppInputContdTag

Thing#{WebApp,BeforePrint} = identity

Nothing#{WebApp,Print} = identity

Thing#{WebApp,Print} = x -> (
    oprompt := concatenate(interpreterDepth:"o", toString lineNumber, " = ");
    y := html x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << endl << oprompt | webAppHtmlTag | y | webAppEndTag << endl;
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

on := () -> concatenate(interpreterDepth:"o", toString lineNumber)

htmlAfterPrint :=  y -> (
    y=deepSplice sequence y;
    z := html \ y;
    if any(z, x -> class x =!= String) then error "invalid html output";
    << endl << on() | " : " | webAppHtmlTag | concatenate z | webAppEndTag << endl;
    )

Thing#{WebApp,AfterPrint} = x -> htmlAfterPrint class x;

Boolean#{WebApp,AfterPrint} = identity

Expression#{WebApp,AfterPrint} = x -> htmlAfterPrint (Expression," of class ",class x)

Describe#{WebApp,AfterPrint} = identity

Ideal#{WebApp,AfterPrint} = Ideal#{WebApp,AfterNoPrint} = (I) -> htmlAfterPrint (Ideal," of ",ring I)
MonomialIdeal#{WebApp,AfterPrint} = MonomialIdeal#{WebApp,AfterNoPrint} = (I) -> htmlAfterPrint (MonomialIdeal," of ",ring I)

InexactNumber#{WebApp,AfterPrint} = x -> htmlAfterPrint (class x," (of precision ",precision x,")")

Module#{WebApp,AfterPrint} = M -> htmlAfterPrint(
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

Matrix#{WebApp,AfterPrint} = Matrix#{WebApp,AfterNoPrint} = f -> htmlAfterPrint (Matrix, if isFreeModule target f and isFreeModule source f then (" ", new MapExpression from {target f,source f}))

Net#{WebApp,AfterPrint} = identity

Nothing#{WebApp,AfterPrint} = identity

RingMap#{WebApp,AfterPrint} = RingMap#{WebApp,AfterNoPrint} = f -> htmlAfterPrint (class f," ",new MapExpression from {target f,source f})

Sequence#{WebApp,AfterPrint} = Sequence#{WebApp,AfterNoPrint} = identity

CoherentSheaf#{WebApp,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     n := rank ambient F;
     htmlAfterPrint("coherent sheaf on ",X,
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

ZZ#{WebApp,AfterPrint} = identity

if topLevelMode === WebApp then (
    -- the help hack: if started in WebApp mode, help is compiled in it as well
    webAppPRE := new MarkUpType of PRE; webAppPRE.qname="pre";
    html webAppPRE := x -> concatenate( -- we really mean this: the browser will interpret it as pure text so no need to htmlLiteral it
	"<pre>",
	webAppTextTag,
	apply(x,y->replace("\\$\\{prefix\\}","usr",y)),
	"\n",
	webAppEndTag,
	"</pre>\n"
	);
    pELBackup:=lookup(processExamplesLoop,ExampleItem);
    processExamplesLoop ExampleItem := x -> (
	res := pELBackup x;
	new webAppPRE from res#0 );
    -- the help hack 2 (incidentally, this regex is safer)
    M2outputRE      = "\n+(?="|webAppEndTag|webAppCellTag|")"; -- TODO: improve so cleanly separates at Cells once #1553 resolved
    -- the print hack
    print = x -> if topLevelMode === WebApp then (
	y := html x; -- we compute the html now (in case it produces an error)
	<< webAppHtmlTag | y | webAppEndTag << endl;
	) else ( << net x << endl; );
    -- redefine htmlLiteral to exclude codes
    htmlLiteral1 = s -> replace("\\$","&dollar;",replace(webAppTagsRegex,"",htmlLiteral s));
    )

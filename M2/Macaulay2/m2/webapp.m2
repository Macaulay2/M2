-- Paul Zinn-Justin 2018-2022

needs "expressions.m2"
needs "matrix1.m2"
needs "monideal.m2"

-- topLevelMode=WebApp definitions
-- tags are required to help the browser app distinguish html from text
webAppTags = apply((17,18,19,20,28,29,30,14,21),ascii);
    (	webAppHtmlTag,        -- indicates what follows is HTML ~ <span class='M2Html'>
	webAppEndTag,         -- closing tag ~ </span>
	webAppCellTag,        -- start of cell (bundled input + output) ~ <p>
	webAppCellEndTag,     -- closing tag for cell ~ </p>
	webAppInputTag,       -- it's text but it's input ~ <span class='M2Input'>
	webAppInputContdTag,  -- text, continuation of input
	webAppLiteralTag,     -- used internally to keep track of encoding
	webAppPromptTag,      -- input/output prompt
	webAppPositionTag     -- code position (row:col)
	)=webAppTags;

webAppTagsRegex := concatenate("[",webAppTags,"]")

-- output routines for WebApp mode

recordPosition = () -> if currentFileName == "stdio" then ( -- for now only stdio recorded
    webAppPositionTag,
--    toString currentFileName,
--    ":",
    toString currentRowNumber(), -- not to be confused with lineNumber!!!
    ":",
    toString currentColumnNumber(),
    webAppEndTag
    )

ZZ#{WebApp,InputPrompt} = lineno -> concatenate(
    webAppCellEndTag, -- close previous cell
    webAppCellTag,
    webAppPromptTag,
    interpreterDepth:"i",
    toString lineno,
    webAppEndTag,
    " : ",
    webAppInputTag,
    recordPosition()
)

ZZ#{WebApp,InputContinuationPrompt} = lineno -> concatenate(
    webAppInputContdTag,
    recordPosition()
    )

Thing#{WebApp,BeforePrint} = identity

Nothing#{WebApp,Print} = identity

printFunc := Thing#{WebApp,print} = x -> (
    y := html x; -- we compute the html now (in case it produces an error)
    if class y =!= String then error "invalid html output";
    << webAppHtmlTag | y | webAppEndTag << endl;
    )

on := () -> concatenate(webAppPromptTag,interpreterDepth:"o", toString lineNumber,webAppEndTag)

timelimit := (t,f) -> (alarm t; r := f(); alarm 0; r)

Thing#{WebApp,Print} = x -> (
    << endl << on() | " = ";
    fun := () -> ( printFunc x; );
    try timelimit(printingTimeLimit, fun) else (
	alarm 0; -- in case it's another error that triggered try
	global debugError <- fun;
	stderr << "--error or time limit reached in conversion of output to html: type " | webAppHtmlTag | "<code data-m2code>debugError()</code>" | webAppEndTag |" to run it again; will try conversion to net" << endl;
	try timelimit(printingTimeLimit, () -> (
--		<< flush << net x << flush << endl; -- not quite right
		printFunc net x;
		)) else (
	    alarm 0;
	    error "time limit/error reached in conversion of output to net";
	    )
	);
    )

InexactNumber#{WebApp,Print} = x ->  withFullPrecision ( () -> Thing#{WebApp,Print} x )

-- afterprint

htmlAfterPrint :=  x -> (
    << endl << on() | " : ";
    if class x === Sequence then x = SPAN deepSplice { x };
    printFunc x;
    )

Thing#{WebApp,AfterPrint} = x -> (
    l:=lookup(AfterPrint,class x);
    if l === null then return;
    s:=l x;
    if s =!= null then htmlAfterPrint s
    )
Thing#{WebApp,AfterNoPrint} = x -> (
    l:=lookup(AfterNoPrint,class x);
    if l === null then return;
    s:=l x;
    if s =!= null then htmlAfterPrint s
    )

removeWebAppTags = s -> if s === null then null else replace(webAppTagsRegex,"😀",s);
if topLevelMode === WebApp then (
    -- the show/edit hack
    show URL := url -> ANCHOR { "href" => htmlLiteral url#0, "Opening "|url#0, "class" => "auto" };
    editMethod String := f -> show URL("#editor:"|f);
    editMethod FilePosition := editMethod @@ toURL; -- shouldn't that always be the case?
    -- redefine htmlLiteral to exclude codes
    htmlLiteral0 := htmlLiteral;
    html1 String := htmlLiteral = s -> if s === null then s else (
	s=separate(webAppLiteralTag,s);
	concatenate apply(#s, i -> if even i then removeWebAppTags htmlLiteral0 s#i else s#i)
	);
    -- the texMath hack
    scan(methods hypertext, (f,T) -> (
	    texMath T := x -> webAppLiteralTag|webAppHtmlTag|html x|webAppEndTag|webAppLiteralTag;
	    )
	)
    )

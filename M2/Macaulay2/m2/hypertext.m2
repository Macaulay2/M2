--  Copyright 1993-2003 by Daniel R. Grayson
-- Revamped by P. Zinn-Justin and Mahrud Sayrafi 2020

needs "debugging.m2" -- for Descent, FilePosition
needs "regex.m2" -- for toLower
needs "lists.m2" -- for all
needs "max.m2" -- for IndeterminateNumber

-----------------------------------------------------------------------------
-- Hypertext type declarations and basic constructors
-----------------------------------------------------------------------------

-- Hypertext, HypertextParagraph, and HypertextContainer
Hypertext = new Type of BasicList
Hypertext.synonym = "markup list"

-- A paragraph starts on a new line and ends with a line break.
HypertextParagraph = new Type of Hypertext
HypertextParagraph.synonym = "markup list paragraph"

-- A container's contents are indented
HypertextContainer = new Type of Hypertext
HypertextContainer.synonym = "markup list container"

-- these must be empty
HypertextVoid = new Type of Hypertext
HypertextVoid.synonym = "void markup"

toString         Hypertext := s -> concatenate(toString class s, toString         toList s)
toExternalString Hypertext := s -> concatenate(toString class s, toExternalString toList s)

new Hypertext from VisibleList := (M,x) -> x -- needed because otherwise next line takes over
new Hypertext from Thing  := (M,x) -> {x}
new Hypertext from Net    := (M,x) -> {toString x}

Hypertext#AfterPrint = x -> null

-----------------------------------------------------------------------------
-- URL type declaration and constructor
-----------------------------------------------------------------------------

URL = new SelfInitializingType of BasicList
new URL from String := (URL, str) -> { str }

-- relative URLs and filenames
isAbsoluteURL = url -> match( "^(#|mailto:|[a-z]+://)", url )

-- TODO: phase this one out eventually
toURL = method()
toURL String := pth -> (
     urlEncode if isAbsolutePath pth then concatenate(rootURI,
	  if fileExists pth then realpath pth
	  else (
	       stderr << "-- *** warning: file needed for URL not found: " << pth << endl;
	       pth))
     else if isAbsoluteURL pth then pth
     else (
	  r := if htmlDirectory === null then pth else relativizeFilename(htmlDirectory, pth);
	  if debugLevel == 121 then (
	       stderr << "--toURL String: htmlDirectory   = " << htmlDirectory << endl;
	       stderr << "--              pth             = " << pth << endl;
	       stderr << "--              relative result = " << r << endl;
	       );
	  r))

toURL(String, String) := (prefix,tail) -> (		    -- this is the good one
     -- we assume we are installing an html file in the directory installPrefix|htmlDirectory
     r := if prefix === installPrefix    -- note: installPrefix might be null, if we aren't installing a package
          and htmlDirectory =!= null
          then relativizeFilename(htmlDirectory,tail)
          else prefix|tail;
     if debugLevel == 121 then (
	  stderr << "--toURL(String,String): htmlDirectory = " << htmlDirectory << endl;
	  stderr << "--                      prefix        = " << prefix << endl;
	  stderr << "--                      result        = " << r << endl;
	  );
     urlEncode r)

-----------------------------------------------------------------------------
-- MarkUpType type declarations
-----------------------------------------------------------------------------

-- MarkUpType
MarkUpType = new Type of SelfInitializingType
MarkUpType.synonym = "markup type"

options MarkUpType := X -> if X.?Options then X.Options else new OptionTable from {}

-- e.g. a MENU, which does not correspond to an html entity.
-- It does not have a qname, nor a default method for producing html,
-- so one should be provided.
IntermediateMarkUpType = new Type of MarkUpType
IntermediateMarkUpType.synonym = "intermediate markup type"

-----------------------------------------------------------------------------
-- Setting GlobalAssignHooks
-----------------------------------------------------------------------------

MarkUpType.GlobalAssignHook = (X, x) -> (
    if not x.?qname then x.qname = toLower toString X;
    if not hasAttribute(x, ReverseDictionary) then setAttribute(x, ReverseDictionary, X))

IntermediateMarkUpType.GlobalAssignHook = globalAssignFunction

-----------------------------------------------------------------------------
-- MarkUptypes
-----------------------------------------------------------------------------
-- The qname (qualified name) is the lowercase name of the type by default.
-- See the end for exceptions (e.g. PARA.qname = p)

-- Standard html
HTML       = new MarkUpType of HypertextContainer
HEAD       = new MarkUpType of HypertextContainer
META       = new MarkUpType of HypertextVoid
LINK       = new MarkUpType of HypertextVoid
TITLE      = new MarkUpType of HypertextParagraph
BODY       = new MarkUpType of HypertextContainer
STYLE      = new MarkUpType of Hypertext
SPAN       = new MarkUpType of Hypertext
PARA       = new MarkUpType of HypertextParagraph -- double spacing inside
DIV        = new MarkUpType of HypertextContainer
BR         = new MarkUpType of HypertextVoid
HR         = new MarkUpType of HypertextVoid
SCRIPT     = new MarkUpType of HypertextParagraph

-- Headers
HEADER1    = new MarkUpType of HypertextParagraph
HEADER2    = new MarkUpType of HypertextParagraph
HEADER3    = new MarkUpType of HypertextParagraph
HEADER4    = new MarkUpType of HypertextParagraph
HEADER5    = new MarkUpType of HypertextParagraph
HEADER6    = new MarkUpType of HypertextParagraph
SUBSECTION = HEADER2

-- Emphasis (TODO: strikethrough)
EM         = new MarkUpType of Hypertext
ITALIC     = new MarkUpType of Hypertext
SMALL      = new MarkUpType of Hypertext
BOLD       = new MarkUpType of Hypertext
STRONG     = new MarkUpType of Hypertext
SUB        = new MarkUpType of Hypertext
SUP        = new MarkUpType of Hypertext
TT         = new MarkUpType of Hypertext -- not supported in HMTL5
-- TT replacements
SAMP        = new MarkUpType of Hypertext
KBD         = new MarkUpType of Hypertext
VAR         = new MarkUpType of Hypertext

-- Lists
OL         = new MarkUpType of HypertextContainer
UL         = new MarkUpType of HypertextContainer
LI         = new MarkUpType of HypertextContainer
DL         = new MarkUpType of HypertextContainer
DT         = new MarkUpType of HypertextParagraph
DD         = new MarkUpType of HypertextParagraph

-- Links and references
IMG        = new MarkUpType of HypertextVoid
ANCHOR     = new MarkUpType of Hypertext
LABEL      = new MarkUpType of Hypertext

-- Blockquotes
BLOCKQUOTE = new MarkUpType of HypertextParagraph

-- Code
CODE       = new MarkUpType of Hypertext
PRE        = new MarkUpType of HypertextParagraph

-- Tables
TABLE      = new MarkUpType of HypertextContainer
TR         = new MarkUpType of HypertextContainer
TD         = new MarkUpType of HypertextContainer
TH         = new MarkUpType of TD

-- Misc
CDATA      = new MarkUpType of Hypertext
COMMENT    = new MarkUpType of Hypertext
INPUT      = new MarkUpType of HypertextVoid
BUTTON     = new MarkUpType of Hypertext

-- Fake
TEX        = new IntermediateMarkUpType of Hypertext
ExampleItem = new IntermediateMarkUpType of Hypertext
HREF       = new IntermediateMarkUpType of Hypertext
LATER      = new IntermediateMarkUpType of Hypertext
LITERAL    = new IntermediateMarkUpType of Hypertext -- fake!!!!! check later
MENU       = new IntermediateMarkUpType of HypertextContainer -- e.g. help sum
TO         = new IntermediateMarkUpType of Hypertext
TO2        = new IntermediateMarkUpType of Hypertext
TOH        = new IntermediateMarkUpType of Hypertext
INDENT     = new IntermediateMarkUpType of HypertextContainer -- temporary: one day, once format.m2 sorted out, we can simply have INDENT = x -> append(DIV x, "class" => "indent")

-----------------------------------------------------------------------------
-- LATER
-----------------------------------------------------------------------------

toExternalString LATER := x -> toExternalString x#0()

-----------------------------------------------------------------------------
-- MarkUpType constructors
-----------------------------------------------------------------------------

new HypertextVoid from List := (X,x) -> if all(x, e -> instance(e, Option) or instance(e,OptionTable)) then x else error "expected empty list"
br = BR{}
hr = HR{}

-- used in the TEX constructor below
convertTexTable := new HashTable from {
    "url" => "HREF",
    "bf" => "BOLD",
    "em" => "EM",
    "it" => "ITALIC",
    "tt" => "TT",
    }

-- the regex should match the tag and content
-- inside braces as subexpressions #2 and #3
convertTex := (tags, re, str) -> (
    off  := 0;
    while (m := regex(re, off, str)) =!= null do (
	off = m#3#0;
	tag := substring(m#2, str);
	if match(tags, tag) then (
	    tag = convertTexTable#tag;
	    if debugLevel > 1 then printerr("parsing ", tag, " in TEX");
	    str = replace(regexQuote substring(m#1, str), "\"," | tag | "{" | format substring(m#3, str) | "},\"", str)));
    str)

new TEX from String    := (T, t) -> T {t}
new TEX from BasicList := (T, t) -> (
    -- TODO: https://www.overleaf.com/learn/latex/Font_sizes,_families,_and_styles#Reference_guide
    -- we do this so {"{\tt", TO sum, "}"} can be rendered correctly
    s := demark_"," \\ toExternalString \ t;
    -- parsing nested braces contained in \url{...}
    s = convertTex("url", ///((?:\\\\(url))?\{((?:[^}{]+|(?1))*+)\})///, s);
    -- parsing nested braces contained in {\xx ...}
    s = convertTex("bf|tt|em|it", ///(\{(?:\\\\(bf|tt|em|it))? *((?:[^}{]+|(?1))*+)\})///, s);
    -- miscellaneous replacements
    s = replace("---", "—", s);
    s = replace("\\b--\\b", "–", s);
    -- evaluate Hypertext types
    s = evaluateWithPackage(getpkg "Text", s, value);
    if instance(s, BasicList) then s else {s})

isLink = x -> instance(x, TO) or instance(x, TO2) or instance(x, TOH)

new TO   from Thing     :=
new TOH  from Thing     := (TO, x) -> new TO from {x}
-- document tags can be sequences or arrays, so keep them intact
new TO   from List      := (TO, x) -> if x#?1 then { makeDocumentTag x#0, concatenate drop(toSequence x,1) } else { makeDocumentTag x#0 }
new TO2  from List      :=
new TO2  from Sequence  := (TO2, x) -> { makeDocumentTag x#0, concatenate drop(toSequence x,1) }
new TOH  from List      := (TOH, x) -> { makeDocumentTag x#0 }
new HREF from List      := (HREF, x) -> (
    url := if x#?0 and (instance(x#0, String) or instance(x#0, Sequence) and #x#0 === 2 and all(x#0, y -> instance(y, String)))
    then x#0 else error "HREF expected URL to be a string or a sequence of 2 strings";
    if x#?1 then prepend(url, drop(x, 1)) else {url})

new OL from VisibleList := 
new UL from VisibleList := (T, x) -> apply(nonnull x, e -> (
	if class e === TO then LI{TOH{e#0}}
	else if instance(e, LI) or instance(e,Option) or instance(e,OptionTable) then e
	else LI e))

-- Written by P. Zinn-Justin
new TABLE from VisibleList := (T,x) -> (
    apply(nonnull x, e -> (
           if instance(e, TR) or instance(e, Option) or instance(e,OptionTable) then e else TR e)))
new TR from VisibleList := (T,x) -> (
    apply(nonnull x, e -> (
           if instance(e, TD) or instance(e, Option) or instance(e,OptionTable) then e else TD e)))

-- the main idea of these comparisons is so sorting will sort by the way things will print:
TO  ? TO  :=
TO  ? TOH :=
TOH ? TO  :=
TOH ? TOH := (x,y) -> x#0 ? y#0
TO  ? TO2 :=
TOH ? TO2 := (x,y) -> x#0 ? y#1
TO2 ? TO  :=
TO2 ? TOH := (x,y) -> x#1 ? y#0
TO2 ? TO2 := (x,y) -> x#1 ? y#1

-----------------------------------------------------------------------------
-- Fixing the qname for non-standard type names
-----------------------------------------------------------------------------
ANCHOR.qname  = "a"
BOLD.qname    = "b"
ExampleItem.qname = "code"
HEADER1.qname = "h1"
HEADER2.qname = "h2"
HEADER3.qname = "h3"
HEADER4.qname = "h4"
HEADER5.qname = "h5"
HEADER6.qname = "h6"
HREF.qname    = "a"
ITALIC.qname  = "i"
LITERAL.qname = "div"
MENU.qname    = "div"
PARA.qname    = "p"
TEX.qname     = "#PCDATA"
TO.qname      = "a"
TO2.qname     = "a"
TOH.qname     = "span"
INDENT.qname  = "div"

-----------------------------------------------------------------------------
-- Add acceptable html attributes to the type of an html tag
-----------------------------------------------------------------------------
addAttribute = (T, opts) -> (
    T.Options = new OptionTable from apply(opts, opt ->
	if class opt === Option then opt else opt => null))

-- html global attributes
htmlGlobalAttr = {
    "accesskey",
    "class",
    "contenteditable",
    --  "data-*", -- at the moment can't handle this
    "dir",
    "draggable",
    "dropzone",
    "hidden",
    "id",
    "lang",
    "spellcheck",
    "style",
    "tabindex",
    "title",
    "translate",
    "xmlns"
    }

scan({HTML, HEAD, TITLE, BODY}, T -> addAttribute(T, htmlGlobalAttr))
addAttribute(META,  htmlGlobalAttr | {"name", "content", "http-equiv"})
addAttribute(LINK,  htmlGlobalAttr | {"href", "rel", "title", "type"})
addAttribute(STYLE, htmlGlobalAttr | {"type"})
addAttribute(SCRIPT, htmlGlobalAttr | {"async", "crossorigin", "defer",
	"integrity", "nomodule", "referrerpolicy", "src", "type"})

-- html global and event attributes
htmlAttr = htmlGlobalAttr | {
    "onafterprint","onbeforeprint","onbeforeunload","onerror","onhashchange",
    "onload","onmessage","onoffline","ononline","onpagehide","onpageshow",
    "onpopstate","onresize","onstorage","onunload","onblur","onchange",
    "oncontextmenu","onfocusscript","oninputscript","oninvalid","onresetscript",
    "onsearch","onselect","onsubmit","onkeydown","onkeypress","onkeyup",
    "onclick","ondblclick","onmousedown","onmousemove","onmouseout",
    "onmouseover","onmouseup","onmousewheel","onwheel","ondrag","ondragend",
    "ondragenter","ondragleave","ondragover","ondragstart","ondrop","onscroll",
    "ontoggle"
    }

scan({BR, HR, PARA, PRE, HEADER1, HEADER2, HEADER3, HEADER4, HEADER5, HEADER6,
	BLOCKQUOTE, EM, ITALIC, SMALL, BOLD, STRONG, SUB, SUP, SPAN, TT, SAMP, KBD, VAR, LI, CODE,
	DL, DT, DD, UL, DIV, TABLE, TR}, T -> addAttribute(T, htmlAttr))
addAttribute(LABEL,  htmlAttr | {"for", "from"})
addAttribute(ANCHOR, htmlAttr | {"href", "rel", "target", "type"})
addAttribute(TD,     htmlAttr | {"colspan", "headers", "rowspan"})
addAttribute(TH,     htmlAttr | {"colspan", "headers", "rowspan"})
addAttribute(IMG,    htmlAttr | {"alt", "src", "srcset", "width", "height",
	"sizes", "crossorigin", "longdesc", "referrerpolicy", "ismap", "usemap"})
addAttribute(OL, htmlAttr | {"start"=>"0", "reversed", "type"})
buttonAttr = htmlAttr | {"autofocus","disabled",
    "form","formaction","formenctype","formmethod","formnovalidate","formtarget",
    "name", "type", "value"}
addAttribute(BUTTON, buttonAttr)
addAttribute(INPUT, buttonAttr | {"accept","alt","checked",
	"height", "list", "max", "maxlength", "min", "minlength", "multiple",
	"pattern", "placeholder", "readonly", "required", "size", "src", "step", "width" })

M2CODE = method()
M2CODE Thing := x -> prepend("class" => "language-macaulay2", CODE x)

-- Written by P. Zinn-Justin
style = method(Options => true)
style Hypertext := true >> o -> x -> (
    str := concatenate apply(keys o, key -> if class key === String then key|":"|toString o#key|";");
    if str === "" then return x;
    (ops,arg) := override(options class x,toSequence x);
    ops = applyPairs(ops,(k,v)->if k==="style" then (k,concatenate(v, if v=!=null and #v>0 and last v =!= ";" then ";",str)) else if v=!=null then (k,v));
    new class x from (toList sequence arg | apply(pairs ops,a->new Option from a))
    )

hypertext = method(Dispatch => Thing, TypicalValue => Hypertext)
hypertext Hypertext := identity
hypertext Descent := x -> SPAN prepend( "style" => "display:inline-table;text-align:left", -- TODO move style to CSS
    deepSplice apply(sortByName pairs x,
     (k,v) -> (
	  if #v === 0
	  then k
	  else (k, " : ", v)
	  , BR{})))
hypertext Time := x -> DIV { x#1, DIV ("-- ", toString x#0, " seconds", "class" => "token comment") }
SAMPc = c -> x -> SAMP {toString x,"class"=>"token "|c}
hypertext Command :=
hypertext FunctionBody :=
hypertext Function := f -> SAMP deepSplice {
    if hasAttribute(f,ReverseDictionary) then toString getAttribute(f,ReverseDictionary) else (
	t := locate if instance(f,Command) then f#0 else f;
	SPAN class f,
	"[", if t === null then "" else SPAN t, "]"
	),
    "class"=>"token function"
    }
hypertext File :=
hypertext IndeterminateNumber :=
hypertext Manipulator :=
hypertext Nothing :=
hypertext Boolean := SAMPc "constant"
hypertext Type :=
hypertext FilePosition :=
hypertext Dictionary := SAMPc "class-name"
hypertext String := SAMPc "string"
hypertext Net := n -> PRE {
    toString n, BR{},
    "class"=>"token net",
    if #n>0 and depth n!=0 then "style" => "vertical-align:"|toString(-100*depth n)|"%"
    }
hypertext VerticalList         := x -> if #x==0 then SPAN{"{}"} else UL append(apply(x, y -> new LI from hold y),"style"=>"display:inline-table")
hypertext NumberedVerticalList := x -> if #x==0 then SPAN{"{}"} else OL append(apply(x, y -> new LI from hold y),"style"=>"display:inline-table")
hypertext RawObject := hypertext @@ net

unique Hypertext := x -> new class x from unique toList x

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

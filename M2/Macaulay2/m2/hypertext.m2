--  Copyright 1993-2003 by Daniel R. Grayson
-- Revamped by P. Zinn-Justin and Mahrud Sayrafi 2020
-- html0.m2 -> hypertext.m2

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

toString         Hypertext := s -> concatenate(toString class s, toString         toList s)
toExternalString Hypertext := s -> concatenate(toString class s, toExternalString toList s)

new Hypertext from VisibleList := (M,x) -> x
new Hypertext from Thing  := (M,x) -> {x}
new Hypertext from Net    := (M,x) -> {toString x}

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
     if isAbsolutePath pth then concatenate(rootURI,
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
     r)

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
META       = new MarkUpType of HypertextParagraph
LINK       = new MarkUpType of HypertextParagraph
TITLE      = new MarkUpType of HypertextParagraph
BODY       = new MarkUpType of HypertextContainer
STYLE      = new MarkUpType of Hypertext
SPAN       = new MarkUpType of Hypertext
PARA       = new MarkUpType of HypertextParagraph -- double spacing inside
DIV        = new MarkUpType of HypertextContainer
BR         = new MarkUpType of Hypertext
HR         = new MarkUpType of HypertextParagraph

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
TT         = new MarkUpType of Hypertext

-- Lists (TODO: OL)
OL         = new MarkUpType of HypertextContainer
UL         = new MarkUpType of HypertextContainer
LI         = new MarkUpType of HypertextContainer
DL         = new MarkUpType of HypertextContainer
DT         = new MarkUpType of HypertextParagraph
DD         = new MarkUpType of HypertextParagraph

-- Links and references
IMG        = new MarkUpType of Hypertext
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

CDATA      = new MarkUpType of Hypertext
COMMENT    = new MarkUpType of Hypertext

TEX        = new MarkUpType of Hypertext -- TEX should be processed further so its output can be checked

ExampleItem = new IntermediateMarkUpType of Hypertext
HREF       = new IntermediateMarkUpType of Hypertext
LATER      = new IntermediateMarkUpType of Hypertext
LITERAL    = new IntermediateMarkUpType of Hypertext -- fake!!!!! check later
MENU       = new IntermediateMarkUpType of HypertextContainer -- e.g. help sum
TO         = new IntermediateMarkUpType of Hypertext
TO2        = new IntermediateMarkUpType of Hypertext
TOH        = new IntermediateMarkUpType of Hypertext

-----------------------------------------------------------------------------
-- LATER
-----------------------------------------------------------------------------

toExternalString LATER := x -> toExternalString x#0()

-----------------------------------------------------------------------------
-- EXAMPLE
-----------------------------------------------------------------------------
-- TODO: Move this

makeExampleItem = method()
makeExampleItem PRE    := identity -- this will allow precomputed example text
makeExampleItem String := s -> ExampleItem s
makeExampleItem Thing  := s -> error ("EXAMPLE expected a string or a PRE item, but encountered ", toString s)

trimfront := x -> apply(x, line -> if not instance(line, String) then line else (
	  s := lines line;
	  r := if not s#?0 then line else concatenate between(newline, prepend(replace("^[[:space:]]+", "", s#0), drop(s, 1)));
	  if #r =!= 0 then r))

EXAMPLE = method(Dispatch => Thing)
EXAMPLE String      := x -> EXAMPLE {x}
EXAMPLE VisibleList := x -> (
    x = nonnull trimfront toSequence x;
    if #x == 0 then error "empty list of examples encountered";
    TABLE splice {"class" => "examples", apply(x, item -> TR TD makeExampleItem item)})

-----------------------------------------------------------------------------
-- MarkUpType constructors
-----------------------------------------------------------------------------
-- TODO: Move this

new HR from List :=
new BR from List := (X,x) -> if all(x, e -> instance(e, Option)) then x else error "expected empty list"
br = BR{}
hr = HR{}

isLink = x -> instance(x, TO) or instance(x, TO2) or instance(x, TOH)

new TO   from Thing     :=
new TOH  from Thing     := (TO, x) -> new TO from {x}
-- document tags can be sequences or arrays, so keep them intact
new TO   from List      := (TO, x) -> if x#?1 then { makeDocumentTag x#0, concatenate drop(toSequence x,1) } else { makeDocumentTag x#0 }
new TO2  from List      :=
new TO2  from Sequence  := (TO2, x) -> { makeDocumentTag x#0, concatenate drop(toSequence x,1) }
new TOH  from List      := (TOH, x) -> { makeDocumentTag x#0 }
new HREF from List      := (HREF, x) -> (
     if #x > 2 or #x == 0 then error "HREF list should have length 1 or 2";
     y := x#0;
     if not (instance(y,String) or instance(y,Sequence) and #y===2 and instance(y#0,String) and instance(y#1,String))
     then error "HREF expected URL to be a string or a pair of strings"; x)

new OL from VisibleList :=
new UL from VisibleList := (T, x) -> (
    apply(nonnull x, e -> (
	    if class e === TO then LI{TOH{e#0}}
	    else if instance(e, LI) or instance(e,Option) then e
	    else LI e)))
-- TODO: deprecate this
ul = x -> ( x = nonnull x; if 0 < #x then UL x )

-- Written by P. Zinn-Justin
new TABLE from VisibleList := (T,x) -> (
    apply(nonnull x, e -> (
           if instance(e, TR) or instance(e, Option) then e else TR e)))
new TR from VisibleList := (T,x) -> (
    apply(nonnull x, e -> (
           if instance(e, TD) or instance(e, Option) then e else TD e)))

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
    "translate"
    }

scan({HTML, HEAD, TITLE, BODY}, T -> addAttribute(T, htmlGlobalAttr))
addAttribute(META,  htmlGlobalAttr | {"name", "content", "http-equiv"})
addAttribute(LINK,  htmlGlobalAttr | {"href", "rel", "title", "type"})
addAttribute(STYLE, htmlGlobalAttr | {"type"})

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
	BLOCKQUOTE, EM, ITALIC, SMALL, BOLD, STRONG, SUB, SUP, SPAN, TT, LI, CODE,
	DL, DT, DD, OL, UL, DIV, TABLE, TR}, T -> addAttribute(T, htmlAttr))
addAttribute(LABEL,  htmlAttr | {"for", "from"})
addAttribute(ANCHOR, htmlAttr | {"href", "rel", "target", "type"})
addAttribute(TD,     htmlAttr | {"colspan", "headers", "rowspan"})
addAttribute(TH,     htmlAttr | {"colspan", "headers", "rowspan"})
addAttribute(IMG,    htmlAttr | {"alt", "src", "srcset", "width", "height",
	"sizes", "crossorigin", "longdesc", "referrerpolicy", "ismap", "usemap"})


-- Written by P. Zinn-Justin
style = method(Options => true)
style Hypertext := true >> o -> x -> style(x, pairs o)
style(Hypertext, VisibleList) := true >> o -> (x, s) -> ( -- here s is a pair of key/values
    str := concatenate apply(s, e -> if class e#0 === String then e#0|":"|toString e#1|";");
    if str === "" then return x;
    i := position(toList x, y -> class y === Option and y#0 === "style");
    if i===null then append(x,"style"=>str) else
    new class x from replace(i,"style"=>x#i#1|(if #x#i#1>0 and last x#i#1 =!= ";" then ";" else "")|str,toList x)
    )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

--  Copyright 1993-2003 by Daniel R. Grayson
-- html0.m2 -> hypertext.m2

-----------------------------------------------------------------------------
-- Hypertext type declarations
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

-----------------------------------------------------------------------------
-- Markup type declarations
-----------------------------------------------------------------------------

-- MarkUpType
MarkUpType = new Type of SelfInitializingType
MarkUpType.synonym = "markup type"

new MarkUpType from Thing := (M,x) -> new M from {x}
new MarkUpType from List := (M,x) -> new M from x
new MarkUpType from Sequence := (M,x) -> new M from toList x
options MarkUpType := X -> X.Options

MarkUpType Net := (M,x) -> new M from {toString x}
MarkUpType String :=
MarkUpType Hypertext := (M,x) -> new M from {x}

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
    if not hasAttribute(x, ReverseDictionary) then (
	setAttribute(x, ReverseDictionary, X)))

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

new  HR  from List :=
new  BR  from List := (X, x) -> if 0 < #x then error "expected empty list" else x
br = BR{}
hr = HR{}

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
     x = nonnull x;
     if #x == 0 then error("empty element of type ", format toString T, " encountered");
     apply(x, e -> (
	       if class e === TO then LI{TOH{e#0}}
	       else if class e === LI then e
	       else LI e)))
ul = x -> ( x = nonnull x; if 0 < #x then UL x )

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
addAttribute := (T, opts) -> (
    T.Options = new OptionTable from apply(opts, opt ->
	if class opt === Option then opt else opt => null))

-- Add html attributes for some types
addAttribute(ANCHOR, {"id"})
addAttribute(DD,    {"class"})
addAttribute(DL,    {"class"})
addAttribute(DT,    {"class"})
addAttribute(DIV,   {"class"})
addAttribute(IMG,   {"src", "alt"})
addAttribute(LABEL, {"title"})
addAttribute(LINK,  {"href", "rel", "title", "type"})
addAttribute(META,  {"name", "content", "http-equiv"})
addAttribute(SPAN,  {"lang"})
addAttribute(STYLE, {"type"})
addAttribute(TABLE, {"class"})
addAttribute(TD,    {"valign"})

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

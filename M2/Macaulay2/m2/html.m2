-*- coding: utf-8 -*-
-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

needs "format.m2"
needs "system.m2" -- for getViewer
needs "monoids.m2" -- for Monoid

getStyleFile := fn -> locateCorePackageFileRelative("Style",
    layout -> replace("PKG", "Style", layout#"package") | fn,
    installPrefix, htmlDirectory);

-- TODO: unify the definition of the tex macros so book/M2book.tex can use them
KaTeX := () -> (
    katexPath := getStyleFile "katex";
    katexTemplate := ///
    <link rel="stylesheet" href="%PATH%/katex.min.css">
    <script defer="defer" src="%PATH%/katex.min.js"></script>
    <script defer="defer" src="%PATH%/contrib/auto-render.min.js"></script>
    <script>
      var macros = {
          "\\break": "\\\\",
          "\\ZZ": "\\mathbb{Z}",
          "\\NN": "\\mathbb{N}",
          "\\QQ": "\\mathbb{Q}",
          "\\RR": "\\mathbb{R}",
          "\\CC": "\\mathbb{C}",
          "\\PP": "\\mathbb{P}"
      }, delimiters = [
          { left: "$$",  right: "$$",  display: true},
          { left: "\\[", right: "\\]", display: true},
          { left: "$",   right: "$",   display: false},
          { left: "\\(", right: "\\)", display: false}
      ], ignoredTags = [
	  "kbd", "var", "samp", "script", "noscript",
	  "style", "textarea", "pre", "code", "option" ];
      document.addEventListener("DOMContentLoaded", function() {
        renderMathInElement(document.body, { delimiters: delimiters, macros: macros, ignoredTags: ignoredTags, trust: true });
      });
    </script>
    <style>.katex { font-size: 1em; }</style>
    <script defer="defer" src="%PATH%/contrib/copy-tex.min.js"></script>
    <script defer="defer" src="%PATH%/contrib/render-a11y-string.min.js"></script>///;
    LITERAL replace("%PATH%", katexPath, katexTemplate | newline))

-- The default stylesheet for documentation
defaultStylesheet := () -> LINK {
    "rel" => "stylesheet", "type" => "text/css",
    "href" => getStyleFile "doc.css"}

-- Also set the character encoding with a meta http-equiv statement. (Sometimes XHTML
-- is parsed as HTML, and then the HTTP header or a meta tag is used to determine the
-- character encoding.  Locally-stored documentation does not have an HTTP header.)
defaultCharset := () -> META { "http-equiv" => "Content-Type", "content" => "text/html; charset=utf-8" }

defaultHEAD = title -> HEAD splice { TITLE title, defaultCharset(), defaultStylesheet(), KaTeX(),
    SCRIPT {"src" => getStyleFile "prism.js", ""},
    SCRIPT {"var current_version = '", version#"VERSION", "';"},
    SCRIPT {"src" => getStyleFile "version-select.js"},
    LINK {
	"rel" => "icon", "type" => "image/x-icon",
	"href" => getStyleFile "icon.gif"}}

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

htmlLiteral = s -> if s === null or regex("<|&|]]>|\42", s) === null then s else (
     s = replace("&", "&amp;", s); -- this one must come first
     s = replace("<", "&lt;", s);
     s = replace("]]>", "]]&gt;", s);
     s = replace("\42", "&quot;", s);  -- note: \42 is "
     s )

-- tracking indentations
indentLevel := -1
pushIndentLevel =  n     -> (indentLevel = indentLevel + n; n)
popIndentLevel  = (n, s) -> (indentLevel = indentLevel - n; s)

-----------------------------------------------------------------------------
-- Setup default rendering
-----------------------------------------------------------------------------

-- This method applies to all types that inherit from Hypertext
-- Most MarkUpTypes automatically work recursively
html1 = method(Dispatch=>Thing)
html1 String := htmlLiteral -- slightly annoying workaround for the ambiguous role of strings in/out of Hypertext
html1 Thing := html
html1 Nothing := x -> ""

scan(methods hypertext, (h,t) -> html t := html @@ hypertext)
html Hypertext := x -> (
    T := class x;
    qname := T.qname;
    attr := "";
    cont := if T.?Options then (
	(op, ct) := override(options T, toSequence x);
	scanPairs(op, (key, val) -> (
		if val =!= null
		then attr = " " | key | "=" | format toString val | attr));
	sequence ct) else x;
    pushIndentLevel 1;
    (head, prefix, suffix, tail) := (
	if instance(x, HypertextVoid) and class x =!= BR
	or instance(x, HypertextContainer) then (concatenate(indentLevel:"  "), newline, concatenate(indentLevel:"  "), newline) else
	if instance(x, HypertextParagraph) then (concatenate(indentLevel:"  "), "", "", newline) else ("","","",""));
    -- LI should look like a paragraph if it doesn't have any containers
    if instance(x, LI) then (
	if not any(x, e -> instance(e, HypertextContainer)) then prefix = suffix = "" else (
	    if not instance(first x, HypertextContainer) then prefix = "";
	    if not instance(last  x, HypertextContainer) then suffix = ""));
    popIndentLevel(1, if instance(x, HypertextVoid)
	then concatenate(head, "<", qname, attr, ">", tail)
	else concatenate(head, "<", qname, attr, ">", prefix,
	    apply(cont, html1), suffix, "</", qname, ">", tail)))

-----------------------------------------------------------------------------
-- Exceptional (html, MarkUpType) methods
-----------------------------------------------------------------------------

-- TOH  -- see format.m2

html LITERAL := x -> concatenate x
--html String  := x -> htmlLiteral x
html TEX     := x -> concatenate apply(x, html1) -- TODO: retire this

html HTML := x -> demark(newline, {
    	///<!DOCTYPE html>///,
    	///<html lang="en">///,
    	popIndentLevel(pushIndentLevel 1, apply(x, html)),
	///</html>///})

treatImgSrc := x -> apply(x, y -> if class y === Option and y#0 === "src" then "src" => toURL y#1 else y)
html IMG := (lookup(html, IMG)) @@ treatImgSrc

toStringMaybe := method()
toStringMaybe Hypertext :=
toStringMaybe Nothing :=
toStringMaybe OptionTable :=
toStringMaybe Option := identity
toStringMaybe Thing := x -> replace("\r\n","\n",toString x) -- toString prevents LaTeX being inserted...
-- ... since non HTML types should *not* be KaTeX-ified inside these tags:
html PRE :=
html SAMP :=
html KBD :=
html CODE := (lookup(html, Hypertext)) @@ (x -> apply(x,toStringMaybe))

-- hack for HTML5 validation
-- ideally, TT should be removed and replaced with CODE, KBD, SAMP, and/or VAR
html TT := x -> html SPAN prepend("class" => "tt", apply(toList x,toStringMaybe))

html CDATA   := x -> concatenate("<![CDATA[", x ,"]]>", newline)
html COMMENT := x -> if match("--", concatenate x) then
    error ///html comments cannot contain "--"/// else
    concatenate("<!--", x, "-->", newline)

html HREF := x -> (
     r := concatenate apply(splice if #x > 1 then drop(x, 1) else x, html1);
     r = if match("^ +$", r) then #r : "&nbsp;&nbsp;" else r;
     concatenate("<a href=\"", htmlLiteral toURL first x, "\">", r, "</a>")
     )

html MENU := x -> html redoMENU x

html INDENT := x -> html DIV append(toList x, "class"=>"indent")

html TO   := x -> html TO2{tag := x#0, format tag | if x#?1 then x#1 else ""}
html TO2  := x -> (
    tag := getPrimaryTag fixup x#0;
    fkey := format tag;
    -- TODO: add this to htmlLiteral?
    name := if match("^ +$", x#1) then #x#1 : "&nbsp;&nbsp;" else x#1;
    if isUndocumented tag or isMissingDoc tag then concatenate(
	html TT name, " (missing documentation)",
	html COMMENT("tag: ", toString tag.Key)) else
    concatenate(html ANCHOR{"title" => htmlLiteral headline tag, "href"  => toURL htmlFilename tag, name}))

----------------------------------------------------------------------------
-- html'ing non Hypertext
----------------------------------------------------------------------------

html Monoid :=
html RingFamily :=
html Ring :=
html Thing := x -> "$" | htmlLiteral texMath x | "$" -- by default, we use math mode tex (as opposed to actual html)

-----------------------------------------------------------------------------
-- Viewing rendered html in a browser
-----------------------------------------------------------------------------

showHtml =
show Hypertext := x -> (
    fn := temporaryFileName() | ".html";
    addEndFunction( () -> if fileExists fn then removeFile fn );
    fn << html HTML { defaultHEAD "Macaulay2 Output", BODY {x}} << endl << close;
    show URL urlEncode(rootURI | realpath fn))
show URL := url -> (
    cmd := { getViewer("WWWBROWSER", "firefox"), url#0 }; -- TODO: silence browser messages, perhaps with "> /dev/null"
    if fork() == 0 then (
        setGroupID(0,0);
        try exec cmd;
        stderr << "exec failed: " << toExternalString cmd << endl;
        exit 1);
    sleep 1;) -- let the browser print errors before the next M2 prompt

-----------------------------------------------------------------------------
-- urlEncode (originally in OnlineLookup)
-----------------------------------------------------------------------------

percentEncoding =  new MutableHashTable from toList apply(
    -- unreserved characters from RFC 3986
    -- ALPHA / DIGIT / "-" / "." / "_" / "~"
    -- we also add "/" and ":" since they're standard URL characters
    -- also "#" for named anchors
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890-._~/:#",
    c -> (c, c))
    -- everything else will be percent encoded and added to the hash table
    -- as needed

urlEncode = method()
urlEncode Nothing := identity
urlEncode String := s -> concatenate apply(s, c -> (
	if percentEncoding#?c then percentEncoding#c
	else percentEncoding#c = "%" | toUpper changeBase(first ascii c, 16)))

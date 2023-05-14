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
    <link rel="stylesheet" href="%PATH%/katex.min.css" />
    <script defer="defer" type="text/javascript" src="%PATH%/katex.min.js"></script>
    <script defer="defer" type="text/javascript" src="%PATH%/contrib/auto-render.min.js"></script>
    <script defer="defer" type="text/javascript">
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
      ], ignoredTags = ["tt", "script", "noscript", "style", "textarea", "pre", "code", "option"];
      document.addEventListener("DOMContentLoaded", function() {
        renderMathInElement(document.body, { delimiters: delimiters, macros: macros, ignoredTags: ignoredTags, trust: true });
      });
    </script>
    <style type="text/css">.katex { font-size: 1em; }</style>
    <link href="%PATH%/contrib/copy-tex.min.css" rel="stylesheet" type="text/css" />
    <script defer="defer" type="text/javascript" src="%PATH%/contrib/copy-tex.min.js"></script>
    <script defer="defer" type="text/javascript" src="%PATH%/contrib/render-a11y-string.min.js"></script>///;
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
    SCRIPT {
	"type" => "text/javascript",
	"src" => getStyleFile "prism.js",
	""
	}
    }

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- TODO: urlEncode
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
	scanPairs(op, (key, val) -> if val =!= null then attr = " " | key | "=" | format val | attr);
	sequence ct) else x;
    pushIndentLevel 1;
    (head, prefix, suffix, tail) := (
	if instance(x, HypertextContainer) then (concatenate(indentLevel:"  "), newline, concatenate(indentLevel:"  "), newline) else
	if instance(x, HypertextParagraph) then (concatenate(indentLevel:"  "), "", "", newline) else ("","","",""));
    popIndentLevel(1, if instance(x, HypertextVoid)
	then concatenate(head, "<", qname, attr, "/>", tail)
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
    	///<?xml version="1.0" encoding="utf-8" ?>///,
    	///<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">///,
    	///<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">///,
    	popIndentLevel(pushIndentLevel 1, apply(x, html)),
	///</html>///})

treatImgSrc := x -> apply(x, y -> if class y === Option and y#0 === "src" then "src" => toURL y#1 else y)
html IMG := (lookup(html, IMG)) @@ treatImgSrc

fixNewLines := method()
fixNewLines Hypertext :=
fixNewLines Nothing :=
fixNewLines Option := identity
fixNewLines Thing := x -> replace("\r\n","\n",toString x)
-- non HTML types should *not* be KaTeX-ified inside these tags:
html PRE :=
html TT :=
html CODE := (lookup(html, Hypertext)) @@ (x -> apply(x,fixNewLines))

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

html Nothing := x -> "null"

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
    show new URL from replace(" ", "%20", rootURI | realpath fn)) -- TODO: urlEncode might need to replace more characters
show URL := url -> (
    cmd := { getViewer("WWWBROWSER", "firefox"), url#0 }; -- TODO: silence browser messages, perhaps with "> /dev/null"
    if fork() == 0 then (
        setGroupID(0,0);
        try exec cmd;
        stderr << "exec failed: " << toExternalString cmd << endl;
        exit 1);
    sleep 1;) -- let the browser print errors before the next M2 prompt

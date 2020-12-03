-*- coding: utf-8 -*-
-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

KaTeX := () -> (
    katexPath := locateCorePackageFileRelative("Style",
	layout -> replace("PKG", "Style", layout#"package") | "katex", installPrefix, htmlDirectory);
    katexTemplate := ///
    <link rel="stylesheet" href="%PATH%/katex.min.css" />
    <script defer="defer" type="text/javascript" src="%PATH%/katex.min.js"></script>
    <script defer="defer" type="text/javascript" src="%PATH%/contrib/auto-render.min.js"></script>
    <script defer="defer" type="text/javascript">
      var macros = {
          "\\break": "\\\\",
          "\\R": "\\mathbb{R}",
          "\\C": "\\mathbb{C}",
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
      ];
      document.addEventListener("DOMContentLoaded", function() {
        renderMathInElement(document.body, { delimiters: delimiters, macros: macros, trust: true });
      });
    </script>
    <style type="text/css">.katex { font-size: 1em; }</style>
    <link href="%PATH%/contrib/copy-tex.min.css" rel="stylesheet" type="text/css" />
    <script defer="defer" type="text/javascript" src="%PATH%/contrib/copy-tex.min.js"></script>
    <script defer="defer" type="text/javascript" src="%PATH%/contrib/render-a11y-string.min.js"></script>///;
    LITERAL replace("%PATH%", katexPath, katexTemplate))

-- The default stylesheet for documentation
defaultStylesheet := () -> LINK {
    "rel" => "stylesheet", "type" => "text/css",
    "href" => locateCorePackageFileRelative("Style",
	layout -> replace("PKG", "Style", layout#"package") | "doc.css", installPrefix, htmlDirectory)}

-- Also set the character encoding with a meta http-equiv statement. (Sometimes XHTML
-- is parsed as HTML, and then the HTTP header or a meta tag is used to determine the
-- character encoding.  Locally-stored documentation does not have an HTTP header.)
defaultCharset := () -> META { "http-equiv" => "Content-Type", "content" => "text/html; charset=utf-8" }

defaultHEAD = title -> HEAD splice { TITLE title, defaultCharset(), defaultStylesheet(), KaTeX() }

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

-- whether fn exists on the path
-- TODO: check executable
runnable := fn -> (
    if fn == "" then return false;
    if isAbsolutePath fn then fileExists fn
    else 0 < # select(1, apply(separate(":", getenv "PATH"), p -> p|"/"|fn), fileExists))

-- preferred web browser
-- TODO: cache this value
browser := () -> (
    if runnable getenv "WWWBROWSER" then getenv "WWWBROWSER" -- compatibility
    else if version#"operating system" === "Darwin" and runnable "open" then "open" -- Apple varieties
    else if runnable "xdg-open" then "xdg-open" -- most Linux distributions
    else if runnable "firefox" then "firefox" -- backup
    else error "neither open nor xdg-open is found and WWWBROWSER is not set")

-----------------------------------------------------------------------------
-- Setup default rendering
-----------------------------------------------------------------------------

-- This method applies to all types that inherit from Hypertext
-- Most MarkUpTypes automatically work recursively
html1 := x -> (if class x === String then htmlLiteral else html) x -- slightly annoying workaround for the ambiguous role of strings in/out of Hypertext

html Hypertext := x -> (
    T := class x;
    qname := T.qname;
    attr := "";
    cont := if T.?Options then (
	(op, ct) := try override(options T, toSequence x) else error("markup type ", toString T, ": ",
	    "unrecognized option name(s): ", toString select(toList x, c -> instance(c, Option)));
	scanPairs(op, (key, val) -> if val =!= null then attr = " " | key | "=" | format val | attr);
	sequence ct) else x;
    pushIndentLevel 1;
    (head, prefix, suffix, tail) := (
	if instance(x, HypertextContainer) then (concatenate(indentLevel:"  "), newline, concatenate(indentLevel:"  "), newline) else
	if instance(x, HypertextParagraph) then (concatenate(indentLevel:"  "), "", "", newline) else ("","","",""));
    popIndentLevel(1, if #cont == 0
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

treatImgSrc := x -> apply(x, y -> if class y === Option and y#0 === "src" then "src" => htmlLiteral toURL y#1 else y)
html IMG := (lookup(html, IMG)) @@ treatImgSrc

fixDollar := x -> replace("\\$","&dollar;",x)
splitLines := x -> apply(x, y -> if class y === String then demark(newline, lines y) else y) -- effectively, \r\n -> \n and removes last [\r]\n
html PRE := fixDollar @@ (lookup(html, PRE)) @@ splitLines
html TT := fixDollar @@ (lookup(html, TT))
html CODE := fixDollar @@ (lookup(html, CODE))

html CDATA   := x -> concatenate("<![CDATA[",x,"]]>")
html COMMENT := x -> concatenate("<!--",x,"-->")

html HREF := x -> (
     r := concatenate apply(splice if #x > 1 then drop(x, 1) else x, html1);
     r = if match("^ +$", r) then #r : "&nbsp;&nbsp;" else r;
     concatenate("<a href=\"", htmlLiteral toURL first x, "\">", r, "</a>")
     )

html MENU := x -> html redoMENU x

html TO   := x -> html TO2{tag := x#0, format tag | if x#?1 then x#1 else ""}
html TO2  := x -> (
    tag := getPrimaryTag x#0;
    fkey := format tag;
    -- TODO: add this to htmlLiteral?
    name := if match("^ +$", x#1) then #x#1 : "&nbsp;&nbsp;" else x#1;
    if isUndocumented tag then concatenate(html TT name, " (missing documentation<!-- tag: ", toString tag.Key, " -->)") else
    if isMissingDoc   tag then concatenate(html TT name, " (missing documentation<!-- tag: ", toString tag.Key, " -->)") else
    concatenate(html ANCHOR{"title" => htmlLiteral headline tag, "href"  => toURL htmlFilename tag, name}))

----------------------------------------------------------------------------
-- html'ing non Hypertext
----------------------------------------------------------------------------

html Thing := htmlLiteral @@ tex -- by default, we use tex (as opposed to actual html)

htmlLiteral1 = fixDollar @@ htmlLiteral

-- text stuff: we use html instead of tex, much faster (and better spacing)
html Net := n -> concatenate("<pre style=\"display:inline-table;vertical-align:",
    toString(if height n+depth n>0 then 100*(height n-1) else 0), "%\">\n", -- the % is relative to line-height
    apply(unstack n, x-> htmlLiteral1 x | "<br/>"), "</pre>")
html String := x -> concatenate("<pre style=\"display:inline\">\n", htmlLiteral1 x,
    if #x>0 and last x === "\n" then " ", -- fix for html ignoring trailing \n
    "</pre>")
html Descent := x -> concatenate("<pre style=\"display:inline-table\">\n", sort apply(pairs x,
     (k,v) -> (
	  if #v === 0
	  then html k
	  else html k | " : " | html v
	  ) | "<br/>"), "</pre>")
-- a few types are just strings
html Boolean :=
html Function :=
html Type := html @@ toString
-- except not these descendants
html Monoid :=
html RingFamily :=
html Ring := lookup(html,Thing)

--html VerticalList         := x -> html UL apply(x, y -> new LI from hold y)
--html NumberedVerticalList := x -> html OL apply(x, y -> new LI from hold y)

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
    cmd := { browser(), url#0 }; -- TODO: silence browser messages, perhaps with "> /dev/null"
    if fork() == 0 then (
        setGroupID(0,0);
        try exec cmd;
        stderr << "exec failed: " << toExternalString cmd << endl;
        exit 1);
    sleep 1;) -- let the browser print errors before the next M2 prompt

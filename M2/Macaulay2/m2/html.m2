-*- coding: utf-8 -*-
-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Common utilities
-----------------------------------------------------------------------------

-- TODO: urlEncode
htmlLiteral = s -> if s === null or not match("<|&|]]>|\42", s) then s else (
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
html Hypertext := x -> (
    T := class x;
    qname := T.qname;
    attr := "";
    cont := if member(Options, keys T) then (
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
	    apply(cont, html), suffix, "</", qname, ">", tail)))

-----------------------------------------------------------------------------
-- Exceptional (html, MarkUpType) methods
-----------------------------------------------------------------------------

-- TEX  -- see texhtml.m2
-- TOH  -- see format.m2
-- MENU -- see format.m2 -- e.g. help sum

html String := htmlLiteral

html HTML := x -> demark(newline, {
    	///<?xml version="1.0" encoding="utf-8" ?>///,
    	///<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN" "http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd">///,
    	///<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">///,
    	popIndentLevel(pushIndentLevel 1, apply(x, html)),
	///</html>///})

treatImgSrc := x -> apply(x, y -> if class y === Option and y#0 === "src" then "src" => htmlLiteral toURL y#1 else y)
html IMG := (lookup(html, IMG)) @@ treatImgSrc

splitLines := x -> apply(x, y -> if class y === String then demark(newline, lines y) else y) -- effectively, \r\n -> \n and removes last [\r]\n
html PRE := (lookup(html, PRE)) @@ splitLines

html CDATA   := x -> concatenate("<![CDATA[",x,"]]>")
html COMMENT := x -> concatenate("<!--",x,"-->")

html HREF := x -> (
     r := html last x;
     r = if match("^ +$", r) then #r : "&nbsp;&nbsp;" else r;
     concatenate("<a href=\"", htmlLiteral toURL first x, "\">", r, "</a>")
     )

-- TODO
html LITERAL := x -> concatenate x

-- TODO: reduce this
html TO   := x -> (
     tag := x#0;
     d := fetchPrimaryRawDocumentation tag;
     r := htmlLiteral DocumentTag.FormattedKey tag;
     if match("^ +$",r) then r = #r : "&nbsp;&nbsp;";
     if d#?"undocumented" and d#"undocumented" === true then (
	  if signalDocError tag then (
	       stderr << "--warning: tag cited also declared as undocumented: " << tag << endl;
	       warning();
	       );
	  concatenate( "<tt>", r, "</tt>", if x#?1 then x#1, " (missing documentation<!-- tag: ",toString DocumentTag.Key tag," -->)")
	  )
     else if d === null					    -- isMissingDoc
     then (
	  warning("missing documentation: "|toString tag);
	  concatenate( "<tt>", r, "</tt>", if x#?1 then x#1, " (missing documentation<!-- tag: ",toString DocumentTag.Key tag," -->)")
	  )
     else concatenate( "<a href=\"", toURL htmlFilename getPrimary tag, "\" title=\"", htmlLiteral headline tag, "\">", r, "</a>", if x#?1 then x#1))

html TO2  := x -> (
     tag := x#0;
     headline tag;		   -- this is a kludge, just to generate error messages about missing links
     d := fetchPrimaryRawDocumentation tag;
     if d#?"undocumented" and d#"undocumented" === true then (
	  if signalDocError tag then (
	       stderr << "--warning: tag cited also declared as undocumented: " << tag << endl;
	       warning();
	       );
	  concatenate("<tt>", htmlLiteral x#1, "</tt> (missing documentation<!-- tag: ",DocumentTag.FormattedKey tag," -->)")
	  )
     else if d === null					    -- isMissingDoc
     then (
	  warning("missing documentation: "|toString tag);
	  concatenate("<tt>", htmlLiteral x#1, "</tt> (missing documentation<!-- tag: ",DocumentTag.FormattedKey tag," -->)"))
     else concatenate("<a href=\"", toURL htmlFilename getPrimary tag, "\">", htmlLiteral x#1, "</a>"))

html VerticalList         := x -> html UL apply(x, html)
html NumberedVerticalList := x -> html OL apply(x, html)

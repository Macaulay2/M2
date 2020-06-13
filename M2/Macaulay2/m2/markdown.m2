--  Copyright 2020 by Mahrud Sayrafi
-- See https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
-- TODO: task lists

-----------------------------------------------------------------------------
-- Common utilities
-----------------------------------------------------------------------------

jekyllLayout := "layout: package\n"

-- Trim extra newlines
shorten := s -> replace("\n+$", "\n", concatenate(s, newline))

-- Parse attributes
parseAttr := x -> first override((class x).Options, toSequence x)

-----------------------------------------------------------------------------
-- Setup default rendering
-----------------------------------------------------------------------------

-- Rendering by concatenation of rendered inputs
setupRenderer(markdown, concatenate, Hypertext)

-----------------------------------------------------------------------------
-- (markdown, MarkUpType) methods
-----------------------------------------------------------------------------

-- See hypertext.m2 for the full list
markdown HTML  := x -> concatenate(apply(x, markdown))
markdown HEAD  := x -> concatenate("---\n", jekyllLayout, apply(x, markdown), "---\n")
markdown TITLE := x -> concatenate("title: ", apply(x, markdown), newline)

markdown BODY  := x -> shorten concatenate(apply(x, markdown))
markdown SPAN  := x -> concatenate(apply(x, markdown), newline)
markdown PARA  := x -> concatenate(apply(x, markdown), 2:newline)
markdown DIV   := x -> (
    c := (parseAttr x)#"class";
    concatenate(shorten apply(x, markdown), if c =!= null then ("{:.", c, "}"), 2:newline))

markdown LABEL :=
markdown SMALL :=
markdown MENU  :=
markdown SUB   :=
markdown SUP   :=
markdown TEX   := html

markdown LINK  :=
markdown META  :=
markdown STYLE :=
markdown Option  :=
markdown Nothing := x -> ""
markdown COMMENT := x -> concatenate("<!--", apply(x, markdown), "-->")

markdown LITERAL :=
markdown String  := identity

-- Headers
markdown HEADER1 := x -> concatenate(newline, 1:"#", " ", apply(x, markdown), newline)
markdown HEADER2 := x -> concatenate(newline, 2:"#", " ", apply(x, markdown), newline)
markdown HEADER3 := x -> concatenate(newline, 3:"#", " ", apply(x, markdown), newline)
markdown HEADER4 := x -> concatenate(newline, 4:"#", " ", apply(x, markdown), newline)
markdown HEADER5 := x -> concatenate(newline, 5:"#", " ", apply(x, markdown), newline)
markdown HEADER6 := x -> concatenate(newline, 6:"#", " ", apply(x, markdown), newline)

-- Emphasis
-- TODO: markdown STRIKE -* ~~Scratch this.~~ *-
markdown EM     :=
markdown ITALIC := x -> concatenate("_",  apply(x, markdown), "_")  -- *asterisks* or _underscores_
markdown BOLD   :=
markdown STRONG := x -> concatenate("**", apply(x, markdown), "**") -- **asterisks** or __underscores__.

-- Lists
lvl := -1              -- nesting level
ctr := new MutableList -- counters for ordered list
markdown OL := x -> (
    lvl = lvl + 1; ctr#lvl = 0;
    out := shorten concatenate(newline, apply(x, markdown));
    lvl = lvl - 1; out)
markdown UL := x -> (
    lvl = lvl + 1; ctr#lvl = -1;
    out := shorten concatenate(newline, apply(x, markdown));
    lvl = lvl - 1; out)
markdown LI := x -> (
    pre := concatenate(lvl:"   ", if ctr#lvl < 0 then  "- " else (ctr#lvl = ctr#lvl + 1; toString ctr#lvl | ". "));
    shorten concatenate(pre, apply(x, markdown)))

-- Description lists
markdown DL := x -> shorten concatenate(apply(x, markdown));
markdown DT := x -> shorten concatenate(apply(x, markdown));
markdown DD := x -> shorten concatenate(lvl:"   ", ": ", apply(x, markdown))

-- Links
-- (markdown, TOH) defined in format.m2
markdown TO     :=
markdown TO2    :=
markdown ANCHOR := html
markdown HREF   := x -> concatenate("[", markdown last x, "](", toURL first x, ")")

-- Images
markdown IMG := x -> (
    (o, cn) := override(IMG.Options, toSequence x);
    if o#"alt" === null then error ("IMG item is missing alt attribute");
    concatenate("![", format o#"alt", "](\"", toURL o#"src", "\")"))

-- Code and Syntax Highlighting
markdown PRE  := x -> markdown concatenate x -* TODO: syntax highlighting *-
markdown TT   := x -> concatenate("`", apply(x, markdown), "`")
markdown CODE := x -> concatenate(
    newline, "```macaulay2", newline, shorten apply(x, markdown), "```", newline)

-- Blockquotes
-- TODO: new lines should also start with >
markdown BLOCKQUOTE := x -> concatenate("> ", apply(x, markdown), 2:newline)

-- Horizontal Rule
markdown HR := x -> "---"

-- Line Breaks
markdown BR := x -> "<br/>\n"

-- Tables
-- | Tables        | Are           | Cool  |
-- | ------------- |:-------------:| -----:|
-- | col 3 is      | right-aligned | $1600 |
-- | col 2 is      | centered      |   $12 |
-- | zebra stripes | are neat      |    $1 |
markdown TABLE := x -> (
    c := (parseAttr x)#"class";
    concatenate(shorten markdown new CODE from x, if c =!= null then ("{:.", c, "}\n")))
markdown TR    :=
markdown TD    := x -> concatenate(apply(x, markdown), newline)

--		Copyright 2006 by Daniel R. Grayson

warning := x -> stderr << "--warning: " << concatenate x << endl

noqname := (x,c) -> (
     if instance(c,IntermediateMarkUpType)
     then warning(format toString x," is an instance of an intermediate mark-up type ", format toString c, " with no qname, appearing in hypertext during validation")
     else error (format toString x," is of an unrecognized type ", format toString c, " with no qname, appearing in hypertext during validation")
     )

haderror := false
oops := x -> (haderror = true; x)


validate2 = method()					    -- check extra requirements, such as having at least one element, and the order of the elements
validate2 Hypertext := x -> null
validate2 HTML := x -> (
     if not x#?0 or not class x#0 === HEAD then oops stderr << "--warning: first element of HTML must be HEAD" << endl;
     if not x#?1 or not class x#1 === BODY then oops stderr << "--warning: second element of HTML must be BODY" << endl;
     if #x != 2 then oops stderr << "--warning: HTML should have 2 elements" << endl);
validate2 HEAD := x -> (
     c := tally (class \ toList x);
     if c_TITLE == 0 then oops stderr << "--warning: HEAD should have a TITLE element" << endl;
     if c_TITLE > 1 then oops stderr << "--warning: HEAD should have at most one TITLE element" << endl;
     -- BASE html item is currently unimplemented:
     -- if c_BASE > 1 then oops stderr << "--warning: HEAD should have at most one BASE element" << endl;
     )
validate2 DL :=
validate2 TR :=
validate2 UL :=
validate2 OL :=
validate2 BLOCKQUOTE := x -> if #x === 0 then oops stderr << "--warning: " << class x << " should contain at least one element" << endl
validate2 LATER := x -> validate2 x#0()

validate = method()
validate String := x -> (
     utf8check x;
     -- activate this later, after we convert the strings with htmlLiteral
     -- if match("<",x) then oops stderr << "--warning: string contains tag start character '<' : \"" << x << "\"" << endl;
     )

validate Option :=					    -- could check each part is a string!
validate TO :=
validate TOH :=
validate TO2 := x -> scan(drop(x,1),validate)
validate COMMENT := x -> (
     if match("--",concatenate x) then oops stderr << "--warning: encountered \"--\" within COMMENT" << endl;
     if match("-$",concatenate x) then oops stderr << "--warning: COMMENT ends with \"-\"" << endl;
     )
validate CDATA := x -> (
     if match("]]>",concatenate x) then oops stderr << "--warning: encountered \"]]>\" within CDATA" << endl;
     )
validate LITERAL := validate TEX := x -> (
     -- don't know what to do here yet...
     )
validate Hypertext := x -> (
     c := class x;
     if c.?qname then (
	  n := c.qname;
	  if validContent#?n then validate(c,validContent#n,x)
	  else error("--internal error: valid content for qname ", format n, " not recorded yet");
	  scan(x,validate))
     else noqname(x,c);
     validate2 x;
     x)
chk := (valid,p,c,x) -> (
     if not c.?qname then noqname(x,c)
     else if not valid#?(c.qname) and c.qname =!= "comment"
     then oops stderr << "--warning: element of type " << format toString p << " can't contain an element of type " << format toString c << endl
     )
validate(Type, Set, BasicList) := (p,valid,x) -> (
     haderror = false;
     scan(x, e -> chk(valid,p,class e,e));
     if haderror then error("validation failed: ", x))
validate(MarkUpType, Set, BasicList) := (p,valid,x) -> (
     haderror = false;
     scan(x, e -> if class e =!= Option then chk(valid,p,class e,e)); 
     if haderror then error("validation failed: ", x))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

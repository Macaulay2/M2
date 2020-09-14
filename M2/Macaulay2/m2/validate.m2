--		Copyright 2006 by Daniel R. Grayson
-- validate and fix Hypertext objects

-- TODO: make this local
currentHelpTag = null

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

haderror := false

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

warning   := x -> stderr << "-- warning: " << concatenate x << endl
flagError := x -> (haderror = true; warning x)

noqname := (x, tag) -> (
    if instance(tag, IntermediateMarkUpType)
    then warning(format toString x, " is an instance of an intermediate markup type ", format toString tag, " with no qname, appearing in hypertext during validation")
    else error(format toString x, " is of an unrecognized type ", format toString tag, " with no qname, appearing in hypertext during validation"))

-----------------------------------------------------------------------------
-- validate
-----------------------------------------------------------------------------

validate = method()
validate String := x -> (
    utf8check x;
    -- activate this later, after we convert the strings with htmlLiteral
    -- if match("<", x) then flagError("string contains tag start character '<' : ", format x);
    )
validate Hypertext := x -> (
    tag := class x;
    if not tag.?qname then noqname(x, tag) else (
	n := tag.qname;
	-- see content.m2
	if validContent#?n then validate(tag, validContent#n, x)
	else error("--internal error: valid content for qname ", format n, " not recorded yet"));
    scan(splice x, validate))

validate HTML       := x -> (
    if #x != 2 then flagError "HTML should have 2 elements";
    if not instance(x#0, HEAD) then flagError "first element of HTML must be HEAD";
    if not instance(x#1, BODY) then flagError "second element of HTML must be BODY";)
validate HEAD       := x -> (
    c := length select(toList x, y -> instance(y, TITLE));
    if c == 0 then flagError "HEAD should have a TITLE element";
    if c > 1  then flagError "HEAD should have at most one TITLE element";)
validate DL         :=
validate TR         :=
validate UL         :=
validate OL         :=
validate Option     :=
validate TO         :=
validate TOH        :=
validate TO2        := x -> scan(drop(x, 1), validate)
validate CDATA      := x -> (
     if match("]]>",concatenate x) then flagError "encountered \"]]>\" within CDATA";)
validate COMMENT    := x -> (
     if match("--", concatenate x) then flagError "encountered \"--\" within COMMENT";
     if match("-$", concatenate x) then flagError "COMMENT ends with \"-\"";)
validate BLOCKQUOTE := x -> if #x === 0 then flagError(toString class x, " should contain at least one element")
validate LITERAL    :=
validate LATER      := x -> validate x#0()
validate TEX        := x -> ( -* don't know what to do here yet... *- )

chk := (valid, tag, c, x) -> (
    if not c.?qname then noqname(x, c)
    else if not valid#?(c.qname) and c.qname =!= "comment"
    then flagError("element of type ", format toString tag, " can't contain an element of type ", format toString c))
validate(Type,       Set, BasicList) := (tag, valid, x) -> (
    haderror = false;
    scan(x, e -> chk(valid, tag, class e, e));
    if haderror then error("validation failed: ", x))
validate(MarkUpType, Set, BasicList) := (tag, valid, x) -> (
    haderror = false;
    scan(x, e -> if class e =!= Option then chk(valid, tag, class e, e));
    if haderror then error("validation failed: ", x))

-----------------------------------------------------------------------------
-- Fixing hypertext issues
-----------------------------------------------------------------------------

fixup = method(Dispatch => Thing)
fixup Thing       := z -> error("unrecognizable item ", toString z, " of class ", toString class z, " encountered while processing documentation node ", toString currentHelpTag)
fixup List        := z -> fixup toSequence z
fixup Nothing     := z -> () -- this will get removed by splice later
fixup Sequence    :=
fixup Hypertext   := z -> splice apply(z, fixup)
fixup MarkUpType  := z -> (
    if instance(z, BR)
    or instance(z, HR)        then error("using '", toString z, "' alone in documentation is no longer supported, use '", toString z, "{}' instead")
    else if instance(z, PARA) then error("using '", toString z, "' alone in documentation is no longer supported, use 'PARA{...}' around a paragraph")
    else error("isolated mark up type encountered: ", toString z))

fixup HREF        := x -> if #x == 2 then HREF{x#0, fixup x#1} else x
fixup String      := s -> demark_" " separate("[ \t]*\r?\n[ \t]*", s)

fixup Option      :=
fixup BR          :=
fixup HR          :=
fixup CODE        :=
fixup PRE         :=
fixup IMG         :=
fixup ANCHOR      :=
fixup LABEL       :=
fixup ExampleItem :=
fixup LATER       :=
fixup LITERAL     :=
fixup TO          :=
fixup TO2         :=
fixup TOH         := identity

-- TODO: move this
hypertext = method(Dispatch => Thing)
hypertext Hypertext := fixup
hypertext Sequence  :=
hypertext List      := x -> fixup DIV x

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

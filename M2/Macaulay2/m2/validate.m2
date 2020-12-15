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

flagError := x -> (haderror = true; printerr("warning: ", x))

noqname := (tag, x) -> (
    if instance(tag, IntermediateMarkUpType)
    then warning(format toString x, " is an instance of an intermediate markup type ", format toString tag, " with no qname, appearing in hypertext during validation")
    else   error(format toString x, " is of an unrecognized type ", format toString tag, " with no qname, appearing in hypertext during validation"))

-- see content.m2
chk := (p, x) -> (
    c := class x;
    if c === Option or c === LITERAL then return;
    if not c.?qname then return noqname(c, x);
    if not validContent#(p.qname)#?(c.qname) and c.qname =!= "comment"
    then flagError("element of type ", toString p, " may not contain an element of type ", toString c))

-----------------------------------------------------------------------------
-- validate
-----------------------------------------------------------------------------

validate = method()
validate(Type, BasicList) := (T, x) -> (
    haderror = false;
    if not T.?qname then return noqname(T, x);
    if not validContent#?(T.qname) then error("internal error: valid content for qname ", toString T, " not recorded yet");
    scan(x, e -> chk(T, e));
    if haderror then error("validation failed: ", x))

validate LATER      := x -> validate x#0()
validate LITERAL    :=
validate TEX        :=
validate Thing      := identity
validate Option     := x -> apply(splice x, validate)

-- TODO: should this check for unescaped "<"?
validate String     := x -> (utf8check x; x)
validate Hypertext  := x -> (
    validate(class x, x);
    apply(splice x, validate))

validate HTML       := x -> (
    if #x != 2 then flagError "HTML should have 2 elements";
    if not instance(x#0, HEAD) then flagError "first element of HTML must be HEAD";
    if not instance(x#1, BODY) then flagError "second element of HTML must be BODY";
    -- TODO: make every part return the content, then use apply
    apply(splice x, validate))
validate HEAD       := x -> (
    validate(class x, x);
    c := length select(toList x, y -> instance(y, TITLE));
    if c != 1 then flagError "HEAD should have exactly one TITLE element";
    apply(splice x, validate))

validate HREF       := identity -- TODO
validate TO         :=
validate TOH        :=
validate TO2        := x -> (
    tag := fixup x#0;
    if isUndocumented tag and signalDocumentationWarning tag then printerr("warning: undocumented node " | format tag | " cited by " | format currentDocumentTag) else
    if isMissingDoc   tag and signalDocumentationWarning tag then printerr("warning: missing node: "     | format tag | " cited by " | format currentDocumentTag);
    apply(splice x, validate))

validate CDATA      := x -> (
     if match("]]>",concatenate x) then flagError "encountered \"]]>\" within CDATA"; x)
validate COMMENT    := x -> (
     if match("--", concatenate x) then flagError "encountered \"--\" within COMMENT";
     if match("-$", concatenate x) then flagError "COMMENT ends with \"-\""; x)

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

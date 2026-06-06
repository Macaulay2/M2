newPackage ("Macaulay2Doc",
    Version => version#"VERSION",
    Headline => "Macaulay2 documentation",
    HomePage => "https://macaulay2.com/",
    Authors => { -* see the contributors listed on the main page *- },
    Keywords => { "Documentation" },
    InfoDirSection => "Macaulay2 and its packages",
    AuxiliaryFiles => true
)

-- a local way to use private global symbols from Core
core = nm -> value Core#"private dictionary"#nm
isMissingDoc = core "isMissingDoc"
isUndocumented = core "isUndocumented"

beginDocumentation()

-- move undocumented nodes from Core here
scan(keys Core#"raw documentation", key ->
    Macaulay2Doc#"raw documentation"#key =
    remove(Core#"raw documentation", key))

-- load the full documentation
load "./Macaulay2Doc/loads.m2"

--------------------------------------------------------------------------------
-- check to make sure the documentation doesn't leak symbols
erase \ { symbol core, symbol isMissingDoc, symbol isUndocumented }
if keys Macaulay2Doc#"private dictionary" =!= {} 
then error splice (
     "global symbols inadvertently defined by package Macaulay2Doc: ", 
     toSequence between_", " values Macaulay2Doc#"private dictionary")

--------------------------------------------------------------------------------

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Macaulay2Doc RemakePackages=false RemakeAllDocumentation=false IgnoreExampleErrors=false"
-- End:

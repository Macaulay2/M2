-- -*- coding: utf-8 -*-
newPackage ("Macaulay2Doc",
     InfoDirSection => "Macaulay 2 and its packages", 
     AuxiliaryFiles => true,
     Headline => "Macaulay 2 documentation", 
     Authors => {
	  {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	  },
     HomePage => "http://www.math.uiuc.edu/Macaulay2/",
     Version => version#"VERSION")

Core#"base packages" = prepend("Macaulay2Doc",Core#"base packages")

beginDocumentation()

scan(pairs Core#"raw documentation", (k,v) -> Macaulay2Doc#"raw documentation"#k = v)

load replace("\\.m2$","/loads.m2",toAbsolutePath currentFileName)

if keys Macaulay2Doc#"private dictionary" =!= {} 
then error splice("Macaulay2 documentation: global symbols inadvertently defined: ", 
     toSequence between_", " apply(values Macaulay2Doc#"private dictionary", s -> (
	       p := locate s;
	       toString s | " [" | p#0 | ":" | toString p#1 | "]"
	       )))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages install-Macaulay2"
-- End:

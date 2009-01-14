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

-- this is big, so we always notify:
stderr << "--loading the Macaulay 2 documentation from " << currentFileDirectory << "Macaulay2Doc/" << endl

scan(pairs Core#"raw documentation", (k,v) -> Macaulay2Doc#"raw documentation"#k = v)

load "./Macaulay2Doc/loads.m2"				    -- the ./ makes it load from the current directory

if keys Macaulay2Doc#"private dictionary" =!= {} 
then error splice (
     "global symbols inadvertently defined by package Macaulay2Doc: ", 
     toSequence between_", " values Macaulay2Doc#"private dictionary")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages install-Macaulay2"
-- End:

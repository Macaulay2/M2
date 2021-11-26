-- -*- coding: utf-8 -*-

-- maybe we can eliminate this package and put all the documentation into the Core package
-- eventually

newPackage ("Macaulay2Doc",
     InfoDirSection => "Macaulay2 and its packages", 
     AuxiliaryFiles => true,
     Headline => "Macaulay2 documentation", 
     Authors => {
	  {Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"},
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"}
	  },
     Keywords => {"Documentation"},
     PackageExports => Core#"pre-installed packages",
     HomePage => "http://www.math.uiuc.edu/Macaulay2/",
     Version => version#"VERSION")

scan(pairs Core#"raw documentation", (k,v) -> (
	  remove(Core#"raw documentation", k);
	  Macaulay2Doc#"raw documentation"#k = v;
	  ))

beginDocumentation()

-- a local way to use private global symbols from Core
core = nm -> value Core#"private dictionary"#nm
load "./Macaulay2Doc/loads.m2"				    -- the ./ makes it load from the current directory
erase symbol core

if keys Macaulay2Doc#"private dictionary" =!= {} 
then error splice (
     "global symbols inadvertently defined by package Macaulay2Doc: ", 
     toSequence between_", " values Macaulay2Doc#"private dictionary")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Macaulay2Doc RemakePackages=false RemakeAllDocumentation=false IgnoreExampleErrors=false"
-- End:

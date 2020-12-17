-- -*- coding: utf-8 -*-
newPackage( "BeginningMacaulay2", Version => "1.0", Date => "November 3, 2009",
     Authors => {
	  {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de"},
	  {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	  },
     Headline => "Mathematicians' Introduction to Macaulay2",
     Keywords => {"Documentation"},
     AuxiliaryFiles => true
     )
beginDocumentation()
doc get (currentFileDirectory|"BeginningMacaulay2/tutorial")
end
restart
installPackage "BeginningMacaulay2"
viewHelp "BeginningMacaulay2"
uninstallPackage "BeginningMacaulay2"
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=BeginningMacaulay2 RemakeAllDocumentation=true IgnoreExampleErrors=false"
-- End:

-- -*- coding: utf-8 -*-
newPackage( "BeginningMacaulay2", Version => "1.0", Date => "November 3, 2009",
     Authors => {
	  {Name => "David Eisenbud", Email => "de@math.berkeley.edu", HomePage => "https://eisenbud.github.io"},
	  {Name => "Mike Stillman", Email => "mikestillman1@gmail.com", HomePage => "https://mikestillman.github.io"}
	  },
     Headline => "Mathematicians' Introduction to Macaulay2",
     Keywords => {"Documentation"},
     PackageExports => {"Complexes"},
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

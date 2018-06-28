document { 
     Key => { (uninstallPackage,String), uninstallPackage, [uninstallPackage,InstallPrefix]},
     Headline => "uninstall a package",
     Usage => "uninstallPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME",
          InstallPrefix => { "see the description of this option for ", TO "installPackage" }
	  },
     Consequences => {
	  { "the links to the files and the files of the specified package created by ", 
	       TO "installPackage", ", in case encapsulation is enabled, are removed,
	       for every version of the package." }
	  },
     SeeAlso => uninstallAllPackages
     }

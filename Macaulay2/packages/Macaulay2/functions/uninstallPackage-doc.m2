document { 
     Key => {uninstallPackage,(uninstallPackage,String),(uninstallPackage,Package)},
     Headline => "uninstall a package",
     Usage => "uninstallPackage PACKAGENAME",
     Inputs => {
	  "PACKAGENAME" => {"a ", TO String, " or ", TO Package},
	  PackagePrefix => { "see the description of this option for ", TT "installPackage" },
          InstallPrefix => { "see the description of this option for ", TT "installPackage" },
	  Encapsulate => Boolean => "whether the files were encapsulated when installed",
	  MakeLinks => Boolean => "whether the links were made when installed"
	  },
     Consequences => {
	  { "the links to the files of the specified package created by ", TO "installPackage", ", in case encapsulation is enabled, are removed" }
	  }
     }

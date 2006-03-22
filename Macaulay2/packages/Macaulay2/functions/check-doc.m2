--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {check,(check,Package)},
     Headline => "perform tests of a package",
     Usage => "check P",
     Inputs => {
	  "P" => Package => "which has been previously installed"
	  },
     Consequences => {
	  "The tests in the package P are run (in separate Macaulay 2 processes), and 
	  any errors are reported."
	  },     
     "For example, to run the tests for the LLLBases package (Lenstra-Lenstra-Lovasz bases)
     do the following.",
     PRE///installPackage "LLLBases"///,
     PRE///check LLLBases///,
     SeeAlso => {"packages", installPackage, loadPackage}
     }



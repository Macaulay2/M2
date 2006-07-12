--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {isUnit, (isUnit, ZZ), (isUnit, RingElement), (isUnit, Number)},
     Headline => "whether a ring element is a unit",
     Usage => "isUnit r",
     Inputs => {
	  "r" => RingElement
	  },
     Outputs => {
	  Boolean => {TO "true", " if the ", TO2("RingElement","ring element"), " ", TT "r", 
	       " generates an ideal containing the multiplicative identity ", TT "1", " and ", 
	       TT "false", " otherwise" }
	  },
     EXAMPLE {
	  "R = QQ[z]/(z^2+1);",
	  "isUnit z",
	  "S = QQ[x,y]/(1-(x-1)*(y-1));",
	  "isUnit (x^2 - 2*x + 1)",
	  "isUnit x"	  
	  },
     }

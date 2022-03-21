--- status: DRAFT
--- author(s): L. Gold
--- notes:

document { 
     Key => {atan2,(atan2,RR,RR),(atan2, RRi, RRi)
	  },
     Headline => "compute an angle of a certain triangle",
     Usage => "atan2(y,x)\natan2(y,I)\natan2(J,x)\natan2(J,I)",
     Inputs => { "y" => RR, "x" => RR, "J" => RRi, "I" => RRi},
     Outputs => { 
	  RR => { "the angle (in radians) formed with the x-axis by the ray from the origin to the point ", TT "(x,y)" },
      RR => { "an interval containing the angles (in radians) formed with the x-axis by the rays from the origin to the points of ", TT "(I,y),(x,J),(I,J)" }
	  },
     EXAMPLE {
	  "atan2(1,0)",
	  "atan2(0,1)",
	  "atan2(-1,-1)"
	  },
     SeeAlso => { atan }
     }

document { 
     Key => {atan,(atan,RR),(atan,CC),(atan, RRi)},
     Headline => "compute the arctangent of a number ",
     Usage => "atan x\natan I",
     Inputs => { "x" => RR, "I" => RRi},
     Outputs => {
	  RR => {"the arctangent (in radians) of ", TT "x"},
      RRi => { "an interval containing the arctangents of the points of ", TT "I" }
	  },
     EXAMPLE {
     	  "atan 1",
	  "abs atan 1 == pi/4"
	  },
     SeeAlso => { atan2 }
     }

document { 
     Key => {acot,(acot,Number)},
     Headline => "compute the inverse cotangent",
     Usage => "acot x",
     Inputs => { "x" => RR },
     Outputs => {
	  RR => {"the arccotangent (in radians) of ", TT "x"} 
	  },
     EXAMPLE {
     	  "acot 2",
	  }
     }

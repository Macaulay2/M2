--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => atan,
     Headline => "compute the arctangent",
     Usage => "atan x"
     }

document { 
     Key => {(atan2,RR,RR),atan2,
	  (atan2, ZZ, ZZ),(atan2, QQ, ZZ),(atan2, ZZ, QQ),
	  (atan2, QQ, QQ),(atan2, RR, ZZ),(atan2, ZZ, RR),(atan2, QQ, RR),
	  (atan2, RR, QQ)
	  },
     Headline => "compute an angle of a certain triangle",
     Usage => "atan2(y,x)",
     Inputs => { "y" => RR, "x" => RR },
     Outputs => { 
	  RR => { "the angle (in radians) formed with the x-axis by the ray from the origin to the point ", TT "(x,y)" } 
	  },
     EXAMPLE {
	  "atan2(1,0)",
	  "atan2(0,1)",
	  "atan2(-1,-1)"
	  },
     SeeAlso => { atan }
     }

document { 
     Key => {(atan,RR),(atan,ZZ),(atan,CC),(atan, QQ)},
     Headline => "compute the arctangent of a number ",
     Usage => "atan x",
     Inputs => { "x" => RR },
     Outputs => {
	  RR => {"the arctangent (in radians) of ", TT "x"} 
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

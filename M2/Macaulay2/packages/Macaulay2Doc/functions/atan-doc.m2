--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => atan,
     Headline => "compute the arctangent",
     Usage => "atan x"
     }
document { 
     Key => {(atan,RR,RR),
	  (atan, ZZ, ZZ),(atan, QQ, ZZ),(atan, ZZ, QQ),
	  (atan, QQ, QQ),(atan, RR, ZZ),(atan, ZZ, RR),(atan, QQ, RR),
	  (atan, RR, QQ)
	  },
     Headline => "compute an angle of a certain triangle",
     Usage => "atan(x,y)",
     Inputs => { "x" => RR, "y" => RR },
     Outputs => { 
	 RR => { "the angle (in radians) formed with the x-axis by the ray from the origin to the point ", TT "(x,y)" } 
	  },
     EXAMPLE {
	  "atan(sqrt(3.0)/2,1/2)",
	  "-- Notice this is not quite pi/6, but it is within a reasonable epsilon.
epsilon = 10.^-15;",	  
	  "abs(atan(sqrt(3.0)/2,1.0/2) - pi/6) < epsilon"
	  }
     }
document { 
     Key => {(atan,RR),(atan,ZZ),(atan, QQ)},
     Headline => "compute the arctangent of a number ",
     Usage => "atan x",
     Inputs => { "x" => RR },
     Outputs => {
	  RR => {"the arctangent (in radians) of ", TT "x"} 
	  },
     EXAMPLE {
     	  "atan 1",
	  "abs atan 1 == pi/4"
	  }
     }

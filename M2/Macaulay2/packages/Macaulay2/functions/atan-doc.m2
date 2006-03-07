--- status: DRAFT
--- author(s): L. Gold
--- notes: needs examples

document { 
     Key => atan,
     Headline => "compute the arctangent"
}
document { 
     Key => (atan,RR,RR),
     Headline => "compute the angle of a triangle determined by a point",
     Usage => "atan(x,y)",
     Inputs => { 
	  "x" => RR => null, 
	  "y" => RR => null
	  },
     Outputs => { 
	 RR => { "the angle formed with the x-axis by the ray from the origin to the point ", TT "(x,y)" } 
	  }
--     EXAMPLE {
--	  }
     }
document { 
     Key => {(atan,RR),(atan,ZZ)},
     Headline => "compute the arctangent of a number ",
     Usage => "atan x",
     Inputs => { 
	  "x" => RR => null 
	  },
     Outputs => {
	  RR => {"the arctangent of ", TT "x"} 
	  }
--     EXAMPLE {
--	  }
     }

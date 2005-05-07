--- status: Draft
--- author(s): Amelia Taylor
--- notes: Be sure to note that trim I and trim R^1/I do 
---        the same thing as the minimal relations for 
---        R^1/I are the minimal generators for I.

document { 
     Key => trim,
     Headline => "minimize generators and relations",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (trim,Ideal),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (trim,Ring),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (trim,Module),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (trim,QuotientRing),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc8.m2:880:     Key => trim,

///

R = ZZ/101[x,y,z,u,w]
I = ideal(x^2-x^2-y^2,z^2+x*y,w^2-u^2,x^2-y^2)
trim I
trim (R^1/I)
R = ZZ/32003[a..d]
M = coker matrix {{a,1,b},{c,3,b+d}}
trim M
prune M
///

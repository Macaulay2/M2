--- status: TODO
--- author(s): 
--- notes: 

document { Key => degrees,
     Headline => "degrees of generators" }
document {
     Key => {
	 (degrees, Ring),
     	 (degrees, QuotientRing),
    	 (degrees, FractionField),
	 (degrees, Module),
	 (degrees, Monoid),
	 (degrees, Ideal)},
     Headline => "degrees of generators",
     Usage => "degrees A",
     Inputs => {"A" => { ofClass Ring, ", ", ofClass Ideal, ", ", ofClass Module, ", or ", ofClass Monoid }
	  },
     Outputs => {
	  List => { "the list of multi-degrees for the generators of ", TT "A"}
	  },
     EXAMPLE lines ///
	  R = ZZ/101[x,y,z];
      	  degrees R
	  S = ZZ/101[x,y,z,Degrees => {{2,3},{1,2},{2,0}}];
      	  degrees S
      ///,
     "This function also applies to ideals, modules, and monoids.",
     EXAMPLE lines ///
          I = ideal"xy2,xyz,y3"
	  degrees I
      ///,
     EXAMPLE lines ///
      	  degrees R^5
      	  degrees R^{1,2,3,4}
	  ///,
      SeeAlso => {degreeLength, degreesRing}
      }
document { 
     Key => (degrees,Matrix),
     Headline => "degrees of target and source",
     Usage => "degrees f",
     Inputs => {
	  "f"
	  },
     Outputs => {
	  { "a list ", TT "{x,y}", " where ", TT "x", " is the list
	       of degrees of the target of ", TT "f", " and ", TT "y", " is the
	       list of degrees of the source of ", TT "f", "." }
	  },
     EXAMPLE lines ///
     	  S = ZZ/101[x,y,z,Degrees => {{2,3},{1,2},{2,0}}];
	  degrees vars S
	  ///,
     SeeAlso => {degreeLength, degreesRing}
     }

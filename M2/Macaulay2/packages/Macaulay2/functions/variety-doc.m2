--- status: TODO
--- author(s): 
--- notes: 


document {
     Key => (variety, CoherentSheaf),
     TT "variety F", " -- produce the variety over which a coherent sheaf is defined.",
     PARA{},
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "OO_X(3)",
	  "variety oo"
	  }
     }

document {
     Key => variety,
     Headline => "get the variety"
     }

document { 
     Key => (variety,SheafOfRings),
     Usage => "variety O",
     Inputs => { "O" },
     Outputs => { { "the variety over which O is a qusicoherent sheaf of rings" } },
     EXAMPLE lines ///
     	  X = Proj(QQ[x..z])
	  O = OO_X
	  variety O
     ///,
     SourceCode => {(variety,SheafOfRings)}
     }

document { 
     Key => (variety,Ideal),
     Headline => "the closed projective subvariety defined by an ideal",
     Usage => "variety I",
     Inputs => { "I" => "a homogeneous ideal" },
     Outputs => {"the closed subvariety defined by an ideal"},
     Caveat => {"An alternative task for this function would be to define the affine subvariety, so if something like this eventually becomes useful,
	  we may have to redesign it.  Suggestions welcome."},
     "In the example, we compute the dimension of a line in the projective plane.",
     EXAMPLE lines ///
     	  R = QQ[x..z]
	  variety ideal x
	  dim oo
     ///
     }


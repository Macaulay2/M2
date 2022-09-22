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
     Outputs => { { "the variety over which O is a quasicoherent sheaf of rings" } },
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

doc ///
  Key
    (variety,Ring)
    (variety,RingElement)
  Headline
    the variety previously associated to a given ring
  Usage
    variety A
  Inputs
    A:Ring
      the intersection ring of a variety $X$, say, or the homogeneous (Cox) ring of a normal toric variety,
      or another ring that has been associated to a variety, or an element in such a ring
  Outputs
    :
      $X$, the variety associated with {\tt A}, or with the ring of {\tt A}, if {\tt A} is a @TO "RingElement"@
  Description
   Example
     needsPackage "NormalToricVarieties"
     X = toricProjectiveSpace 1
     S = ring X
     X === variety S
   Example
     needsPackage "Schubert2"
     Y = abstractProjectiveSpace 1
     IY = intersectionRing Y
     Y === variety IY
   Text
     If a @TO RingElement@ is provided, then the variety of its ring is returned.
   Example
     variety S_0
     variety IY_0
   Text
     For package developers: All this function does is to look up the symbol {\tt variety} in {\tt A}.  This is currently
     used in two packages, but can be used in other settings, if desired.
  SeeAlso
    "NormalToricVarieties::NormalToricVarieties"
    "Schubert2::Schubert2"
///


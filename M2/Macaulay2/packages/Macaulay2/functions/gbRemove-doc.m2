--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {gbRemove,
	  (gbRemove,Ideal),
	  (gbRemove,Matrix),
	  (gbRemove,Module)},
     Headline => "remove Groebner basis",
     Usage => "gbRemove M",
     Inputs => { "M" => {ofClass{Ideal,Matrix,Module}}},
     Consequences => {"all Groebner bases computed for M are removed"},
     "This is a simple way to remove the space associated with large Groebner bases
     that are no longer needed.",
     EXAMPLE lines ///
     	  R = ZZ[a]/(a^2-3)[x,y]
	  F = y^2-x*(x-1)*(x-a)
	  J = ideal(diff(x,F),diff(y,F),F)
	  gens gb J
	  peek J.generators.cache
	  gbRemove J
	  peek J.generators.cache
          ///,
     SeeAlso =>{ gb, "gbTrace", gbSnapshot}
     }

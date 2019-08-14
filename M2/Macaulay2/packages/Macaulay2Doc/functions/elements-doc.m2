--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {elements,(elements,Tally),(elements,Set)},
     Headline => "list of elements",
     Usage => "elements S",
     Inputs => {
	  "S" => {ofClass Set, ", or ", ofClass Tally}
	  },
     Outputs => {
	  List
	  },
     EXAMPLE lines ///
     	  set{1,2,3,1,2,4}
	  elements oo
	  ///,
     EXAMPLE lines ///
     	  tally apply(10, i -> random 5)
	  elements oo
	  ///,
     SeeAlso => {tally, set}
     }

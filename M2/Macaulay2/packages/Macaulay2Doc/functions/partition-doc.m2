--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => {partition,(partition,Function,VisibleList),(partition,Function,Tally)},
     Headline => "partition a set or list by values of a function",
     Usage => "partition(f,L)",
     Inputs => {
	  "f" => Function,
	  "L" => {ofClass VisibleList, ", or ", ofClass Tally}
	  },
     Outputs => {
	  HashTable => {"whose keys are the possible values of the function ", TT "f", 
	       " on the elements of the list, set or tally ", TT "L", 
	       ", and the corresponding value of the hash table is the list, set or 
	       tally of elements of ", TT "L", " which take that value under ", TT "f"}
	  },
     EXAMPLE lines ///
     	  L = {1,3,6,5,3,1,2,8,8,8}
	  partition(odd, L)
	  partition(odd, set L)
	  partition(odd, tally L)
	  ///,
     "The following example parititions the generators of an ideal by degree.",
     EXAMPLE lines ///
          R = QQ[a..f]
	  I = ideal"ab,ade,ac3,d4,b3,adf,f4,e10"
	  partition(f -> first degree f, flatten entries gens I)
     	  ///,
     SeeAlso => {tally, partitions}
     }

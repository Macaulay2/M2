--- status: Draft
--- author(s): Giulio 
--- notes: 

document { 
     Key => {topComponents},
     Headline => "compute top dimensional component",
     "The method used is that of Eisenbud-Huneke-Vasconcelos, in their 1993 Inventiones Mathematicae paper.",
     PARA{},
     "If M is a module in a polynomial ring R, then the implementations of 
     ", TO "topComponents", " and ", TO "removeLowestDimension", " are based on 
     the following observations:",
     UL {
	  TEX ///$codim Ext^d(M,R) \ge d$ for all d///,
	  TEX ///If $P$ is an associated prime of $M$ of codimension $d := codim P > codim M$,
	  then $codim Ext^d(M,R) = d$ and the annihilator of $Ext^d(M,R)$ is contained
	  in $P$///,
	  TEX ///If $codim Ext^d(M,R) = d$, then there really is an associated prime 
	  of codimension $d$.///,
	  TEX ///If $M$ is $R/I$, then $topComponents(I) = ann Ext^c(R/I,R)$, where $c = codim I$///
	  },
    SeeAlso => {"removeLowestDimension", "MinimalPrimes :: saturate", "MinimalPrimes :: quotient", "radical", "component example"},
    }

document { 
     Key => (topComponents,Ideal),
     Usage => "topComponents I",
     Inputs => {"I"
	  },
     Outputs => {
	  "I" => Ideal => {"which is the intersection of the primary components of " 
	       ,TT "I", " having the greatest Krull dimension" }
	  },
    "The method used is that of Eisenbud-Huneke-Vasconcelos. ", 
    "For a brief description see: ",TO "topComponents",".", 
    
     EXAMPLE {
	  "R=ZZ/32003[a..c];",
	  "I=intersect(ideal(a,b),ideal(b,c),ideal(c,a),ideal(a^2,b^3,c^4));",
	  "topComponents I" 
	  },
     SeeAlso => {(topComponents, Module),"removeLowestDimension", "MinimalPrimes :: saturate", "MinimalPrimes :: quotient", "radical", "component example"}
     },

document { 
     Key => (topComponents,Module),
     Usage => "topComponents M",
     Inputs => {
	  "M"
	  },
     Outputs => {
	  "N" => Module => {" which is the intersection of the 
	       primary components of ", TT "M", 
	       " having gretest Krull dimension"} 
	  },
    "The method used is that of Eisenbud-Huneke-Vasconcelos. ", 
    "For a brief description see: ",TO "topComponents",".", 
     
     SeeAlso => {(topComponents,Ideal),"removeLowestDimension", "MinimalPrimes :: saturate", "MinimalPrimes :: quotient", "radical","component example"}
     }








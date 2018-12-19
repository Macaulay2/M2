-- -*- coding: utf-8 -*-
--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {isWellDefined,(isWellDefined,Matrix),(isWellDefined,RingMap)},
     Headline => "whether a map is well defined",
     Usage => "isWellDefined f",
     Inputs => {
	  "f" => ofClass{Matrix, RingMap}
	  },
     Outputs => {
	  Boolean => {"whether ", TT "f", " is a well-defined map"}
	  },
     "In order to check whether a matrix, whose source module is not free, is well defined,
     then a Gröbner basis computation will probably be required.",
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  f = map(R^1,coker vars R,{{1_R}})
	  isWellDefined f
	  isWellDefined map(coker vars R, R^1, {{1_R}})
	  ///,
     "In order to check whether a ring map is well defined,
     it is often necessary to check that the image of an ideal 
     under a related ring map is zero.  This often requires
     a Gröbner basis as well.",
     EXAMPLE lines ///
     	  A = ZZ/5[a]
	  factor(a^3-a-2)
     	  B = A/(a^3-a-2);
	  isWellDefined map(A,B)
	  isWellDefined map(B,A)
          ///,
     SeeAlso => {
	  map
	  }
     }

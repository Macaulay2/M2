document {
    Key => contract,
    Headline => "contract one matrix by another",
    SeeAlso => {"diff and contract", contract'}
}

document {
     Key => {
	  (contract,Matrix,Matrix),(contract,RingElement,RingElement), (contract,Vector,RingElement),
	  (contract,RingElement,Vector), (contract,Vector,Vector), (contract,Matrix,RingElement),
	  (contract,RingElement,Matrix), (contract,Vector,Matrix), (contract,Matrix,Vector),
	  (contract,Number,RingElement), (contract,RingElement,Number), (contract,Number,Number),
	  (contract,Number,Vector), (contract,Vector,Number), (contract,Number,Matrix),
	  (contract,Matrix,Number)},
     Headline => "contract a matrix by a matrix",
     Usage => "h = contract(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  "h" => {"the contraction of ", TT "n", " by ", TT "m", ", a matrix with the shape ", TT "h : dual F ** G <--- dual P ** Q", ",
	       whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, " is the result of contracting
	       ", TT { "n", SUB "j,l" }, ", by ", TT {"m", SUB "i,k"}
		}
	   },
     "The arguments can also be ring elements or vectors.",
     EXAMPLE lines ///
	  R = ZZ[x,y,z]
	  f = vars R ** vars R
	  contract(transpose vars R, f)
	  contract(x, f)
	  contract(y, f)
	  contract(z, f)
     ///,
     PARA{},
     "This function is identical to ", TO (diff,Matrix,Matrix), ", except that
     the multiplication by integers that occurs during differentiation is
     omitted.",
     PARA{},
     SeeAlso => {contract', "diff and contract"}
     }

document {
     Key => {(contract', Matrix, Matrix), contract'},
     Headline => "contract a matrix by a matrix, the dual notion",
     Usage => "h = contract'(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  "h" => {"a matrix with the shape ", TT "h : F ** dual G <--- P ** dual Q", ", whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of contracting ", TT { "m", SUB "i,k" }, ", by ", TT {"n", SUB "j,l", "."}}},
     PARA{},
     "This function is identical to ", TO (diff',Matrix,Matrix), ", except that
     the multiplication by integers that occurs during differentiation is
     omitted.",
     PARA{},
     SeeAlso => {contract,"diff and contract"}
     }

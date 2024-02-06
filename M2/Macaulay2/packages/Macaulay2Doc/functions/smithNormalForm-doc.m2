document {
     Key => {(smithNormalForm,Matrix),
	  smithNormalForm,
	  [smithNormalForm,ChangeMatrix],
	  [smithNormalForm,KeepZeroes]
	  },
     Headline => "smith normal form for a matrix over ZZ or a PID",
     Usage => "(D,P,Q) = smithNormalForm M\n(D,P) = smithNormalForm(M,ChangeMatrix=>{true,false})\n(D,Q) = smithNormalForm(M,ChangeMatrix=>{false,true})\nD = smithNormalForm(M,ChangeMatrix=>{false,false})\n",
     Inputs => {
	  "M",
	  ChangeMatrix => List => {"of two Boolean elements.
	  This determines whether the change of basis matrices ", TT "P", " and/or ", TT "Q", " are computed"},
	  KeepZeroes => Boolean => "whether to keep rows and columns that are completely zero"
	  },
     Outputs => {
	  "D" => Matrix => {"The Smith normal form of ", TT "M"},
	  "P" => Matrix => "invertible (left) change of basis matrix",
	  "Q" => Matrix => "invertible (right) change of basis matrix"
	  },
     "This function produces a diagonal matrix ", TT "D", ", and invertible matrices ", TT "P", " and ", TT "Q", " such that
     ", TT "D = PMQ", ".  Warning: even though this function is called the Smith normal form, it doesn't necessarily satisfy the
     more stringent condition that the diagonal entries ", TT "d1, d2, ..., dn", " of ", TT "D", " satisfy: ", TT "d1|d2|...|dn.", ".",
     EXAMPLE lines ///
	 M = matrix{{1,2,3},{1,34,45},{2213,1123,6543},{0,0,0}}
	 (D,P,Q) = smithNormalForm M
	 D == P * M * Q
	 (D,P) = smithNormalForm(M, ChangeMatrix=>{true,false})
	 D = smithNormalForm(M, ChangeMatrix=>{false,false}, KeepZeroes=>true)
     ///,
     PARA{
	  "This function is the underlying routine used by ", TO minimalPresentation,
	  " in the case when the ring is ", TO ZZ, ", or a polynomial ring in one variable over a field."},
     EXAMPLE lines ///
	 prune coker M
     ///,
     "In the following example, we test the result be checking that the entries of ", TT "D1, P1 M Q1", " are the same.
     The degrees associated to these matrices do not match up, so a simple test of equality would return false.",
     EXAMPLE lines ///
	  S = ZZ/101[t]
	  D = diagonalMatrix{t^2+1, (t^2+1)^2, (t^2+1)^3, (t^2+1)^5}
	  P = random(S^4, S^4)
	  Q = random(S^4, S^4)
	  M = P*D*Q
	  (D1,P1,Q1) = smithNormalForm M;
	  D1 - P1*M*Q1 == 0
	  prune coker M
     ///,
     "This routine is under development.  The main idea is to compute a Gröbner basis, transpose the generators, and repeat, until
     we encounter a matrix whose transpose is already a Gröbner basis.  This may depend heavily on the monomial order.",
     Caveat => "The Smith normal form itself is NOT returned! This function is under development,
     and its performance might need to be improved.  Also, this function
       doesn't warn the user if the ring is not a PID.",
     SeeAlso => {(minimalPresentation,Module)}
     }

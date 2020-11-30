newPackage(
     "NumericalLinearAlgebra",
     -- PackageExports => {"NAGtypes"},
     Version => "0.2", 
     Date => "Dec 2020",
     Authors => {{Name => "Robert Krone", 
    	       Email => "krone@math.gatech.edu"}},
     Headline => "numerically compute local dual space and Hilbert functions",
     Keywords => {"Numerical Linear Algebra"},
     --PackageImports => {"Truncations"},
     AuxiliaryFiles => false
     --AuxiliaryFiles => true
)

export{
    "Tolerance", 
    "Normalize",
    "numericalKernel",
    "numericalImage",
    "colReduce",
    "basisIndices"
    }

-- Default tolerance value respectively for exact fields and inexact fields
defaultT = R -> if precision 1_R == infinity then 0 else 1e-6;
getTolerance = true >> opts -> R -> if not opts.?Tolerance or opts.Tolerance === null then defaultT(R) else opts.Tolerance;

numericalKernel = method(Options => {Tolerance => null})
numericalKernel (Matrix) := Matrix => o -> M -> (
    R := ring M;
    tol := getTolerance(R,o);
    (m,n) := (numrows M, numcols M);
    if m == 0 then return id_(source M);
    if n == 0 then return map(R^0,R^0,0);
    (S,U,Vh) := SVD M;
    cols := positions(S, sv->(sv > tol));
    K := submatrix'(transpose Vh,,cols);
    if K == 0 then K else conjugate K
    )

--performs Gaussian reduction on M
colReduce = method(Options => {Tolerance => null, Normalize => true, Reverse => false})
colReduce Matrix := o -> M -> (
    if o.Reverse then M = matrix reverse(entries M);
    tol := getTolerance(ring M,o);
    if tol == 0 then M = gens gb M
    else (
    	M = mutableMatrix sub(M, ultimate(coefficientRing, ring M));
    	(m,n) := (numrows M, numcols M);
    	j := 0; --column of pivot
    	for i in reverse(0..m-1) do (
	    if debugLevel >= 1 then <<i<<"/"<<m-1<<endl;
	    if j >= n then break;
	    a := j + maxPosition apply(j..n-1, l->(abs M_(i,l)));
	    c := M_(i,a);
	    if abs c <= tol then (for k from j to n-1 do M_(i,k) = 0; continue);
	    columnSwap(M,a,j);
	    if o.Normalize then (columnMult(M,j,1/c); c = 1);
	    for k from 0 to n-1 do if k != j then columnAdd(M,k,-M_(i,k)/c,j);
	    j = j+1;
	    );
    	M = (new Matrix from M)_{0..j-1};
    	if precision M < infinity then M = clean(tol,M);
	);
    if o.Reverse then M = matrix reverse(entries M);
    M
    )

TEST ///
N = matrix {{0.001, 0, 0}, {1, 1, 3}, {2, 2, 5.999}}
N = colReduce(N, Tolerance=>0.01)
assert(numcols N == 1)
///

--a list of column indices for a basis of the column space of M
basisIndices = (M, tol) -> (
    M = new MutableMatrix from sub(M, coefficientRing ring M);--sub(M, ultimate(coefficientRing, ring M));
    (m,n) := (numrows M, numcols M);
    i := 0; --row of pivot
    I := new MutableList;
    for j from 0 to n-1 do (
	if i == m then break;
	a := if tol > 0 then i + maxPosition apply(i..m-1, l->(abs M_(l,j)))
	else i + position(i..m-1, l -> M_(l,j) != 0);
	c := M_(a,j);
	if tol > 0 and abs c <= tol then continue;
	I#(#I) = j;
	rowSwap(M,a,i);
	for l from 0 to n-1 do M_(i,l) = M_(i,l)/c; --rowMult(M,i,1/c); is bugged
	for k from 0 to m-1 do rowAdd(M,k,-M_(k,j),i);
	i = i+1;
	);
    new List from I
    )


numericalImage = method(Options => {Tolerance => null})
numericalImage Matrix := o -> M -> (
    R := ultimate(coefficientRing, ring M);
    tol := getTolerance(R,o);
    numericalImage(M,tol)
    )
numericalImage (Matrix, Number) := o -> (M, tol) -> (
    R := ultimate(coefficientRing, ring M);
    M = sub(M, R);
    if numcols M == 0 then return M;
    if numrows M == 0 then return map(R^0,R^0,0);
    if precision 1_(ring M) < infinity then (
	(svs, U, Vt) := SVD M;
	cols := positions(svs, sv->(sv > tol));
	submatrix(U,,cols)
	) else (
	gens image M
	)
    )

TEST ///
M = matrix {{0.999, 2}, {1, 2}}
Mimage = numericalImage(M, 0.01)
assert(numcols Mimage == 1)
///

beginDocumentation()

doc ///
Key
  NumericalLinearAlgebra
Headline
  numerical linear algebra
Description
  Text
    This package collects implementations of numerical linear algebra algorithms.
    @UL {
	{TO numericalKernel},
	{TO numericalImage},
	{TO colReduce}
	}@
///


doc ///
Key 
  Tolerance
Headline
  the tolerance of a numerical computation
///
doc ///
Key 
  "Tolerance(NumericalLinearAlgebra)"
  [numericalKernel,Tolerance]	 
  [colReduce, Tolerance]
  [numericalImage,Tolerance]
Headline
  the tolerance of a numerical computation
Description
  Text 
    The default value {\tt null} sets tolerance to 1e-6.
///

doc ///
     Key
          numericalKernel
	  (numericalKernel,Matrix)
     Headline
          approximate kernel of a matrix
     Usage
          V = numericalKernel(M)
     Inputs
	  M:Matrix
     Outputs
          V:Matrix
     Description
          Text
	       Computes the kernel of a matrix M numerically using singular value decomposition.
	  Example
	       M = matrix {{1., 1, 1}}
	       numericalKernel(M, Tolerance=>0.01)
	  Text
	       Singular values less than the tolerance are treated as zero.
	  Example
	       M = matrix {{1., 1}, {1.001, 1}}
	       numericalKernel(M, Tolerance=>0.01)
///

doc ///
Key
  colReduce
  (colReduce,Matrix)
  [colReduce,Reverse]
  [colReduce,Normalize]
  Normalize
Headline
  column reduce a matrix
Usage
  N = colReduce M
Inputs
  M:Matrix
Outputs
  N:Matrix
    in reduced column echelon form
Description
  Text
    Performs Gaussian column reduction on a matrix M, retaining only the linearly independent columns.
  Example
    M = matrix {{1., 2, 3}, {2, 4, 0}, {-1, -2, 3}}
    colReduce(M, Tolerance=>0.01) 
  Text
    Entries with absolute value below the tolerance are treated as zero and not used as pivots.
  Example
    N = matrix {{0.001, 0, 0}, {1, 1, 3}, {2, 2, 5.999}}
    colReduce(N, Tolerance=>0.01)
  Text
    The lower rows are treated as the lead terms unless the optional argument {\tt Reverse} is set to true.
  Example
    colReduce(M, Reverse=>true)
  Text
    If the optional argument {\tt Normalize} is set to true (default) each vector is normalized so that the lead entry is 1.  Otherwise this step is skipped.
  Example
    colReduce(M, Normalize=>false)
///

doc ///
     Key
          numericalImage
	  (numericalImage,Matrix,Number)
	  (numericalImage,Matrix)
     Headline
          Image of a matrix
     Usage
          V = numericalImage(M, tol)
     Inputs
	  M:Matrix
	  tol:Number
	       a positive number, the numerical tolerance
     Outputs
          V:Matrix
     Description
          Text
	       Computes the image of a matrix M numerically using singular value decomposition.
	  Example
	       M = matrix {{1., 0, 1}, {0, 1, 1}, {1, 0, 1}}
	       numericalImage(M, 0.01)
	  Text
	       Singular values less than the tolerance are treated as zero.
	  Example
	       M = matrix {{0.999, 2}, {1, 2}}
	       numericalImage(M, 0.01)
///


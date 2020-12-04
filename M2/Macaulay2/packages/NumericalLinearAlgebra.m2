newPackage(
    "NumericalLinearAlgebra",
    Version => "1.16", 
    Date => "Dec 2020",
    Authors => {
        {Name => "Robert Krone", 
            Email => "krone@math.gatech.edu"},
	{Name => "Marc Harkonen", 
            Email => "harkonen@gatech.edu"},
    	{Name => "Anton Leykin",
            Email => "anton.leykin@gmail.com"}
    	},
    Headline => "numerically compute local dual space and Hilbert functions",
    Keywords => {"Numerical Linear Algebra"},
    PackageExports => {"LLLBases"},
    AuxiliaryFiles => false
)

export{
    "Tolerance", 
    "Normalize",
    "numericalKernel",
    "numericalRank", "isFullNumericalRank",
    "numericalImage",
    "colReduce"
    }

-- Default tolerance value respectively for exact fields and inexact fields
defaultT = R -> if precision 1_R == infinity then 0 else 1e-6;
getTolerance = true >> opts -> R -> if not opts.?Tolerance or opts.Tolerance === null then defaultT(R) else opts.Tolerance;

-- conjugate all entries of the matrix (should be a part of M2!!!)
conjugate Matrix := Matrix =>M -> matrix(entries M / (row->row/conjugate))
-- installPackage doesn't complain about the absence of conjugate

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

numericalRank = method(Options=>{Threshold=>1e-4})
numericalRank Matrix := o -> M -> (
     if not member(class ring M, {RealField,ComplexField}) 
     then error "matrix with real or complex entries expected";
     t := o.Threshold; 
     N := if t<=1 then M -- use t as an absolute cutoff for singular values, otherwise look for a "gap"  
          else matrix apply(entries M, row->(      -- nomalize "large" rows (a hack!!!)
	     	  m := max(row/abs);
	     	  if m<1 then row else apply(row,e->e/m)
	     	  ));
     S := first SVD N;
     r := 0; last's := 1;
     for i to #S-1 do (
	 if t>1 then (if t*S#i < last's 
	     then break
	     else (r = r + 1; last's = S#i)
	     )
	 else (
	     if S#i>t then r = r + 1
	     else break 
	     )
	 );
     r 
     )  

isFullNumericalRank = method(Options=>{Threshold=>1e-4}) 
isFullNumericalRank Matrix := o -> M -> (
    r := numericalRank(M,o);
    r == min(numColumns M, numRows M) 
    )

TEST ///
N = matrix {{0.001, 0, 0}, {1, 1, 3}, {2, 2, 5.999}}
assert(numericalRank(N,Threshold=>0.01) == 1)
assert not isFullNumericalRank(N,Threshold=>0.001)
assert isFullNumericalRank(N,Threshold=>0.00001)
///


TEST ///
N = matrix {{0.001, 0, 0}, {1, 1, 3}, {2, 2, 5.999}}
K = numericalKernel(N, Tolerance=>0.001)
assert(numcols K == 2)
assert(norm(N*K) < 0.001)
///

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
     	{TO numericalRank},
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

document {
	Key => {numericalRank, (numericalRank, Matrix), [numericalRank, Threshold],
	    isFullNumericalRank, (isFullNumericalRank,Matrix)},
	Headline => "numerical rank of a matrix",
	Usage => "r = numericalRank M\nB = isFullNumericalRank M",
	Inputs => { 
	    "M"=>Matrix=>"a matrix with real or complex entries"
	     },
	Outputs => {
	    "r"=>ZZ, 
	    "B"=>Boolean
	    },
	PARA {
	    TO numericalRank, " finds an approximate rank of the matrix ", TT "M", "."
	    },
	PARA {
	    TO isFullNumericalRank, " = ", TT "M", " is _not_ rank-deficient."
	    },
	PARA {
	    "Let ", TEX "\\sigma_1,...,\\sigma_n", " be the singular values of ", TT "M", ". "
	    },
	PARA {
	    "If ", TO "Threshold", " is >1, then to establish numerical rank we look 
	    for the first large gap between two consecutive singular values. ",
	    "The gap between ", TEX "\\sigma_i", " and ", TEX "\\sigma_{i+1}", 
	    " is large if ", TEX "\\sigma_i/\\sigma_{i+1} > ", TO "Threshold",
	    "."
	    },
	PARA {
	    "If ", TO "Threshold", " is <=1, then the rank equals 
	    the number of singular values larger then ", TO "Threshold", "." 
	    },
	Caveat => {"We assume ", TEX "\\sigma_0=1", " above."},
        EXAMPLE lines ///
options numericalRank
numericalRank matrix {{2,1},{0,0.001}}
numericalRank matrix {{2,1},{0,0.0001}}
     	///,
     	SeeAlso => {SVD}
	}

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


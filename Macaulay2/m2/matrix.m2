--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

ModuleMap = new Type of MutableHashTable
Matrix = new Type of ModuleMap
ring Matrix := f -> (
     S := ring target f;
     R := ring source f;
     if R === S then R
     else error "expected module map with source and target over the same ring"
     )
document { quote Matrix,
     TT "Matrix", " -- the class of all matrices for which Groebner basis operations
     are available from the ", TO "engine", ".",
     PARA,
     "A matrix is a map from a graded module to a graded module, see ",
     TO "Module", ".  The degree of the map is not necessarily 0, and may
     be obtained with ", TO "degree", ".",
     PARA,
     "Multiplication of matrices corresponds to composition of maps, and 
     when f and g are maps so that the target Q of g equals the source P of f,
     the product f*g is defined, its source is the source of g, and its target
     is the target of f.  The degree of f*g is the sum of the degrees
     of f and of g.  The product is also defined when P != Q, provided only that
     P and Q are free modules of the same rank.  If the degrees of P differ
     from the corresponding degrees of Q by the same degree d, then the degree
     of f*g is adjusted by d so it will have a good chance to be homogeneous,
     and the target and source of f*g are as before.",
     PARA,
     "If h is a matrix then h_j is the j-th column of the matrix, and 
     h_j_i is the entry in row i, column j.  The notation h_(i,j) can be
     used as an abbreviation for h_j_i, allowing row and column indices
     to be written in the customary order.",
     PARA,
     "If ", TT "m", " and ", TT "n", " are matrices, ", TT "a", " is a ring element, 
     and ", TT "i", " is an integer, then 'm+n', 'm-n', '-m', 'm n', 'a m', 
     and 'i m' denote the usual matrix arithmetic.  'm == n', and 'm == 0' are used 
     to check equality of matrices.",
     PARA,
     "Operations which produce matrices:", 
     MENU {
	  TO "flip",
          (TO "genericMatrix", "(R,x,r,c) -- an r by c generic matrix"),
          (TO "genericSkewMatrix", "(R,x,r) -- an r by r generic skew matrix"),
          (TO "genericSymmetricMatrix", "(R,x,r) -- an r by r generic symmetric matrix"),
	  (TO "id", "_F         -- identity map F <--- F"),
	  (TO "matrix", "       -- create a matrix"),
	  (TO "map", "          -- create a map of modules"),
	  (TO "random", "(F,G)  -- a random graded matrix F <-- G")
	  },
     "Operations on matrices:",
     MENU {
	  TO "==",
	  TO "!=",
	  TO "+",
	  TO "-",
	  TO "*",
	  TO "^",
	  TO "%",
	  TO "//",
	  (TO "f_(i,j)", " -- getting an entry"),
	  (TO "f_{i,j}", " -- extracting or permuting columns"),
	  (TO "f|g", " -- horizontal concatenation"),
	  (TO "||", " -- vertical concatenation"),
	  ("m ", TO "++", " n   -- direct sum"),
	  (TO "Matrix ** Matrix", " -- tensor product of matrices"),
	  (TO "Matrix ** Module", " -- tensor product, degree shifting"),
	  TO ":",
	  (TO "substitute", "         -- evaluation of a matrix of polynomials"),
	  TO "adjoint",
	  TO "adjoint1",
	  TO "ambient",
	  (TO "borel", " m      -- smallest Borel submodule containing lead
		monomials of m"),
	  TO "codim",
	  TO "complement",
	  (TO "compress", " -- removal of zero columns"),
	  TO "content",
	  (TO "contract", "(m,n) -- contraction of n by m (i.e. diff without 
                          the coefficients)"),
	  TO "degree",
	  (TO "det", "       -- determinant"),
	  (TO "diff", "(m,n) -- differentiation of n by m"),
	  (TO "divideByVariable", " -- divide columns by a variable repeatedly"),
	  TO "dual",
	  TO "selectInSubring",
	  (TO "entries", " m -- the entries of m"),
	  (TO "exteriorPower", "(i,m) -- exterior power of m"),
          (TO "flatten", " m -- the one row matrix with the entries of m"),
	  TO "poincare",
          (TO "isHomogeneous", " m -- whether the matrix m is graded"),
	  (TO "isInjective", " m -- whether a map is injective"),
          (TO "isIsomorphism", " m -- whether the map m is an isomorphism"),
	  (TO "isSurjective", " m -- whether a map is surjective"),
	  (TO "isWellDefined", " m -- whether a map is well-defined"),
	  (TO "homogenize", " m -- homogenize the marix m"),
	  (TO "jacobian", " m   -- Jacobian matrix of m"),
	  (TO "koszul", "(i,m)  -- i-th Koszul matrix of m"),
	  (TO "basis", "(deg,m) -- k-basis of a module in a given degree"),
          (TO "leadTerm", " m -- the lead monomial matrix of the columns of m"),
          (TO "leadTerm", "(i,m) -- the lead terms w.r.t the first i 
                           weight vectors of m"),
 	  (TO "minors", "(i,m)  -- ideal of i by i minors of m"),
	  TO "modulo",
	  (TO "pfaffians", "(i,m) -- ideal of i by i Pfaffians of the skew
		symmetric matrix m"),
	  TO "reshape",
          (TO "ring", " m -- the base ring of the matrix m"),
	  TO "singularLocus",
          (TO "source", " m -- the source freemodule (i.e., the columnspace) of m"),
 	  (TO "submatrix", "(m,rows,cols)  -- extract a submatrix"),
	  (TO "symmetricPower", "(i,m)    -- i-th symmetric power of m"),
          (TO "target", " m -- the target free module (i.e., the rowspace) of m"),
	  TO "top",
	  TO "topCoefficients",
	  (TO "trace", "        -- trace"),
 	  (TO "transpose", " m  -- transpose a matrix")
	  },
     PARA,
     "Operations which produce modules:",
     MENU {
	  (TO "cokernel", " m -- the cokernel of the matrix m"),
	  TO "homology",
	  TO "image",
	  TO "kernel",
	  (TO "kernel", " m -- the kernel of the matrix m"),
	  TO "submodule",
	  TO "subquotient"
	  },
     "Operations which produce Groebner bases from matrices:",
     MENU {
	  TO "gb",
	  TO "mingens",
	  TO "syz"
	  }
     }

document { "Matrix ** Matrix",
     TT "f ** g", " -- computes the tensor product of two matrices.",
     PARA,
     SEEALSO "Matrix"
     }

document { quote gcdDegree,
     TT "gcdDegree F", " -- I don't know what this is supposed to do.",
     }
document { quote lcmDegree,
     TT "lcmDegree F", " -- I don't know what this is supposed to do.",
     }

local newMatrix				  -- defined below

reduce := (tar) -> (
     if not isFreeModule tar then (
	  g := gb presentation tar;
	  sendgg(ggPush g, ggPush 1, ggpick, ggreduce, ggpop);
	  ))

ZZ * Matrix := (i,m) -> (
     sendgg(ggPush ring m, ggPush i, ggfromint, ggPush m, ggmult);
     T := target m;
     reduce T;
     newMatrix(T, source m))

Matrix * ZZ := (m,i) -> (
     sendgg(ggPush ring m, ggPush i, ggfromint, ggPush m, ggmult);
     T := target m;
     reduce T;
     newMatrix(T, source m))

RingElement * Matrix := (r,m) -> (
     R := ring r;
     if R =!= ring m then error "scalar not in ring of matrix";
     sendgg (ggPush r, ggPush m, ggmult);
     T := target m;
     reduce T;
     newMatrix(T, source m))

newMatrix = (tar,src) -> (
     R := ring tar;
     p := new Matrix;
     p.source = src;
     p.target = tar;
     p.handle = newHandle "";
     p)

getMatrix = (R) -> newMatrix(
     (sendgg(ggdup,gggetrows); new Module from R),
     (sendgg(ggdup,gggetcols); new Module from R)
     )

document { quote getMatrix,
     TT "getMatrix R", " -- pops a matrix over ", TT "R", " from the top of 
     the engine's stack and returns it."
     }

BinaryMatrixOperation := (operation) -> (m,n) -> (
     if ring m =!= ring n then (
	  try m = promote(m,ring n)
	  else try n = promote(n,ring m)
	  else error "expected matrices over compatible rings");
     sendgg (ggPush m, ggPush n, operation);
     getMatrix ring m)

BinaryMatrixOperationSame := (operation) -> (m,n) -> (
     -- same source and target
     if ring m =!= ring n then (
	  try m = promote(m,ring n)
	  else try n = promote(n,ring m)
	  else error "expected matrices over compatible rings");
     sendgg (ggPush m, ggPush n, operation);
     T := target m;
     reduce T;
     newMatrix(T, source m))

Matrix _ Sequence := (m,ind) -> (
     if # ind === 2
     then (
     	  R := ring m;
	  rows := numgens target m;
	  cols := numgens source m;
	  i := ind#0;
	  j := ind#1;
	  if i < 0 or i >= rows then error (
	       "encountered row index ", name i,
	       " out of range 0 .. ", name (rows-1));
	  if j < 0 or j >= cols then error (
	       "encountered column index ", name j,
	       " out of range 0 .. ", name (cols-1));
     	  sendgg (ggPush m, ggINT, gg ind#0, ggINT, gg ind#1, ggelem);
     	  R.pop())
     else error "expected a sequence of length two"
     )
document { "f_(i,j)",
     TT "f_(i,j)", " -- provide the element in row i and column j of the matrix f.",
     SEEALSO ("_", "Matrix")
     }

Matrix _ ZZ := (m,i) -> (
     if 0 <= i and i < numgens source m then (
     	  sendgg (ggPush m, ggPush i, ggelem);
     	  new m.target)
     else error ("subscript '", name i, "' out of range"))
document { "f_i",
     TT "f_i", " -- provide the i-th column of a matrix f as a vector.",
     SEEALSO "_"
     }

Matrix == Matrix := (m,n) -> (
     target m == target n
     and source m == source n
     and (
     	  sendgg (ggPush m, ggPush n, ggisequal); 
     	  eePopBool()))
Matrix == RingElement := (m,f) -> m == f*id_(target m)
RingElement == Matrix := (f,m) -> m == f*id_(target m)

Matrix == ZZ := (m,i) -> (
     if i === 0 then ( sendgg(ggPush m, ggiszero); eePopBool())
     else if i === 1 then ( source m == target m and m == id_(target m) )
     else ( source m == target m and m == i*id_(target m) )
     )
ZZ == Matrix := (i,m) -> m == i

Matrix + Matrix := {Matrix, BinaryMatrixOperationSame ggadd}
Matrix + RingElement := {Matrix, (f,r) -> if r == 0 then f else f + r*id_(target f)}
RingElement + Matrix := {Matrix, (r,f) -> if r == 0 then f else r*id_(target f) + f}
ZZ + Matrix := {Matrix, (i,f) -> if i === 0 then f else i*1_(ring f) + f}
Matrix + ZZ := {Matrix, (f,i) -> if i === 0 then f else f + i*1_(ring f)}

Matrix - Matrix := BinaryMatrixOperationSame ggsubtract
Matrix - RingElement := (f,r) -> if r == 0 then f else f - r*id_(target f)
RingElement - Matrix := (r,f) -> if r == 0 then -f else r*id_(target f) - f
ZZ - Matrix := (i,f) -> if i === 0 then -f else i*1_(ring f) - f
Matrix - ZZ := (f,i) -> if i === 0 then f else f - i*1_(ring f)

- Matrix := f -> (
     h := new Matrix;
     h.source = source f;
     h.target = target f;
     h.handle = newHandle (ggPush f, ggnegate);
     h)

Matrix * Matrix := (m,n) -> (
     if source m == target n then (
	  if ring target m =!= ring target n then (
	       n = matrix n ** ring target m;
	       );
     	  sendgg (ggPush m, ggPush n, ggmult);
	  M := target m;
	  N := source n;
	  reduce M;
	  newMatrix(M,N))
     else (
     	  R := ring m;
	  S := ring n;
	  if R =!= S then (
	       try m = m ** S else
	       try n = n ** R else
	       error "maps over incompatible rings";
	       );
	  M = target m;
	  P := source m;
	  N = source n;
	  Q := target n;
	  if not isFreeModule P or not isFreeModule Q or rank P =!= rank Q
	  then error "maps not composable";
	  dif := degrees P - degrees Q;
	  if same dif then (
	       sendgg (ggPush m, ggPush n, ggmult, 
		    ggdup, ggPush (degree m + degree n + dif#0), ggsetshift);
	       reduce M;
	       newMatrix(M,N))
	  else (
	       sendgg (ggPush m, ggPush n, ggmult, 
		    ggdup, ggPush elements (degreeLength R:0), ggsetshift);
	       reduce M;
	       newMatrix(M,N))))

Matrix ^ ZZ := (f,n) -> (
     if n === 0 then id_(target f)
     else SimplePowerMethod (f,n))

TEST "
R=ZZ/101[a,b]
f=matrix(R,{{1,a},{0,1}})
g=matrix(R,{{1,0},{b,1}})
h=f*g*f*g
assert( h^3 * h^-1 == h^2 * h^0 )
assert( h * h^-1 == 1 )
"

TEST "
R=ZZ/101[a,b]
f = matrix {{a}}
assert( source f != target f)
assert( target f == target f^2 )
assert( source f == source f^2 )
assert( target f == target f^0 )
assert( source f != source f^0 )
"

transpose Matrix :=  (m) -> (
     if not (isFreeModule source m and isFreeModule target m) 
     then error "expected a map between free modules";
     sendgg (ggPush m, ggtranspose);
     f := getMatrix ring m;
     map(dual source m, dual target m, f, Degree => - degree m))

ring(Matrix) := m -> (
     R := m.source.ring;
     if R =!= m.target.ring
     then error "expected map to have source and target over same ring";
     R)

Matrix * Vector := (m,v) -> (
     if class v =!= source m then error "map not applicable to vector";
     if not isFreeModule source m then notImplemented();
     sendgg(ggPush m, ggPush v, ggmult);
     new m.target)

expression Matrix := m -> (
     rows := numgens target m;
     cols := numgens source m;
     new MatrixExpression from applyTable(entries m, expression)
     )

name Matrix := m -> concatenate (
     -- "matrix (", name target m, ", ", name source m, ", ", name entries m, ")"
     "matrix ", name entries m
     )

isIsomorphism Matrix := f -> coker f == 0 and ker f == 0

isHomogeneous Matrix := m -> (
     if m.?isHomogeneous then m.isHomogeneous else m.isHomogeneous = (
     	  M := source m;
     	  N := target m;
	  (sendgg(ggPush m, ggishomogeneous); eePopBool())
	  and
	  ( not M.?generators or isHomogeneous M.generators )
	  and
	  ( not N.?generators or isHomogeneous N.generators )
	  ))

isWellDefined Matrix := f -> (
     matrix f * presentation source f % presentation target f == 0
     )

document { quote isWellDefined,
     TT "isWellDefined m", " -- tells whether a map m of modules is 
     well-defined."
     }

ggConcatCols := (tar,src,mats) -> (
     sendgg(apply(mats,ggPush), ggPush (#mats), ggconcat);
     newMatrix(tar,src))

ggConcatRows := (tar,src,mats) -> (
     sendgg(
	  apply(mats,m -> (ggPush m, ggtranspose)), 
	  ggPush (# mats), ggconcat, ggtranspose
	  );
     newMatrix(tar,src))

samering := mats -> (
     R := ring mats#0;
     if not all ( mats, m -> ring m === R )
     then error "expected matrices over the same ring";
     )

directSum Matrix := identity

Matrix.directSum = args -> (
     R := ring args#0;
     if not all(args, f -> ring f === R) 
     then error "expected matrices all over the same ring";
     sendgg(apply(args, ggPush), ggPush (#args), ggdirectsum);
     f := newMatrix(directSum apply(args,target),directSum apply(args,source));
     f.components = elements args;
     f)

isDirectSum = method()
isDirectSum Module := (M) -> M.?components
document { quote isDirectSum,
     TT "isDirectSum M", " -- returns ", TT "true", " if ", TT "M", " was
     formed as a direct sum.",
     PARA,
     "Works for modules, graded modules, etc.  The components of the sum
     can be recovered with ", TO "components", "."
     }

TEST "
assert isDirectSum (QQ^1 ++ QQ^2)
assert isDirectSum (QQ^1 ++ QQ^2)
"

components Module := M -> if M.?components then M.components else {M}
components Matrix := f -> if f.?components then f.components else {f}

Module.directSum = args -> (
	  R := ring args#0;
	  if not all(args, f -> ring f === R) 
	  then error "expected modules all over the same ring";
	  N := if all(args, M -> not M.?generators) 
	  then (
	       if all(args, M -> not M.?relations) 
	       then R ^ (- join unlist apply(args, degrees))
	       else subquotient( null, directSum apply(args,relations) )
	       )
	  else (
	       if all(args, M -> not M.?relations) then (
		    subquotient( directSum apply(args,generators), null )
		    )
	       else subquotient(
		    directSum apply(args,generators), 
		    directSum apply(args,relations)));
	  N.components = elements args;
	  N)

single := v -> (
     if not same v 
     then error "incompatible objects in direct sum";
     v#0)

directSum Sequence := args -> (
     if #args === 0 then error "expected more than 0 arguments";
     type := single apply(args, class);
     if type.?directSum then type.directSum args
     else error "no method for direct sum"
     )
directSum List := args -> directSum unlist args
Matrix ++ Matrix := directSum
Module ++ Module := directSum

document { quote directSum,
     TT "directSum(m,n,...)", " -- forms the direct sum of matrices or modules.",
     PARA,
     "The components can be recovered later with ", TO "components", ".",
     SEEALSO ("++", "components")
     }
Matrix ++ RingElement := (f,r) -> f ++ matrix {{r}}
RingElement ++ Matrix := (r,f) -> matrix {{r}} ++ f
RingElement ++ RingElement := (r,s) -> matrix {{r}} ++ matrix {{s}}

concatCols := mats -> (
     mats = select(elements mats,m -> m =!= null);
     if # mats === 1 
     then mats#0
     else (
	  samering mats;
	  targets := unique apply(mats,target);
	  M := targets#0;
	  if not all(targets, F -> F == M) 
	  and not all(targets, F -> isFreeModule F)
	  then error "unequal targets";
	  ggConcatCols(targets#0, Module.directSum apply(mats,source), mats)))

concatRows := mats -> (
     mats = select(elements mats,m -> m =!= null);
     if # mats === 1 
     then mats#0
     else (
	  samering mats;
	  sources := unique apply(mats,source);
	  N := sources#0;
	  if not all(sources, F -> F == N) 
	  and not all(sources, F -> isFreeModule F)
	  then error "unequal sources";
	  ggConcatRows(Module.directSum apply(mats,target), sources#0, mats)))

Matrix | Matrix := (f,g) -> concatCols(f,g)
RingElement | Matrix := (f,g) -> concatCols(f**id_(target g),g)
Matrix | RingElement := (f,g) -> concatCols(f,g**id_(target f))
ZZ | Matrix := (f,g) -> concatCols(f*id_(target g),g)
Matrix | ZZ := (f,g) -> concatCols(f,g*id_(target f))

Matrix || Matrix := (f,g) -> concatRows(f,g)
RingElement || Matrix := (f,g) -> concatRows(f**id_(source g),g)
     -- we would prefer for f**id_(source g) to have the exact same source as g does
Matrix || RingElement := (f,g) -> concatRows(f,g**id_(source f))
ZZ || Matrix := (f,g) -> concatRows(f*id_(source g),g)
Matrix || ZZ := (f,g) -> concatRows(f,g*id_(source f))

listZ := v -> (
     if not all(v,i -> class i === ZZ) then error "expected list of integers";
     )

submatrix(Matrix,List,Nothing) := (m,rows,cols) -> (
     submatrix(m, rows, 0 .. numgens source m - 1)
     )

submatrix(Matrix,Nothing,List) := (m,rows,cols) -> (
     submatrix(m, 0 .. numgens target m - 1, cols)
     )

submatrix(Matrix,Sequence,Sequence) := 
submatrix(Matrix,Sequence,List) := 
submatrix(Matrix,List,Sequence) := 
submatrix(Matrix,List,List) := (m,rows,cols) -> (
     if not isFreeModule source m or not isFreeModule target m
     then error "expected a homomorphism between free modules";
     rows = elements splice rows;
     listZ rows;
     cols = elements splice cols;
     listZ cols;
     sendgg(ggPush m, 
	  ggINTARRAY, gg rows, ggINTARRAY, gg cols, ggsubmatrix);
     getMatrix ring m)

submatrix(Matrix,List) := (m,cols) -> (
     cols = elements splice cols;
     listZ cols;
     sendgg(ggPush m, 
	  ggINTARRAY, gg cols, 
	  ggsubmatrix);
     getMatrix ring m)
     
document { quote submatrix,
     TT "submatrix(m, rows, cols)", " -- yields a submatrix of the matrix ", TT "m", ".",
     BR,NOINDENT,
     TT "submatrix(m, cols)", " -- yields a submatrix of the matrix ", TT "m", ".",
     PARA,
     "Yields an r by c matrix, where r is the length of the list of integers
     ", TT "rows", ", and c is the length of the list of integers ", TT "cols", ".  
     The (i,j)-th entry of the result is m_(rows_i, cols_j).  If necessary, any
     sequences in the lists are spliced into the list.",
     PARA,
     EXAMPLE "R = ZZ/101[a .. o]",
     EXAMPLE "m = genericMatrix(R, a, 3, 5)",
     EXAMPLE "submatrix(m, {1,2,0}, {0..2, 4})",
     PARA,
     "If ", TT "rows", " or ", TT "cols", " is omitted, all the indices are used.",
     EXAMPLE "submatrix(m, {1,2}, )",
     PARA,
     "It is an error if any element of ", TT "rows", " or ", TT "cols", " is out 
     of range."
     }

diff(Matrix, Matrix) := BinaryMatrixOperation ggdiff
diff(RingElement, RingElement) := (f,g) -> (
     (diff(matrix{{f}},matrix{{g}}))_(0,0)
     )
diff(Matrix, RingElement) := (m,f) -> diff(m,matrix{{f}})
diff(RingElement, Matrix) := (f,m) -> diff(matrix{{f}},m)
diff(Vector, RingElement) := (v,f) -> (diff(matrix{v},matrix{{f}}))_0
diff(RingElement, Vector) := (f,v) -> diff(matrix{{f}},transpose matrix{v})
diff(Vector, Vector) := (v,w) -> diff(matrix{v}, transpose matrix{w})
diff(Matrix, Vector) := (m,w) -> diff(m,transpose matrix {w})
diff(Vector, Matrix) := (v,m) -> diff(matrix {v}, m)
document { quote diff,
     TT "diff(m,n)", " -- differentiate the matrix n by the matrix m",
     BR,NOINDENT,
     TT "diff P", " -- compute the difference polynomial for a projective
     Hilbert polynomial, see ", TO "ProjectiveHilbertPolynomial", ".",
     BR,NOINDENT,
     TT "diff(P,i)", " -- compute the i-th difference polynomial for a projective
     Hilbert polynomial, see ", TO "ProjectiveHilbertPolynomial", ".",
     PARA,
     "Given matrices m : F0 <--- F1, and n : G0 <--- G1, produce a matrix
     with the shape diff(m,n) : F0' ** G0 <--- F1' ** G1, whose 
     entry in the slot ((i,j),(k,l)) is the result of differentiating
     n_(j,l) by the differential operator corresponding to m_(i,k).",
     PARA,
     "If ", TT "m", " or ", TT "n", " is a ring element, then it is interpreted
     as a one-by-one matrix.  If ", TT "m", " is a vector, it is interpreted as
     a matrix with one column, and if ", TT "n", " is a vector, it is interpreted
     as a matrix with one row.  If both ", TT "m", " and ", TT "n", " are ring
     elements, then the result will be a ring element rather than a one-by-one
     matrix.  If ", TT "m", " is a vector and ", TT "n", " is a ring element,
     then the result will be a vector rather than a matrix with one column.",
     PARA,
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "m = genericMatrix(R,a,2,2)",
     EXAMPLE "diff(transpose m,m*m)",
     PARA,
     "The most common usage of this function is when m : F <--- R^1
     and n : R^1 <--- G.  In this case the result is a matrix with shape
     diff(m,n) : F' <--- G, and the (i,j) th entry is the result of
     differentiating n_j by the differential operator corresponding to m_i.",
     EXAMPLE "m = matrix {{a,b,c,d}}",
     EXAMPLE "n = matrix {{a^2, (b + c)*(a + d), a*b*c}}",
     EXAMPLE "p = diff(transpose m,n)",
     EXAMPLE "target p",
     EXAMPLE "source p",
     PARA,
     "As another example, we show how to compute the Wronskian of a
     polynomial f.",
     EXAMPLE "R = ZZ/101[a, x .. z]",
     EXAMPLE "f = matrix {{x^3 + y^3 + z^3 - a*x*y*z}}",
     EXAMPLE "v = matrix {{x,y,z}}",
     EXAMPLE "W = diff(transpose v * v, f)",
     EXAMPLE "Wf = minors(3,W)",
     PARA,
     SEEALSO ( "contract", "jacobian" )
     }

contract(Matrix, Matrix) := BinaryMatrixOperation ggcontract
contract(RingElement, RingElement) := (f,g) -> (
     (contract(matrix{{f}},matrix{{g}}))#(0,0)
     )
contract(Matrix, RingElement) := (m,f) -> contract(m,matrix{{f}})
contract(RingElement, Matrix) := (f,m) -> contract(matrix{{f}},m)
contract(Vector, RingElement) := (v,f) -> (contract(matrix{v},matrix{{f}}))_0
contract(RingElement, Vector) := (f,v) -> contract(matrix{{f}},transpose matrix{v})
contract(Vector, Vector) := (v,w) -> contract(matrix{v}, transpose matrix{w})
contract(Matrix, Vector) := (m,w) -> contract(m,transpose matrix {w})
contract(Vector, Matrix) := (v,m) -> contract(matrix {v}, m)
document { quote contract,
     TT "usage: contract(m, n)", " -- contract the matrix n by the matrix m",
     PARA,
     "This function is identical to ", TO "diff", ", except that contraction is
     used instead of differentiation.  This means for example that x^3
     contracted by x^2 is x, not 6 x.  For example, ",
     EXAMPLE "R = ZZ/101[a..c]",
     EXAMPLE "diff(transpose matrix {{a,b,c}}, matrix {{(a+b+c)^3, a^2 * b^3 * c^2}})",
     PARA,
     "As another example, the Sylvester resultant between homogeneous polynomials
     f(x,y) and g(x,y) can be found in the following way.",
     EXAMPLE "R = (ZZ/101[a,b])[x,y]",
     EXAMPLE "f = a * x^3 + b * x^2 * y + y^3",
     EXAMPLE "g = b * x^3 + a * x * y^2 + y^3",
     "Multiply each of these by all quadrics, obtaining a set of elements in
     degree 5:",
     EXAMPLE "n = matrix {{f,g}} ** symmetricPower(2,vars R)",
     "Now create the matrix of coefficients by using contract against all
     monomials of degree 5 in x and y.",
     EXAMPLE "M = contract(transpose symmetricPower(5,vars R), n)",
     EXAMPLE "Resfg = minors(6, M)",
     PARA,
     SEEALSO "diff"
     }

jacobian = method()
jacobian Matrix := (m) -> diff(transpose vars ring m, m)

jacobian Ring := (R) -> jacobian presentation R ** R

TEST "
R = ZZ/101[a..d]
I = monomialCurve(R,{1,3,4})
A = R/I
jacobian A
singA = minors(codim ideal presentation A, jacobian A)
generators gb singA
"
document { quote jacobian,
     TT "jacobian R", " -- calculates the Jacobian matrix of the ring R",
     BR,NOINDENT,
     TT "jacobian f", " -- calculates the Jacobian matrix of the matrix f,
     which will normally be a matrix with one row.",
     BR,NOINDENT,
     TT "jacobian I", " -- compute the matrix of derivatives of the 
     generators of I w.r.t. all of the variables",
     PARA,
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "I = monomialCurve(R,{1,3,4})",
     EXAMPLE "A = R/I",
     EXAMPLE "jacobian A",
     "For a one row matrix, the derivatives w.r.t. all the variables
     is given",
     EXAMPLE "R = ZZ/101[a..c]",
     EXAMPLE "p = symmetricPower(2,vars R)",
     EXAMPLE "jacobian p",
     "Caveat: if a matrix or ideal over a quotient polynomial ring S/J
     is given, then only the derivatives of the given elements are
     computed and NOT the derivatives of elements of J."
     }

leadTerm(ZZ, Matrix) := (i,m) -> (
     sendgg(ggPush m, ggINT, gg i, gginitial);
     getMatrix ring m)
leadTerm(Matrix) := m -> (
     sendgg(ggPush m, gginitial);
     getMatrix ring m)

document { quote leadTerm,
     TT "leadTerm f", " -- return the leading term of the polynomial or 
     vector f.",
     BR, NOINDENT,
     TT "leadTerm m", " -- return the matrix of initial forms of 
     the columns of the matrix m.",
     BR, NOINDENT,
     TT "leadTerm(i,m)", " -- return the matrix of polynomials formed 
     by retaining those monomials of each entry which agree on the first i 
     weight vectors.",
     PARA,
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "leadTerm (3 + 8*a^2*b + 7*b*c^2)",
     EXAMPLE "leadTerm matrix {{a,b},{c,d}}",
     EXAMPLE "leadTerm matrix {{c,d},{a,b}}",
     SEEALSO ("leadCoefficient", "leadMonomial", "leadComponent")
     }

borel Matrix := m -> (
     sendgg (
	  ggPush m, ggINT, gg 0, ggmonideal,  -- get monomial lead ideal
	  ggborel,                            -- monomial borel now on stack
	  ggmatrix);
     getMatrix ring m)
document { quote borel,
  TT "usage: borel m", " -- create a matrix of monomials",
  PARA,
  "Yields the matrix with the same target as the matrix ", TT "m", ", whose columns
   generate the smallest Borel fixed submodule containing the lead monomials
   of the columns of ", TT "m", ".",
  PARA,
  "For example, if R = ZZ/101[a..f], then",
  EXAMPLE "R = ZZ/101[a..e]",
  EXAMPLE "borel matrix {{a*d*e, b^2}}"
  }

--------------------------------------------------------------------------
------------------------ matrix and map for modules ----------------------
--------------------------------------------------------------------------

mopts := Options => {
     Degree => null
     }

matrix = method mopts
map = method mopts

map(Module,Module) := (M,N,options) -> (
     F := ambient N;
     if F == ambient M
     then map(M,N,
	  if M.?generators 
	  then map(M,N,generators N // generators M)
	  else generators N,
	  options)
     else error "expected modules to have the same ambient free module"
     )

TEST "
R = ZZ/101[a..d]
F = R^3
H = subquotient(F_{1,2}, F_{2})
f = map(H,cover H,id_(cover H))
assert( cokernel f == 0 )
assert( kernel f == image R^2_{1} )
assert( isWellDefined f )
assert( isSurjective f )
assert( not isInjective f )
"

map(Module,Module,RingElement) := (M,N,r,options) -> (
     if r == 0 then (
	  R := ring M;
	  f := new Matrix;
	  f.handle = newHandle(ggPush cover M, ggPush cover N, ggzeromat);
	  f.source = N;
	  f.target = M;
	  f.ring = ring M;
	  f)
     else if M == N then map(M,r) 
     else error "expected 0, or same source and target")

map(Module,Module,ZZ) := (M,N,i,options) -> (
     if i === 0 then (
	  R := ring M;
	  f := new Matrix;
	  f.handle = newHandle(ggPush cover M, ggPush cover N, ggzeromat);
	  f.source = N;
	  f.target = M;
	  f.ring = ring M;
	  f)
     else if M == N then map(M,i)
     else error "expected 0, or same source and target")

map(Module,RingElement) := (M,r,options) -> (
     R := ring M;
     try r = r + R#0
     else error "encountered scalar of unrelated ring";
     if r == 0 then map(M,M,0)
     else if r == 1 then map(M,1)
     else r * (map(M,1)))

map(Module) := (M,options) -> (
     R := ring M;
     f := new Matrix;
     sendgg(ggPush cover M, ggiden);
     if options.Degree =!= null 
     then sendgg(ggdup, ggPush options.Degree, ggsetshift);
     reduce M;
     f.handle = newHandle "";
     f.source = f.target = M;
     f)

map(Module,ZZ) := (M,i,options) -> (
     if i === 0 then map(M,M,0)
     else if i === 1 then map(M,options)
     else i * map M)

map(Module,Matrix) := (M,f,options) -> (
     R := ring M;
     if R =!= ring f then error "expected the same ring";
     if # degrees M =!= # degrees target f then (
	  error "expected ambient modules of the same rank";
	  );
     diffs := degrees M - degrees target f;
     if not same diffs then (
	  error "expected to find uniform difference between degrees"
	  );
     map(M,source f ** R^{-first diffs},f)
     )

degreeCheck := (d,R) -> (
     if class d === ZZ then d = {d};
     if class d === List
     and all(d,i -> class i === ZZ) 
     and #d === degreeLength R
     then d
     else (
	  if degreeLength R === 1
	  then error "expected degree to be an integer or list of integers of length 1"
	  else error (
	       "expected degree to be a list of integers of length ",
	       string degreeLength R
	       )
	  )
     )

map(Module,Module,Matrix) := (M,N,f,options) -> (
     if M === f.target and N === f.source then f
     else (
	  R := ring M;
	  sendgg (ggPush cover M, ggPush cover N, ggPush f,
	       ggPush (
		    if options.Degree === null
		    then elements (degreeLength R : 0)
		    else degreeCheck(options.Degree, R)),
	       ggmatrix);
	  reduce M;
	  newMatrix(M,N)))

matrix(Ring,List) := (R,m,options) -> (
     if not isTable m then error "expected a table";
     map(R^#m,,m,options))

map(Module,Module,Function) := (M,N,f,options) -> (
     map(M,N,table(numgens M, numgens N, f))
     )

document { "map(Module,Module,Function)",
     TT "map(M,N,f)", " -- creates a map from the module N to the module M whose
     matrix entries are obtained from the function f by evaluating f(i,j)"
     }

map(Matrix) := (f,options) -> (
     if options.Degree === null then f
     else (
     	  R := ring source f;
	  d := options.Degree;
	  if class d === ZZ then d = {d};
     	  map(target f, source f ** R^{d - degree f}, f, options)))

map(Module,ZZ,Function) := (M,n,f,options) -> map(M,n,table(numgens M,n,f),options)
map(Module,ZZ,List) := (M,rankN,p,options) -> (
     if options.Degree =!= null
     then error "Degree option given with indeterminate source module";
     R := ring M;
     p = apply(splice p,splice);
     if #p != numgens M
     or #p > 0 and ( not isTable p or # p#0 != rankN )
     then error( "expected ", name numgens M, " by ", name rankN, " table");
     p = applyTable(p,x -> promote(x,R));
     m := new Matrix;
     m.target = M;
     coverM := cover M;
     m.handle = newHandle(
	  apply(
	       if # p === 0 then splice {rankN:{}}
	       else transpose p, 
	       col -> {apply(col, r -> ggPush r), ggPush coverM, ggvector}
	       ),
	  ggPush coverM,
	  ggPush rankN,
	  ggmatrix);
     m.source = ( sendgg(ggPush m,gggetcols); new Module from R );
     m)

TEST "
R = ZZ/101[x,y,z]
assert isHomogeneous map(R^2,2,(i,j)->R_j)
assert isHomogeneous map(R^2,5,{{x,y,z,x^2,y^2},{x,0,z,z^2,0}})
"

map(Module,Nothing,Matrix) := (M,nothing,p,options) -> (
     R := ring M;
     coverM := cover M;
     n := numgens cover source p;
     colvectors := apply(n, i -> p_i);
     if options.Degree =!= null
     then error "Degree option given with indeterminate source module";
     m := new Matrix;
     m.target = M;
     m.handle = newHandle( colvectors / ggPush, ggPush coverM, ggPush n, ggmatrix);
     m.source = (sendgg(ggPush m,gggetcols); new Module from R);
     m
     )

map(Module,Nothing,List) := map(Module,Module,List) := (M,N,p,options) -> (
     R := ring M;
     if N === null
     then (
	  k := R;
	  if #p === 0 then error "expected non-empty list of entries for matrix";
	  rankN := #p#0;
	  )
     else (
     	  k = ring N;
     	  try promote(1_k,R) else error "modules over incompatible rings";
	  -- later, allow a ring homomorphism
	  rankN = numgens N;
	  );
     p = apply(splice p,splice);
     if #p != numgens M
     or #p > 0 and ( not isTable p or # p#0 != rankN )
     then error( "expected ", name numgens M, " by ", name rankN, " table");
     p = applyTable(p,x -> promote(x,R));
     m := new Matrix;
     m.target = M;
     coverM := cover M;
     m.handle = newHandle(
	  apply(
	       if # p === 0 then splice {rankN:{}}
	       else transpose p, 
	       col -> {apply(col, r -> ggPush r), ggPush coverM, ggvector}
	       ),
	  ggPush coverM,
	  if N === null
	  then (
	       if options.Degree =!= null
	       then error "Degree option given with indeterminate source module";
	       ggPush rankN
	       )
	  else (
	       ggPush cover N,
	       ggPush (
		    if options.Degree === null
	       	    then elements (degreeLength R:0)
	       	    else degreeCheck(options.Degree,R)
		    )
	       ),
	  ggmatrix);
     m.source = (
     	  if N === null then (sendgg(ggPush m,gggetcols); new Module from R)
     	  else N
	  );
     m)

fixDegree := (m,d) -> (
     M := target m;
     N := source m;
     R := ring M;
     sendgg (
	  ggPush cover M,
	  ggPush cover N,
	  ggPush m, 
	  ggPush degreeCheck(d,R),
	  ggmatrix);
     newMatrix(M,N)
     )

Matrix.matrix = (f,options) -> concatRows apply(f, v -> concatCols v)

matrixTable := (f,options) -> (
     types := unique apply(flatten f, class);
     if # types === 1 then (
	  type := types#0;
	  if instance(type,Ring) then (
	       R := type;
	       map(R^#f,, f, options))
	  else if type.?matrix then type.matrix(f,options)
	  else error "no method for forming a matrix from elements of this type")
     else if all(types, T -> instance(T,Ring)) then (
	  R = ring (
	       try sum apply(types, R -> R#0)
	       else error "couldn't put matrix elements into the same ring"
	       );
	  map(R^#f,,f,options))
     else if all(types, T -> instance(T,Ring) or T === Matrix) then (
	  rings := unique apply(select(flatten f,m -> class m === Matrix), ring);
	  if #rings > 1 then error "matrices over different rings";
	  R = rings#0;
	  f = apply(f, row -> new MutableList from row);
	  m := #f;
	  n := #f#0;
	  tars := new MutableHashTable;
	  srcs := new MutableHashTable;
	  scan(m, i->scan(n, j-> (
			 r := f_i_j;
			 if class r === Matrix then (
			      if tars#?i and tars#i != target r
			      then error "matrices not compatible";
			      tars#i = target r;
			      if srcs#?i and srcs#i != source r
			      then error "matrices not compatible";
			      srcs#j = source r;
			      ))));
	  scan(m, i->scan(n, j-> (
			 r := f_i_j;
			 if instance(class r,Ring) and r != 0 then (
			      r = R#0 + r;
			      d := degree r;
			      if tars#?i then (
				   M := tars#i;
				   if srcs#?j then (
					N := srcs#j;
					if apply(degrees M, e -> e + d) =!= degrees N 
					then error ("matrices not compatible");
					f#i#j = map(M,N,r))
				   else (
					srcs#j = N = M ** R^{-d};
					f#i#j = map(M,N,r)))
			      else (
				   if srcs#?j then (
					N = srcs#j;
					tars#i = M = N ** R^{d};
					f#i#j = map(M,N,r))
				   else (
					tars#i = M = R^1;
					srcs#j = N = R^{-d};
					f#i#j = map(M,N,r)))))));
	  scan(m, i->scan(n, j-> (
			 r := f_i_j;
			 if r == 0 then (
			      if tars#?i then (
				   M := tars#i;
				   if srcs#?j then (
					N := srcs#j;
					f#i#j = map(M,N,0);)
				   else (
					srcs#j = M;
					f#i#j = map(M,M,0); ) )
			      else (
				   if srcs#?j then (
					N = srcs#j;
					tars#i = N;
					f#i#j = map(N,N,0);
					)
				   else (
					M = tars#i = srcs#j = R^1;
					f#i#j = map(M,M,0);
					))))));
	  mm := concatRows apply(f, row -> concatCols row);
	  if options.Degree === null
	  then mm
	  else fixDegree(mm,options.Degree)
	  )
     else error "expected ring elements or matrices")

document { quote matrix,
  TT "matrix(...)", " -- create a matrix.",
  PARA,
  "This function can be used to create a matrix or map (homomorphism) between
  modules, but it is complicated because there are many different ways it can
  be used.  The entries of the matrix can be provided as a list of lists of ring
  elements, or as a function which accepts row and column indices.  The ring of
  the matrix can be provided explicitly, or the source and target modules can be 
  provided.  There are other alternatives.",
  PARA,
  "Various ways to use ", TO "matrix", ".",
  MENU {
       TO "matrix(List)",
       TO "matrix(Matrix)",
       TO "matrix(Ring,List)"
       },
  "Optional arguments, valid with each form above:",
  MENU {
       (TO "Degree", " -- specify the degree of the resulting map."),
       },
  SEEALSO ("map", "Matrix" )
  }

document { "map(Module,Module,...)",
     TT "map(N,M,...)", " -- methods for making maps from a module M to
     a module N.",
     PARA,
     "If a matrix is provided, and the modules are subquotient modules, then
     the matrix is understood to be formed with respect to generators of
     the subquotient modules.",
     PARA,
     MENU {
	  TO "map(Matrix)",
	  TO "map(Module)",
       	  TO "map(Module,Module)",
       	  TO "map(Module,Module,List)",
       	  TO "map(Module,Module,Function)",
       	  TO "map(Module,Module,Matrix)",
       	  TO "map(Module,RingElement)",
       	  TO "map(Module,Nothing,List)",
       	  TO "map(Module,ZZ,List)",
       	  TO "map(Module,ZZ,Function)",
       	  TO "map(Module,Matrix)",
	  TO "map(Module,Module,RingElement)"
	  },
     SEEALSO ("map", "matrix")
     }

matrix(Matrix) := (m,options) -> (
     if isFreeModule target m and isFreeModule source m
     and ring source m === ring target m
     then m
     else map(cover target m, cover source m ** ring target m, m, Degree => degree m)
     )

document { "map(Matrix)",
     TT "map(f, Degree => d)", " -- make a map of degree d from a map f
     of modules by tensoring the source module with a free module of
     rank 1 and appropriate degree."
     }

document { "matrix(Matrix)",
     TT "matrix f", " -- produce the matrix of a map f.",
     PARA,
     "If the source and target of f are free, then the result is
     f itself.  Otherwise, the source and target will be replaced by
     the free modules whose basis elements correspond to the generators
     of the modules.",
     SEEALSO ("map", "matrix")
     }

document { "matrix(List)",
     TT "matrix v", " -- create a matrix from a doubly-nested list of
     ring elements or matrices, or from a list of (column) vectors.",
     PARA,
     "An attempt is made to coerce the ring elements and matrices to
     a common ring.  If the entries are ring elements, they are used as
     the entries of the matrix, and if the entries are matrices, then
     they are used to provide blocks of entries in the resulting matrix.",
     PARA,
     "An attempt is made to set up the degrees of the generators of the
     free module serving as source so that the map will be homogeneous and of
     degree zero.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "p = matrix {{x,y,z}}",
     EXAMPLE "degrees source p",
     EXAMPLE "isHomogeneous p",
     "Notice that the degrees were set up so that p is homogeneous, because
     the source module is not explicitly specified by the user.  The next
     example involves block matrices.",
     EXAMPLE "q = vars R",
     EXAMPLE "matrix {{q,q,q}}",
     EXAMPLE "matrix {{q},{q},{q}}",
     "Here we construct a matrix from column vectors.",
     EXAMPLE "F = R^3",
     EXAMPLE "matrix {F_2, F_1, x*F_0 + y*F_1 + z*F_2}",
     SEEALSO ("map", "matrix")
     }
document { "matrix(Ring,List)",
     TT "matrix(R,v)", " -- create a matrix over R from a doubly-nested list of
     ring elements or matrices.",
     PARA,
     "This is essentially the same as ", TO "matrix(List)", " together with
     the specification of the ring.",
     PARA,
     EXAMPLE "R = ZZ/101[a..f]",
     EXAMPLE "matrix(R, {{a,b,0},{d,0,f}})",
     SEEALSO ("map", "matrix")
     }

document { "map(Module,Module)",
     TT "map(M,N)", " -- constructs the natural map from N to M.",
     PARA,
     "The modules M and N should be subquotient modules of the same
     free module",
     SEEALSO ("map", "isWellDefined")
     }

document { "map(Module,Matrix)",
     TT "map(M,p)", " -- recasts a matrix p to a map whose target is M by
     tensoring p with a graded free module of rank 1.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y]",
     EXAMPLE "p = matrix{{x,y}}",
     EXAMPLE "q = map(R^{3},p)",
     EXAMPLE "degrees target q",
     EXAMPLE "degrees source q",
     SEEALSO ("map", "matrix")
     }

document { "map(Module,Module,List)",
     TT "map(M,N,v)", " -- produces a map (matrix) from the module N
     to the module M whose entries are obtained from the doubly-nested list
     v of ring elements.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "p = map(R^2,R^{-2,-2},{{x^2,0},{0,y^2}})",
     EXAMPLE "isHomogeneous p",
     SEEALSO ("map", "matrix")
     }
document { "map(Module,Module,Matrix)",
     TT "map(M,N,p)", " -- recasts the matrix p as a map (matrix) from
     the module N to the module M.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "p = matrix {{x,y,z}}",
     EXAMPLE "q = map(R^1,R^3,p)",
     EXAMPLE "degrees source p",
     EXAMPLE "degrees source q",
     SEEALSO ("map", "matrix")
     }
document { "map(Module,Module,RingElement)",
     TT "map(M,N,r)", " -- construct a map from a module N to M which provided
     by the ring element r.",
     PARA,
     "If r is nonzero, then M and N should be equal, or differ at most by
     a degree (i.e., by tensoring with a graded free module of rank 1).",
     PARA,
     EXAMPLE "R = ZZ/101[x]",
     EXAMPLE "map(R^2,R^3,0)",
     EXAMPLE "map(R^2,R^2,x)",
     EXAMPLE "q = map(R^2,R^2,x,Degree=>1)",
     EXAMPLE "isHomogeneous q",
     PARA,
     SEEALSO ("map", "matrix")
     }
document { "map(Module)",
     TT "map M", " -- construct the identity map from M to itself.",
     PARA,
     "This can also be accomplished with ", TT "id_M", " or ", TT "map(M,1)", ".",
     SEEALSO ("map", "id")
     }
document { "map(Module,RingElement)",
     TT "map(M,r)", " -- construct the map from M to itself which is provided
     by scalar multiplication by the ring element r.",
     PARA,
     EXAMPLE "R = ZZ/101[x]",
     EXAMPLE "map(R^2,x)",
     SEEALSO ("map", "matrix")
     }
document { quote Degree,
     TT "Degree => d", " -- an optional argument to ", TO "matrix", " that
     specifies that the degree of the map created should be ", TT "d", ".",
     PARA,
     "The degree may be an integer or a list of integers (multidegree).  The
     length of the list should be the same as the length of a degree for the
     ring, see ", TO "degreeLength", ".",
     PARA,
     EXAMPLE "R = ZZ/101[x]",
     EXAMPLE "p = map(R^1, R^1, {{x^4}})",
     EXAMPLE "isHomogeneous p",
     EXAMPLE "q = map(R^1, R^1, {{x^4}}, Degree => 4)",
     EXAMPLE "isHomogeneous q",
     SEEALSO ("map", "matrix")
     }

document { "map(Module,ZZ,Function)",
     TT "map(M,n,f)", " -- construct a map from a free graded module of
     rank n to M whose entries are obtained from the function f by 
     evaluating f(i,j).",
     PARA,
     "The degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero."
     }

document { "map(Module,ZZ,List)",
     TT "map(M,n,v)", " -- construct a map from a free graded module of
     rank n to M whose entries are in the doubly nested list v.",
     PARA,
     "The degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero."
     }

document { "map(Module,Nothing,List)",
     TT "map(M,,v)", " -- construct a map from a free graded module to M
     whose entries are obtained from the doubly-nested list v of
     ring elements.",
     PARA,
     "The absence of the second argument indicates that the source of the map
     is to be a free module constructed with an attempt made to assign degrees
     to its basis elements so as to make the map homogeneous of degree zero.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y]",
     EXAMPLE "f = map(R^2,,{{x^2,y^2},{x*y,0}})",
     EXAMPLE "degrees source f",
     EXAMPLE "isHomogeneous f",
     SEEALSO ("map", "matrix")
     }
matrix(List) := (m,options) -> (
     if #m === 0 then error "expected nonempty list";
     m = apply(splice m,splice);
     types := unique apply(m,class);
     if #types === 1 then (
	  type := types#0;
	  if instance(type,Module) 
	  then map(type,,table(numgens type, #m, (i,j) -> m_j_i))
	  else if type === List then (
	       if isTable m then matrixTable(m,options)
	       else error "expected rows all to be the same length"
	       )
	  else error "expected a table of ring elements or matrices")
     else error "expected a table of ring elements or matrices")

--------------------------------------------------------------------------

Module#id = (M) -> map(M,1)

document { quote id,
     TT "id_M", " -- the identity homomorphism from M to M.",
     PARA,
     "M may be a ", TO "Module", " or a ", TO "ChainComplex", ".",
     PARA,
     SEEALSO("Matrix", "ChainComplexMap", "ScriptedFunction")
     }

reshape = (F, G, m) -> (
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     sendgg(ggPush m, ggPush F, ggPush G, ggreshape);
     getMatrix ring m)
document { quote reshape,
     TT "reshape(F,G,m)", " -- reshapes the matrix m to give a map from G to F.",
     PARA,
     "It yields the matrix obtained from ", TT "m", " of shape F <--- G, by
     taking elements from the first row of ", TT "m", ", then the second, and
     so on, filling them into the result row by row.  Currently, it is assumed
     that ", TT "m", " and the result both have the same number of entries.
     The resulting map is always of degree zero."
     }

TEST "
R=ZZ/101[a..d]
f = matrix {{a}}
assert( isHomogeneous f )

g = reshape(R^1, R^{-1}, f)
assert isHomogeneous g
"

-- adjoint1:  m : F --> G ** H ===> F ** dual G --> H
-- adjoint:   m : F ** G --> H ===> F --> dual G ** H
adjoint1 = (m,G,H) -> reshape(H, (source m) ** (dual G), m)
document { quote adjoint1,
     TT "adjoint1 (f,G,H)", " -- if f is a homomorphism of free modules of the
     form F -> G ** H, then produce the adjoint homomorphism of the
     form F ** (dual G) -> H.",
     SEEALSO "adjoint"
     }
adjoint =  (m,F,G) -> reshape((dual G) ** (target m), F, m)
document { quote adjoint,
     TT "adjoint (f,F,G)", " -- if f is a homomorphism of free modules of the
     form F ** G -> H, then produce the adjoint homomorphism of the
     form F -> (dual G) ** H.",
     SEEALSO "adjoint1"
     }

flatten Matrix := m -> (
     F := target m;
     G := source m;
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     if numgens F === 1 then m
     else (
     	  R := ring m;
     	  reshape(R^1, G ** dual F ** R^{- degree m}, m)))

flip = (F,G) -> (
  sendgg(ggPush F, ggPush G, ggflip);
  getMatrix ring F)
document { quote flip,
     TT "flip(F,G)", " -- yields the matrix representing the map F ** G --> G ** F."
     }

subquotient(Nothing,Matrix) := (null,relns) -> (
     M := new Module of Vector;
     M.ring = ring relns;
     E := target relns;
     M.handle = handle E;
     relns = matrix relns;
     if E.?generators then (
	  M.generators = E.generators;
	  relns = E.generators * relns;
	  );
     if E.?relations then relns = relns | E.relations;
     if relns != 0 then M.relations = relns;
     if not M.?generators and not M.?relations then M.numgens = E.numgens;
     M#0 = (
	  sendgg(ggPush M, ggzero);
	  new M);
     M)
subquotient(Matrix,Nothing) := (subgens,null) -> (
     M := new Module of Vector;
     E := target subgens;
     subgens = matrix subgens;
     if E.?generators then subgens = E.generators * subgens;
     M.handle = E.handle;
     M.generators = subgens;
     if E.?relations then M.relations = E.relations;
     M.ring = ring subgens;
     M#0 = (
	  sendgg(ggPush M, ggzero);
	  new M);
     M)
subquotient(Matrix,Matrix) := (subgens,relns) -> (
     E := target subgens;
     if E != target relns then error "expected maps with the same target";
     M := new Module of Vector;
     M.ring = ring subgens;
     M.handle = handle E;
     relns = matrix relns;
     subgens = matrix subgens;
     if E.?generators then (
	  relns = E.generators * relns;
	  subgens = E.generators * subgens;
	  );
     if E.?relations then relns = relns | E.relations;
     M.generators = subgens;
     if relns != 0 then M.relations = relns;
     M#0 = (
	  sendgg(ggPush M, ggzero);
	  new M);
     M)
document { quote subquotient,
     TT "subquotient(f,g)", " -- given matrices f and g with the same target, 
     produces a new module representing the image of f in the cokernel
     of g.",
     PARA,
     "The columns of f are called the generators, and the columns of
     g are the relations.",
     PARA,
     "Functions:",
     MENU {
	  {TO "generators", "      -- recover the generators"},
	  {TO "relations", " -- recover the relations"},
	  {TO "prune", "     -- convert to a module with presentation"}
	  },
     "This is the general form in which modules are represented, and
     subquotient modules are often returned as values of computations.",
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "M = kernel vars R ++ cokernel vars R",
     EXAMPLE "generators M",
     EXAMPLE "relations M",
     EXAMPLE "prune M",
     SEEALSO ("generators", "relations")
     }


Matrix ** Matrix := (f,g) -> (
     R := ring f;
     if ring g =!= R then error "expected matrices over the same ring";
     sendgg (ggPush f, ggPush g, ggtensor);
     h := getMatrix R;
     map(target f ** target g, source f ** source g, h))

TEST "
ZZ[t]
assert (matrix {{t}} ** matrix {{t}} == matrix{{t^2}})
"

Matrix ** RingElement := (f,r) -> f ** matrix {{r}}
RingElement ** Matrix := (r,f) -> matrix {{r}} ** f
RingElement ** RingElement := (r,s) -> matrix {{r}} ** matrix {{s}}

AfterPrint Matrix := AfterNoPrint Matrix := f -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : Matrix";
     if isFreeModule target f and isFreeModule source f
     then << " " << target f << " <--- " << source f;
     << endl;
     )

precedence Matrix := x -> precedence quote x

net Matrix := f -> (
     if f == 0 
     then "0"
     else verticalJoin unlist apply(
	  lines sendgg(ggPush f,ggsee,ggpop), x -> concatenate("| ",x,"|"))
     )

image Matrix := f -> (
     if f.?image then f.image else f.image = subquotient(f,)
     )
cokernel Matrix := m -> (
     if m.?cokernel then m.cokernel else m.cokernel = subquotient(,m)
     )

cokernel RingElement := f -> cokernel matrix {{f}}
image RingElement := f -> image matrix {{f}}

Ideal = new Type of MutableHashTable
net Ideal := (I) -> net new FunctionApplication from { ideal, I.generators }
name Ideal := (I) -> name new FunctionApplication from { ideal, 
     unlist first entries gens I }

isHomogeneous Ideal := (I) -> isHomogeneous I.generators
genera(Ideal) := (I) -> genera module I
euler(Ideal) := (I) -> euler module I

RingElement * Ideal := (r,I) -> ideal (r ** generators I)
ZZ * Ideal := (r,I) -> ideal (r * generators I)

generators Ideal := (I) -> I.generators
mingens Ideal := (I,options) -> mingens(module I,options)
Ideal / Ideal := (I,J) -> module I / module J
Module / Ideal := (M,J) -> M / (J * M)

AfterPrint Ideal := AfterNoPrint Ideal := (I) -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : Ideal of " << ring I << endl;
     )

Ideal ^ ZZ := (I,n) -> ideal symmetricPower(n,generators I)
Ideal * Ideal := (I,J) -> ideal flatten (generators I ** generators J)
Ideal * Module := (I,M) -> subquotient (generators I ** generators M, relations M)
dim Ideal := I -> dim cokernel generators I
codim Ideal := I -> codim cokernel generators I
Ideal + Ideal := {Ideal,
     (I,J) -> ideal (generators I | generators J),
     TT "I + J", " -- the sum of two ideals."}
degree Ideal := I -> degree cokernel generators I
trim Ideal := (I,options) -> ideal trim(module I, options)
map(Ideal) := (I,options) -> map(module I,options)
map(Ideal,Ideal) := (I,J,options) -> map(module I,module J,options)
Ideal _ ZZ := (I,n) -> (generators I)_(0,n)
Matrix % Ideal := (f,I) -> f % gb I
numgens Ideal := (I) -> numgens source generators I
leadTerm Ideal := (I) -> leadTerm generators gb I
leadTerm(ZZ,Ideal) := (n,I) -> leadTerm(n,generators gb I)
jacobian Ideal := (I) -> jacobian generators I
poincare Ideal := (I) -> poincare module I
hilbertPolynomial Ideal := (I,options) -> hilbertPolynomial(module I,options)

protect quote Order
assert( class infinity === InfiniteNumber )
hilbertSeries = method(Options => {
     	  Order => infinity
	  }
     )

hilbertSeries Ideal := (I,options) -> hilbertSeries(module I,options)

TEST "
R = ZZ/101[x,y,z]
I = ideal(x,y)
assert( 1 == dim I )
assert( 2 == codim I )
"

document { quote Ideal,
     TT "Ideal", " -- the class of all ideals.",
     PARA,
     "The justification for considering an ideal I as different from a
     submodule M of R^1 is some methods are different.  For example, M^3 is a
     direct sum, whereas I^3 is still an ideal.  Similar remarks apply to
     ", TO "dim", " and ", TO "codim", ".",
     PARA,
     "Creating ideals:",
     MENU {
	  TO "annihilator",
	  TO "fittingIdeal",
	  TO "ideal",
	  TO "quotient"
	  },
     "Operations on ideals:",
     MENU {
	  TO "codim",
	  TO "decompose",
	  TO "dim",
	  TO "Ideal * Ideal",
	  TO "Ideal ^ ZZ",
	  TO "module",
	  TO "radical",
	  TO "removeLowestDimension",
	  TO "top"
	  }
     }

document { "Ideal * Ideal",
     TT "I * J", " -- the product of two ideals."
     }

document { "Ideal ^ ZZ",
     TT "I^n", " -- the n-th power of an ideal I."
     }

ring Ideal := (I) -> I.ring

Ideal == Ring := (I,R) -> (
     if ring I =!= R
     then error "expected ideals in the same ring";
     1_R % I == 0)

Ring == Ideal := (R,I) -> I == R

Ideal == Ideal := (I,J) -> (
     if ring I =!= ring J
     then error "expected ideals in the same ring";
     ( I.generators == J.generators or 
	  if isHomogeneous I and isHomogeneous J  -- can be removed later
	  then gb I == gb J 
	  else isSubset(I,J) and isSubset(J,I)	  -- can be removed later
	  ))

Ideal == Module := (I,M) -> module I == M
Module == Ideal := (M,I) -> M == module I

module = method()
module Ideal := submodule Ideal := I -> image I.generators

document { quote module,
     TT "module I", " -- produce the submodule of R^1 corresponding to an
     ideal I."
     }

ideal Matrix := (f) -> (
     if not isFreeModule target f or not isFreeModule source f 
     then error "expected map between free modules";
     new Ideal from {
	  quote generators => flatten f,
	  quote ring => ring f
	  }
     )
ideal Module := (M) -> (
     F := ambient M;
     if isSubmodule M and rank F === 1 then ideal generators M
     else error "expected a submodule of a free module of rank 1"
     )
ideal List := ideal Sequence := v -> ideal matrix {elements v}
submodule List := submodule Sequence := v -> image matrix elements v
ideal RingElement := v -> ideal {v}
submodule(Vector) := (v) -> image matrix {v}
ideal ZZ := v -> ideal {v}

document { quote submodule,
     TT "submodule (u,v,w)", " -- form the submodule generated by a sequence
     or list of elements of a module.",
     BR,NOINDENT,
     TT "submodule I", " -- form the submodule corresponding to an ideal."
     }

document { quote ideal,
     "ideal v", " -- produces the ideal spanned by a list or sequence of ring
     elements.",
     PARA,
     EXAMPLE "ZZ[a..i]",
     EXAMPLE "ideal (c..h)"
     }

kernel = method(Options => {
	  SubringLimit => infinity
	  })

ker = kernel
document { quote ker,
     "See ", TO "kernel", "."
     }

document { quote kernel,
     TT "kernel f", " -- produces the kernel of a matrix or ring homomorphism.",
     PARA,
     "If f is a ring element, it will be interpreted as a one by one
     matrix.",
     PARA,
     "Options:",
     MENU {
	  TO "SubringLimit"
	  },
     PARA,
     "For an abbreviation, use ", TO "ker", "."
     }

document { quote SubringLimit,
     TT "SubringLimit => n", " -- an option for ", TO "kernel", " which
     causes the computation of the kernel of a ring map to stop after n
     elements have been discovered."
     }

kernel Matrix := (g,options) -> if g.?kernel then g.kernel else g.kernel = (
     N := source g;
     P := target g;
     g = matrix g;
     if P.?generators then g = P.generators * g;
     h := modulo(g, if P.?relations then P.relations);
     if N.?generators then h = N.generators * h;
     subquotient( h, if N.?relations then N.relations))

kernel RingElement := (g,options) -> kernel (matrix {{g}},options)

homology(Matrix,Matrix) := (g,f) -> (
     M := source f;
     N := target f;
     P := target g;
     if source g != N then error "expected maps to be composable";
     f = matrix f;
     g = matrix g;
     if P.?generators then g = P.generators * g;
     h := modulo(g, if P.?relations then P.relations);
     if N.?generators then (
	  f = N.generators * f;
	  h = N.generators * h;
	  );
     subquotient(h, if N.?relations then f | N.relations else f))

TEST "
"

Hom(Matrix, Module) := (f,N) -> (
     if not (isFreeModule source f and isFreeModule target f) then notImplemented();
     (transpose f) ** N)

dual(Matrix) := { Matrix,
     f -> (
	  R := ring f;
	  Hom(f,R^1)
	  ),
     TT "dual f", " -- the dual (transpose) of a homomorphism."
     }

InverseMethod Matrix := m -> if m#?-1 then m#-1 else m#-1 = (
     id_(target m) // m
     )

singularLocus(Ring) := (R) -> (
     if not isAffineRing(R) then error "expected an affine ring";
     R / minors(codim R, jacobian presentation R))

singularLocus(Ideal) := (I) -> singularLocus(ring I / I)

document { quote singularLocus,
     TT "singularLocus R", " -- produce the singular locus of a ring,
     which is assumed to be integral and defined by a homogeneous ideal.",
     PARA,
     "Can also be applied to an ideal, in which case the singular locus of
     the quotient ring is returned."
     }

TEST "
     R=ZZ/101[x,y,z]

     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x + z) } === 0 )
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x - z) } === 1 )

     S = ZZ/103[a..d]
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + b^2 + 3*c^2 + 2*d^2 } === 1 )
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + 5*b^2 + 3*c^2 + 2*d^2 } === 0 )
     "

Matrix _ Array := (f,v) -> f * (source f)_v
Matrix ^ Array := (f,v) -> (target f)^v * f

Matrix _ List := (f,v) -> (
     v = splice v;
     listZ v;
     submatrix(f,v)
     )
document { "f_{i,j}",
     TT "f_{i,j,k,...}", " -- produce the submatrix of a matrix f consisting of 
     columns numbered i, j, k, ... .",
     PARA,
     "Repetitions of the indices are allowed.",
     PARA,
     "If the list of column indices is a permutation of 0 .. n-1, where n is
     the number of columns, then the result is the corresponding permutation
     of the columns of f.",
     PARA,
     EXAMPLE "R = ZZ/101[a..f]",
     EXAMPLE "p = matrix {{a,b,c},{d,e,f}}",
     EXAMPLE "p_{1}",
     EXAMPLE "p_{1,1,2}",
     EXAMPLE "p_{2,1,0}",
     SEEALSO "_"
     }

Matrix ^ List := (f,v) -> (
     v = splice v;
     listZ v;
     submatrix(f,v,)
     )
document { "f^{i,j}",
     TT "f^{i,j,k,...}", " -- produce the submatrix of a matrix f consisting of 
     rows numbered i, j, k, ... .",
     PARA,
     "Repetitions of the indices are allowed.",
     PARA,
     "If the list of row indices is a permutation of 0 .. n-1, where n is
     the number of rows, then the result is the corresponding permutation
     of the rows of f.",
     PARA,
     EXAMPLE "R = ZZ/101[a..f]",
     EXAMPLE "p = matrix {{a,b,c},{d,e,f}}",
     EXAMPLE "p^{1}",
     EXAMPLE "p^{1,0}",
     SEEALSO "^"
     }

entries = method()
entries Matrix := (m) -> (
     M := target m;
     R := ring M;
     N := source m;
     sendgg (ggPush m,
      	  apply(numgens M, i -> apply(numgens N, j -> (
		    	 ggdup, ggINT, gg i, ggINT, gg j, ggelem, ggINT, gg 1, ggpick
		    	 ))));
     RPop := R.pop;
     sendgg ggpop;
     r := reverse apply(numgens M, i -> reverse apply(numgens N, j -> RPop()));
     r)
document { quote entries,
     TT "entries f", " -- produces the matrix of the homomorphism f as a doubly
     nested list of ring elements.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "p = matrix {{x^2,y^2},{x*y*z, x^3-y^3}}",
     EXAMPLE "entries p"
     }
TEST"
R=ZZ/101[a..f]
p = {{a,b},{c,d},{e,f}}
assert( entries matrix p == p )
"

TEST "
R = ZZ/101[a .. r]
assert ( genericMatrix(R,a,3,6) == genericMatrix(R,a,3,6) )
ff = genericMatrix(R,a,3,6)
fff = genericMatrix(R,a,3,6)
assert( # expression ff == 3 )
assert( ff == matrix {{a,d,g,j,m,p},{b,e,h,k,n,q},{c,f,i,l,o,r}} )
assert( -ff == matrix {
	  {-a,-d,-g,-j,-m,-p},
	  {-b,-e,-h,-k,-n,-q},
	  {-c,-f,-i,-l,-o,-r}} )
assert( 2*ff == matrix {
	  {2*a,2*d,2*g,2*j,2*m,2*p},
	  {2*b,2*e,2*h,2*k,2*n,2*q},
	  {2*c,2*f,2*i,2*l,2*o,2*r}} )
assert( ff != 0 )
assert( ff - ff == 0 )
assert( transpose ff - matrix{{a,b,c},{d,e,f},{g,h,i},{j,k,l},{m,n,o},{p,q,r}} == 0 )
--assert( transpose ff == matrix{{a,b,c},{d,e,f},{g,h,i},{j,k,l},{m,n,o},{p,q,r}} ) -- mike will fix.  DRG: these are not equal: they have different degrees...
assert( ff_0 == vector {a,b,c} )
assert( ff_1 == vector {d,e,f} )
assert( ff_2 == vector {g,h,i} )
M = cokernel ff
assert ( ff === presentation M )		  -- original map saved
assert ( cokernel ff === M )		  -- cokernel memoized
-- gbTrace 3
-- << \"gb ff ...\" << flush
G = gb ff
pM = poincare M
MM = cokernel fff
MM.poincare = pM
-- << \"gb fff (with poincare provided) ...\" << flush
GG = gb fff

assert( numgens source generators G == numgens source generators GG )
T := (ring pM)_0
assert ( pM == 3-6*T+15*T^4-18*T^5+6*T^6 )
assert ( gb ff === G )
assert ( numgens source generators G == 41 )
assert ( numgens source mingens G == 6 )
time C = resolution M
assert( C === resolution M )
-- betti C
time D = resolution cokernel leadTerm generators G
-- betti D
"

getshift := (f) -> (
     sendgg(ggPush f, gggetshift);
     eePopIntarray())

degree(Matrix) := (f) -> (
     M := source f;
     N := target f;
     d := getshift f;
     if M.?generators then d = d - getshift M.generators;
     if N.?generators then d = d + getshift N.generators;
     d)

promote(Matrix,ZZ) := (f,ZZ) -> (
     if ring f === ZZ then f
     else error "can't promote");
promote(Matrix,QQ) := (f,QQ) -> (
     if ring f === QQ then f
     else matrix applyTable(entries f, r -> promote(r,QQ)));

super(Matrix) := (f) -> (
     M := target f;
     if M.?generators then map(super M, M, M.generators) * f
     else f
     )

isInjective Matrix := (f) -> kernel f == 0
isSurjective Matrix := (f) -> cokernel f == 0

document { quote isInjective,
     TT "isInjective f", " -- tells whether the ring map or module
     map f is injective.",
     SEEALSO "isSurjective"
     }

document { quote isSurjective,
     TT "isSurjective f", " -- tells whether the map f of modules is
     surjective",
     SEEALSO "isInjective"
     }

TEST "
R = ZZ/101[a]
assert isInjective R^2_{0}
assert not isInjective R^2_{0,0}
assert isSurjective R^2_{0,0,1}
assert not isSurjective R^2_{1}
"


scan({ZZ}, S -> (
	  lift(Matrix,S) := (f,S) -> (
	       -- this will be pretty slow
	       if ring target f === S then f
	       else if isQuotientOf(ring f,S) and
		       isFreeModule source f and
		       isFreeModule target f then
		   map(S^(-degrees target f), S^(-degrees source f), 
		       applyTable(entries f, r -> lift(r,S)))
	       else matrix(S, applyTable(entries f, r -> lift(r,S)))
	       );
	  lift(Ideal,S) := (I,S) -> (
	       -- this will be pretty slow
	       if ring I === S then I
	       else
		   (ideal lift(I.generators,S)) +
		   ideal (presentation ring I ** S));
	  ));

content(RingElement) := content(Matrix) := (f) -> (
     R := ring f;
     n := numgens R;
     k := coefficientRing R;
     trim ideal lift((coefficients(splice {0..n-1},f))#1, k))

document { quote content,
     TT "content f", " -- returns the content of a matrix or polynomial.",
     PARA,
     "The content is the ideal of the base ring generated by the 
     coefficients."
     }


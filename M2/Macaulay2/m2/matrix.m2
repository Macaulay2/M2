--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

ModuleMap = new Type of MutableHashTable
document { quote ModuleMap,
     TT "ModuleMap", " -- the class of all maps between modules.",
     PARA,
     "This class is experimental, designed to support graded modules.",
     SEEALSO {"Matrix"}
     }

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
     "Multiplication of matrices corresponds to composition of maps, and when
     ", TT "f", " and ", TT "g", " are maps so that the target ", TT "Q", "
     of ", TT "g", " equals the source ", TT "P", " of ", TT "f", ", the
     product ", TT "f*g", " is defined, its source is the source of ", TT
     "g", ", and its target is the target of ", TT "f", ".  The degree of ",
     TT "f*g", " is the sum of the degrees of ", TT "f", " and of ", TT "g",
     ".  The product is also defined when ", TT "P", " != ", TT "Q", ",
     provided only that ", TT "P", " and ", TT "Q", " are free modules of the
     same rank.  If the degrees of ", TT "P", " differ from the corresponding
     degrees of ", TT "Q", " by the same degree ", TT "d", ", then the degree
     of ", TT "f*g", " is adjusted by ", TT "d", " so it will have a good
     chance to be homogeneous, and the target and source of ", TT "f*g", "
     are as before.", 
     PARA,
     "If ", TT "h", " is a matrix then ", TT "h_j", " is the ", TT "j", "-th
     column of the matrix, and ", TT "h_j_i", " is the entry in row ", TT
     "i", ", column ", TT "j", ".  The notation ", TT "h_(i,j)", " can be
     used as an abbreviation for ", TT "h_j_i", ", allowing row and column
     indices to be written in the customary order.",
     PARA,
     "If ", TT "m", " and ", TT "n", " are matrices, ", TT "a", " is a ring element, 
     and ", TT "i", " is an integer, then ", TT "m+n", ", ", TT "m-n", ", 
     ", TT "-m", ", ", TT "m n", ", ", TT "a*m", ", and ", TT "i*m", " denote the
     usual matrix arithmetic.  Use ", TT "m == n", ", and ", TT "m == 0", " to 
     check equality of matrices.",
     PARA,
     "Operations which produce matrices:", 
     MENU {
	  TO "flip",
          (TO "genericMatrix", " -- an generic matrix"),
          (TO "genericSkewMatrix", " -- an generic skew-symmetric matrix"),
          (TO "genericSymmetricMatrix", " -- a generic symmetric matrix"),
	  (TO "id", " -- identity maps"),
	  (TO "matrix", " -- create a matrix"),
	  (TO "map", " -- create a map of modules"),
	  (TO "random", " -- a random homgeneous matrix")
	  },
     "Operations on matrices:",
     MENU {
	  (TO "==", " -- equality test"),
	  (TO "!=", " -- inequality test"),
	  (TO "+", " -- sum"),
	  (TO "-", " -- difference"),
	  (TO "*", " -- product"),
	  (TO "^", " -- power"),
	  (TO (quote ^, Matrix, List), " -- extracting or permuting rows"),
	  (TO (quote ^, Matrix, Array), " -- extracting or permuting blocks of rows"),
	  (TO (quote %,Matrix,Matrix), " -- remainder"),
	  (TO (quote %,Matrix,RingElement), " -- remainder"),
	  (TO (quote //,Matrix,Matrix), " -- quotient"),
	  (TO (quote //,Matrix,RingElement), " -- quotient"),
	  (TO (quote _, Matrix, Sequence), " -- getting an entry"),
	  (TO (quote _, Matrix, List), " -- extracting or permuting columns"),
	  (TO (quote _, Matrix, Array), " -- extracting or permuting blocks of columns"),
	  (TO (quote |, Matrix, Matrix), " -- horizontal concatenation"),
	  (TO (quote ||, Matrix, Matrix), " -- vertical concatenation"),
	  (TO (quote ++, Matrix, Matrix), " -- direct sum"),
	  (TO (quote **,Matrix, Matrix), " -- tensor product of matrices"),
	  (TO (quote **, Matrix, Module), " -- tensor product, e.g., degree shifting"),
	  (TO (quote **,Matrix,Ring), " -- tensor product, base change"),
	  TO ":",
	  TO "adjoint",
	  TO "adjoint1",
	  TO "ambient",
	  (TO "basis", "               -- k-basis of a module in a given degree"),
	  (TO "borel", " m              -- smallest Borel submodule containing lead
		monomials of m"),
	  TO "codim",
	  TO "complement",
	  (TO "compress", "             -- removal of zero columns"),
	  TO "content",
	  (TO {"contract", "(m,n)"}, " -- contraction of n by m (differentiation without 
                          the coefficients)"),
	  TO "degree",
	  (TO "det", "                 -- determinant"),
	  (TO "diff", "                -- differentiation"),
	  (TO "divideByVariable", "    -- divide columns by a variable repeatedly"),
	  TO "dual",
	  TO "selectInSubring",
	  (TO "entries", "             -- the entries of m"),
	  (TO "exteriorPower", "       -- exterior power of m"),
          (TO "flatten", "             -- collect entries of a matrix into one row"),
	  (TO "inducedMap", "          -- a map induced on subquotients"),
	  (TO "inducesWellDefinedMap", " -- whether a matrix would induce a well defined map"),
          (TO "isHomogeneous", "       -- whether a matrix is homogeneous"),
	  (TO "isInjective", "         -- whether a map is injective"),
          (TO "isIsomorphism", "       -- whether a map is an isomorphism"),
	  (TO "isSurjective", "        -- whether a map is surjective"),
	  (TO "isWellDefined", "       -- whether a map is well-defined"),
	  (TO "homogenize", "          -- homogenize a matrix"),
	  (TO "jacobian", "            -- Jacobian matrix of a matrix"),
	  (TO "koszul", "              -- i-th Koszul matrix of a matrix"),
          (TO "leadTerm", "            -- lead monomial matrix of the columns of a matrix"),
 	  (TO "minors", "              -- ideal minors of a matrix"),
	  TO "modulo",
	  (TO "pfaffians", " -- ideal of i by i Pfaffians of a skew symmetric matrix"),
	  TO "poincare",
	  TO "reshape",
          (TO "ring", "                -- the base ring of a matrix"),
	  TO "singularLocus",
	  (TO "sortColumns", "         -- sort the columns of a matrix"),
          (TO "source", "              -- the source free module of a map"),
 	  (TO "submatrix", "           -- extract a submatrix"),
	  (TO "substitute", "          -- replacing the variables in a matrix"),
	  (TO "symmetricPower", "      -- symmetric power of a matrix"),
          (TO "target", "              -- the target module of a map"),
	  TO "top",
	  TO "topCoefficients",
	  (TO "trace", "               -- trace"),
 	  (TO "transpose", "           -- transpose a matrix")
	  },
     PARA,
     "Operations which produce modules from matrices:",
     MENU {
	  (TO "cokernel", "            -- the cokernel of a map"),
	  TO "homology",
	  TO "image",
	  TO "kernel",
	  (TO "kernel", "              -- the kernel of a map"),
	  TO "submodule",
	  TO "subquotient"
	  },
     "Operations which produce Groebner bases from matrices:",
     MENU {
	  TO "gb",
	  TO "mingens",
	  TO "syz"
	  },
     "Printing matrices:",
     MENU {
	  TO "compactMatrixForm",
	  }
     }

document { quote gcdDegree,
     TT "gcdDegree F", " -- I don't know what this is supposed to do.",
     }
document { quote lcmDegree,
     TT "lcmDegree F", " -- I don't know what this is supposed to do.",
     }

local newMatrix				  -- defined below

reduce := (tar) -> (
     if not isFreeModule tar and not ring tar === ZZ then (
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
document { (quote _, Matrix, Sequence),
     TT "f_(i,j)", " -- provide the element in row ", TT "i", " and
     column ", TT "j", " of the matrix ", TT "f", ".",
     SEEALSO {"_", "Matrix"}
     }

Matrix _ ZZ := (m,i) -> (
     if 0 <= i and i < numgens source m then (
     	  sendgg (ggPush m, ggPush i, ggelem);
     	  new m.target)
     else error ("subscript '", name i, "' out of range"))
document { (quote _, Matrix, ZZ),
     TT "f_i", " -- provide the ", TT "i", "-th column of a matrix ", TT "f", " as a vector.",
     PARA,
     "Vectors are disparaged, so we may do away with this function in the future.",
     SEEALSO "_"
     }

Matrix == Matrix := (m,n) -> (
     target m == target n
     and source m == source n
     and (
     	  sendgg (ggPush m, ggPush n, ggisequal); 
     	  eePopBool()))
Matrix == RingElement := (m,f) -> m - f == 0
RingElement == Matrix := (f,m) -> m - f == 0
Matrix == ZZ := (m,i) -> (
     if i === 0
     then (
	  sendgg(ggPush m, ggiszero); 
	  eePopBool()
	  )
     else (
	  R := ring m;
	  m == i_R
	  )
     )
ZZ == Matrix := (i,m) -> m == i

Matrix + Matrix := BinaryMatrixOperationSame ggadd
Matrix + RingElement := (f,r) -> if r == 0 then f else f + r*id_(target f)
RingElement + Matrix := (r,f) -> if r == 0 then f else r*id_(target f) + f
ZZ + Matrix := (i,f) -> if i === 0 then f else i*1_(ring f) + f
Matrix + ZZ := (f,i) -> if i === 0 then f else f + i*1_(ring f)

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
		    ggdup, ggPush toList (degreeLength R:0), ggsetshift);
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

expression Matrix := m -> MatrixExpression applyTable(entries m, expression)

name Matrix := m -> concatenate (
     -- "matrix (", name target m, ", ", name source m, ", ", name entries m, ")"
     "matrix ", name entries m
     )

isIsomorphism Matrix := f -> coker f == 0 and ker f == 0

isHomogeneous Matrix := m -> (
     if m.?isHomogeneous then m.isHomogeneous 
     else m.isHomogeneous = (
	  isHomogeneous ring target m
	  and isHomogeneous ring source m
	  and (
	       M := source m;
	       N := target m;
	       (sendgg(ggPush m, ggishomogeneous); eePopBool())
	       and
	       ( not M.?generators or isHomogeneous M.generators )
	       and
	       ( not N.?generators or isHomogeneous N.generators )
	       )))

isWellDefined Matrix := f -> matrix f * presentation source f % presentation target f == 0

document { quote isWellDefined,
     TT "isWellDefined m", " -- tells whether a map m of modules is 
     well-defined."
     }

ggConcatCols := (tar,src,mats) -> (
     sendgg(apply(mats,ggPush), ggPush (#mats), ggconcat);
     f := newMatrix(tar,src);
     if same(degree \ mats) and degree f != degree mats#0
     then f = map(target f, source f, f, Degree => degree mats#0);
     f)

ggConcatRows := (tar,src,mats) -> (
     sendgg(
	  apply(mats,m -> (ggPush m, ggtranspose)), 
	  ggPush (# mats), ggconcat, ggtranspose
	  );
     f := newMatrix(tar,src);
     if same(degree \ mats)
     and degree f != degree mats#0
     then f = map(target f, source f, f, Degree => degree mats#0);
     f)

ggConcatBlocks := (tar,src,mats) -> (
     sendgg (
	  apply(mats, row -> ( 
		    apply(row, m -> ggPush m), 
		    ggPush(#row), ggconcat, ggtranspose )),
	  ggPush(#mats), ggconcat, ggtranspose );
     f := newMatrix(tar,src);
     if same(degree \ flatten mats)
     and degree f != degree mats#0#0
     then f = map(target f, source f, f, Degree => degree mats#0#0);
     f)

samering := mats -> (
     R := ring mats#0;
     if not all ( mats, m -> ring m === R )
     then error "expected matrices over the same ring";
     R)

Matrix.directSum = args -> (
     R := ring args#0;
     if not all(args, f -> ring f === R) 
     then error "expected matrices all over the same ring";
     sendgg(apply(args, ggPush), ggPush (#args), ggdirectsum);
     f := newMatrix(directSum apply(args,target),directSum apply(args,source));
     f.components = toList args;
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
	       then R ^ (- join toSequence apply(args, degrees))
	       else subquotient( null, directSum apply(args,relations) )
	       )
	  else (
	       if all(args, M -> not M.?relations) then (
		    subquotient( directSum apply(args,generators), null )
		    )
	       else subquotient(
		    directSum apply(args,generators), 
		    directSum apply(args,relations)));
	  N.components = toList args;
	  N)

single := v -> (
     if not same v 
     then error "incompatible objects in direct sum";
     v#0)

indices = method()
indices MutableHashTable := X -> (
     if not X.?components then error "expected an object with components";
     if X.?indices then X.indices else toList ( 0 .. #X.components - 1 ) )

document { quote youngest,
     TT "youngest s", " -- return the youngest mutable hash table in the sequence
     ", TT "s", ", if any, else ", TT "null", "."
     }

directSum List := args -> directSum toSequence args
directSum Sequence := args -> (
     if #args === 0 then error "expected more than 0 arguments";
     y := youngest args;
     key := (directSum, args);
     if y =!= null and y#?key then y#key else (
	  type := single apply(args, class);
	  if not type.?directSum then error "no method for direct sum";
	  S := type.directSum args;
	  if y =!= null then y#key = S;
	  S))
Option ++ Option := directSum
Option.directSum = args -> (
     if #args === 0 then error "expected more than 0 arguments";
     modules := apply(args,last);
     y := youngest modules;
     key := (directSum, args);
     if y =!= null and y#?key then y#key else (
	  type := single apply(modules, class);
	  if not type.?directSum then error "no method for direct sum";
	  S := type.directSum modules;
	  if y =!= null then y#key = S;
     	  keys := S.indices = toList args/first;
     	  S.indexComponents = new HashTable from toList apply(#keys, i -> keys#i => i);
	  S))
Matrix ++ Matrix := directSum
Module ++ Module := directSum

document { (quote ++,Module,Module),
     TT "M++N", " -- computes the direct sum of two modules.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "image vars R ++ kernel vars R",
	  },
     "Projection and inclusion maps for direct sums:",
     MENU {
	  TO (quote ^,Module,Array),
	  TO (quote _,Module,Array)
	  },
     SEEALSO directSum
     }

document { (quote ++,Matrix,Matrix),
     TT "f++g", " -- computes the direct sum of two maps between modules.",
     PARA,
     "If an argument is a ring element or integer, it is promoted
     to a one by one matrix.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "vars R ++ transpose vars R",
      	  "oo^[1]",
      	  "a++b++c",
	  },
     "Selecting rows or columns of blocks:",
     MENU {
	  TO (quote ^,Matrix,Array),
	  TO (quote _,Matrix,Array)
	  },
     SEEALSO {directSum, (quote |, Matrix, Matrix), (quote ||, Matrix, Matrix)}
     }

document { quote directSum,
     TT "directSum(M,N,...)", " -- forms the direct sum of matrices or modules.",
     PARA,
     "The components can be recovered later with ", TO "components", ".",
     PARA,
     "Projection and inclusion maps for direct sums:",
     MENU {
	  TO (quote ^,Module,Array),
	  TO (quote _,Module,Array),
	  TO (quote ^,Matrix,Array),
	  TO (quote _,Matrix,Array)
	  },
     PARA,
     "It sometimes happens that the user has indices for the components of
     a direct sum preferable to the usual consecutive small integers.  In 
     this case the preferred indices can be specified with code
     like ", TT "directSum(a=>M,b=>N,...)", ", as in the following example.",
     EXAMPLE {
	  ///F = directSum(a=>ZZ^1, b=>ZZ^2, c=>ZZ^3)///,
	  ///F_[b]///,
	  ///F^[c]///,
	  },
     "Similar syntax works with ", TO "++", ".",
     EXAMPLE {
	  ///F = (a => ZZ^1) ++ (b => ZZ^2)///,
	  ///F_[b]///,
	  },
     SEEALSO {"++", "components", "indexComponents", "indices"}
     }

document { quote indexComponents,
     TT "indexComponents", " -- a symbol used as a key in a direct sum
     under which to store a hash table in which to register preferred keys used
     to index the components of the direct sum.",
     PARA,
     SEEALSO {"directSum", "components", "indices"}
     }

document { quote indices,
     TT "indices", " -- a symbol used as a key in a direct sum
     under which to store a list of the preferred keys used
     to index the components of the direct sum.",
     PARA,
     SEEALSO {"directSum", "components", "indexComponents"}
     }

Matrix ++ ZZ :=
Matrix ++ RingElement := (f,r) -> f ++ matrix {{r}}

ZZ ++ Matrix :=
RingElement ++ Matrix := (r,f) -> matrix {{r}} ++ f

ZZ ++ RingElement :=
RingElement ++ ZZ :=
RingElement ++ RingElement := (r,s) -> matrix {{r}} ++ matrix {{s}}

concatCols := mats -> (
     mats = select(toList mats,m -> m =!= null);
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
     mats = select(toList mats,m -> m =!= null);
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

concatBlocks := mats -> (
     if not isTable mats then error "expected a table of matrices";
     if #mats === 1
     then concatCols mats#0
     else if #(mats#0) === 1
     then concatRows (mats/first)
     else (
     	  samering flatten mats;
	  sources := unique applyTable(mats,source);
	  N := sources#0;
	  if not all(sources, F -> F == N) and not all(sources, F -> all(F,isFreeModule))
	  then error "unequal sources";
	  targets := unique transpose applyTable(mats,target);
	  M := targets#0;
	  if not all(targets, F -> F == M) and not all(targets, F -> all(F,isFreeModule))
	  then error "unequal targets";
     	  ggConcatBlocks(
	       Module.directSum (mats/first/target),
	       Module.directSum (mats#0/source),
	       mats)))

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
     rows = toList splice rows;
     listZ rows;
     cols = toList splice cols;
     listZ cols;
     sendgg(ggPush m, 
	  ggINTARRAY, gg rows, ggINTARRAY, gg cols, ggsubmatrix);
     getMatrix ring m)

submatrix(Matrix,List) := (m,cols) -> (
     cols = toList splice cols;
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
     EXAMPLE {
	  "R = ZZ/101[a .. o]",
      	  "m = genericMatrix(R, a, 3, 5)",
      	  "submatrix(m, {1,2,0}, {0..2, 4})",
	  },
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
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "m = genericMatrix(R,a,2,2)",
      	  "diff(transpose m,m*m)",
	  },
     PARA,
     "The most common usage of this function is when m : F <--- R^1
     and n : R^1 <--- G.  In this case the result is a matrix with shape
     diff(m,n) : F' <--- G, and the (i,j) th entry is the result of
     differentiating n_j by the differential operator corresponding to m_i.",
     EXAMPLE {
	  "m = matrix {{a,b,c,d}}",
      	  "n = matrix {{a^2, (b + c)*(a + d), a*b*c}}",
      	  "p = diff(transpose m,n)",
      	  "target p",
      	  "source p",
	  },
     PARA,
     "As another example, we show how to compute the Wronskian of a
     polynomial f.",
     EXAMPLE {
	  "R = ZZ/101[a, x .. z]",
      	  "f = matrix {{x^3 + y^3 + z^3 - a*x*y*z}}",
      	  "v = matrix {{x,y,z}}",
      	  "W = diff(transpose v * v, f)",
      	  "Wf = minors(3,W)",
	  },
     PARA,
     SEEALSO { "contract", "jacobian" }
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
     EXAMPLE {
	  "R = ZZ/101[a..c]",
      	  "diff(transpose matrix {{a,b,c}}, matrix {{(a+b+c)^3, a^2 * b^3 * c^2}})",
	  },
     PARA,
     "As another example, the Sylvester resultant between homogeneous polynomials
     f(x,y) and g(x,y) can be found in the following way.",
     EXAMPLE {
	  "R = (ZZ/101[a,b])[x,y]",
      	  "f = a * x^3 + b * x^2 * y + y^3",
      	  "g = b * x^3 + a * x * y^2 + y^3",
	  },
     "Multiply each of these by all quadrics, obtaining a set of elements in
     degree 5:",
     EXAMPLE "n = matrix {{f,g}} ** symmetricPower(2,vars R)",
     "Now create the matrix of coefficients by using contract against all
     monomials of degree 5 in x and y.",
     EXAMPLE {
	  "M = contract(transpose symmetricPower(5,vars R), n)",
      	  "Resfg = minors(6, M)",
	  },
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
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "I = monomialCurve(R,{1,3,4})",
      	  "A = R/I",
      	  "jacobian A",
	  },
     "For a one row matrix, the derivatives w.r.t. all the variables
     is given",
     EXAMPLE {
	  "R = ZZ/101[a..c]",
      	  "p = symmetricPower(2,vars R)",
      	  "jacobian p",
	  },
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
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "leadTerm (3 + 8*a^2*b + 7*b*c^2)",
      	  "leadTerm matrix {{a,b},{c,d}}",
      	  "leadTerm matrix {{c,d},{a,b}}",
	  },
     SEEALSO {"leadCoefficient", "leadMonomial", "leadComponent"}
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
  EXAMPLE {
       "R = ZZ/101[a..e]",
       "borel matrix {{a*d*e, b^2}}"
       }
  }

--------------------------------------------------------------------------
------------------------ matrix and map for modules ----------------------
--------------------------------------------------------------------------

mopts := Options => {
     Degree => null
     }

matrix = method mopts
map = method mopts

map(Module,Module) := options -> (M,N) -> (
     F := ambient N;
     if F == ambient M
     then map(M,N,
	  if M.?generators 
	  then map(M,N,generators N // generators M) -- sigh, should we check for zero remainder?
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

map(Module,Module,RingElement) := options -> (M,N,r) -> (
     R := ring M;
     if r == 0 then (
	  f := new Matrix;
	  f.handle = newHandle(ggPush cover M, ggPush cover N, ggzeromat);
	  f.source = N;
	  f.target = M;
	  f.ring = ring M;
	  f)
     else if numgens cover M == numgens cover N then map(M,N,r * id_(cover M)) 
     else error "expected 0, or source and target with same number of generators")

map(Module,Module,ZZ) := options -> (M,N,i) -> (
     if i === 0 then (
	  R := ring M;
	  f := new Matrix;
	  f.handle = newHandle(ggPush cover M, ggPush cover N, ggzeromat);
	  f.source = N;
	  f.target = M;
	  f.ring = ring M;
	  f)
     else if M == N then map(M,i)
     else if numgens cover M == numgens cover N then map(M,N,i * id_(cover M)) 
     else error "expected 0, or source and target with same number of generators")

map(Module,RingElement) := options -> (M,r) -> (
     R := ring M;
     try r = r + R#0
     else error "encountered scalar of unrelated ring";
     if r == 0 then map(M,M,0)
     else if r == 1 then map(M,1)
     else r * (map(M,1)))

map(Module) := options -> (M) -> (
     R := ring M;
     f := new Matrix;
     sendgg(ggPush cover M, ggiden);
     if options.Degree =!= null 
     then sendgg(ggdup, ggPush options.Degree, ggsetshift);
     reduce M;
     f.handle = newHandle "";
     f.source = f.target = M;
     f)

map(Module,ZZ) := options -> (M,i) -> (
     if i === 0 then map(M,M,0)
     else if i === 1 then map(M,options)
     else i * map M)

map(Module,Matrix) := options -> (M,f) -> (
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

map(Module,Module,Matrix) := options -> (M,N,f) -> (
     if M === f.target and N === f.source
     and (options.Degree === null or options.Degree === degree f)
     then f
     else (
	  R := ring M;
	  N' := cover N ** R;
	  sendgg (ggPush cover M, ggPush N', ggPush f,
	       ggPush (
		    if options.Degree === null
		    then toList (degreeLength R : 0)
		    else degreeCheck(options.Degree, R)),
	       ggmatrix);
	  reduce M;
	  newMatrix(M,N)))

inducedMap = method (
     Options => {
	  Verify => true,
	  Degree => null 
	  })
inducedMap(Module,Module,Matrix) := options -> (M,N,f) -> (
     sM := target f;
     sN := source f;
     if ambient M != sM
     then error "'inducedMap' expected target of map to be a subquotient of target module provided";
     if ambient N != sN
     then error "'inducedMap' expected source of map to be a subquotient of source module provided";
     g := f * generators N;
     h := generators M;
     p := map(M, N, g // h, Degree => options.Degree);
     if options.Verify then (
	  if g % h != 0
	  then error "'inducedMap' expected matrix to induce a map";
	  if not isWellDefined p
	  then error "'inducedMap' expected matrix to induce a well-defined map";
	  );
     p)
inducedMap(Module,Nothing,Matrix) := o -> (M,N,f) -> inducedMap(M,source f, f,o)
inducedMap(Nothing,Module,Matrix) := o -> (M,N,f) -> inducedMap(target f,N, f,o)
inducedMap(Nothing,Nothing,Matrix) := o -> (M,N,f) -> inducedMap(target f,source f, f,o)

inducedMap(Module,Module) := o -> (M,N) -> (
     if ambient M != ambient N 
     then error "'inducedMap' expected modules with same ambient free module";
     inducedMap(M,N,id_(ambient N),o))

document { quote inducedMap,
     TT "inducedMap(M,N,f)", " -- produce the map from ", TT "N", " to ", TT "M", " 
     induced by ", TT "f", ".",
     PARA,
     "Here ", TT "M", " should be a subquotient module of the target of ", TT "f", ", and
     ", TT "N", " should be a subquotient module of the source of ", TT "f", ".",
     PARA,
     "Options: ",
     MENU {
	  TO (inducedMap => Verify),
	  TO (inducedMap => Degree)
	  },
     SEEALSO "inducesWellDefinedMap"
     }

document { (inducedMap => Degree),
     TT "Degree => n", " -- an option to ", TO "inducedMap", " that provides the
     degree of the map produced."
     }

document { quote Verify,
     TT "Verify", " -- an option that can be used to request verification
     that a map is well defined.",
     PARA,
     MENU {
	  TO (inducedMap => Verify)
	  }
     }

document { (inducedMap => Verify),
     TT "Verify => true", " -- an option for ", TO "inducedMap", " which
     requests verification that the induced map produced is well defined."
     }


inducesWellDefinedMap = method()
inducesWellDefinedMap(Module,Module,Matrix) := (M,N,f) -> (
     sM := target f;
     sN := source f;
     if ambient M =!= sM
     then error "'inducesWellDefinedMap' expected target of map to be a subquotient of target module provided";
     if ambient N =!= sN
     then error "'inducesWellDefinedMap' expected source of map to be a subquotient of source module provided";
     (f * generators N) % (generators M) == 0
     and
     (f * relations N) % (relations M) == 0)     
inducesWellDefinedMap(Module,Nothing,Matrix) := (M,N,f) -> inducesWellDefinedMap(M,source f,f)
inducesWellDefinedMap(Nothing,Module,Matrix) := (M,N,f) -> inducesWellDefinedMap(target f,N,f)
inducesWellDefinedMap(Nothing,Nothing,Matrix) := (M,N,f) -> true

document { quote inducesWellDefinedMap,
     TT "inducesWellDefinedMap(M,N,f)", " -- tells whether the matrix ", TT "f", " would
     induce a well defined map from ", TT "N", " to ", TT "M", ".",
     SEEALSO "inducedMap"
     }

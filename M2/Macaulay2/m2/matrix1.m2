--		Copyright 1993-1998 by Daniel R. Grayson

matrix(Ring,List) := options -> (R,m) -> (
     if not isTable m then error "expected a table";
     map(R^#m,,m,options))

map(Module,Module,Function) := options -> (M,N,f) -> (
     map(M,N,table(numgens M, numgens N, f))
     )

document { (map,Module,Module,Function),
     TT "map(M,N,f)", " -- creates a map from the module N to the module M whose
     matrix entries are obtained from the function f by evaluating f(i,j)"
     }

map(Matrix) := options -> (f) -> (
     if options.Degree === null then f
     else (
     	  R := ring source f;
	  d := options.Degree;
	  if class d === ZZ then d = {d};
     	  map(target f, source f ** R^{d - degree f}, f, options)))

map(Module,ZZ,Function) := options -> (M,n,f) -> map(M,n,table(numgens M,n,f),options)
map(Module,ZZ,List) := options -> (M,rankN,p) -> (
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

map(Module,Nothing,Matrix) := options -> (M,nothing,p) -> (
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

map(Module,Nothing,List) := map(Module,Module,List) := options -> (M,N,p) -> (
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
	       	    then toList (degreeLength R:0)
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

Matrix.matrix = options -> (f) -> concatBlocks f

matrixTable := options -> (f) -> (
     types := unique apply(flatten f, class);
     if # types === 1 then (
	  type := types#0;
	  if instance(type,Ring) then (
	       R := type;
	       map(R^#f,, f, options))
	  else if type.?matrix then (type.matrix options)(f)
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
			 r := f#i#j;
			 if class r === Matrix then (
			      if tars#?i and tars#i != target r
			      then error "matrices not compatible";
			      tars#i = target r;
			      if srcs#?i and srcs#i != source r
			      then error "matrices not compatible";
			      srcs#j = source r;
			      ))));
	  scan(m, i->scan(n, j-> (
			 r := f#i#j;
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
			 r := f#i#j;
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
	  mm := concatBlocks f;
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
  "Various ways to use ", TT "matrix", ":",
  MENU {
       TO (matrix, List),
       TO (matrix,Matrix),
       TO (matrix,Ring,List)
       },
  "Optional arguments, valid with each form above:",
  MENU {
       (TO "Degree", " -- specify the degree of the resulting map."),
       },
  RETURNS "Matrix",
  SEEALSO {"map"}
  }

document { "making module maps",
     "There are several different ways to use ", TO "map", " to make maps
     maps between modules.  In all case, if a matrix is provided, and the
     modules are subquotient modules, then the matrix is understood to be
     formed with respect to generators of the subquotient modules.",
     PARA,
     MENU {
	  TO (map,Matrix),
	  TO (map,Module),
       	  TO (map,Module,Module),
       	  TO (map,Module,Module,List),
       	  TO (map,Module,Module,Function),
       	  TO (map,Module,Module,Matrix),
       	  TO (map,Module,RingElement),
       	  TO (map,Module,Nothing,List),
       	  TO (map,Module,ZZ,List),
       	  TO (map,Module,ZZ,Function),
       	  TO (map,Module,Matrix),
	  TO (map,Module,Module,RingElement),
	  TO (map,Module,Module,ZZ),
	  TO (map,ChainComplex,ChainComplex,Function),
	  },
     SEEALSO {"map", "matrix"}
     }

matrix(Matrix) := options -> (m) -> (
     if isFreeModule target m and isFreeModule source m
     and ring source m === ring target m
     then m
     else map(cover target m, cover source m ** ring target m, m, Degree => degree m)
     )

document { (map,Matrix),
     TT "map(f, Degree => d)", " -- make a map of degree d from a map f
     of modules by tensoring the source module with a free module of
     rank 1 and appropriate degree."
     }

document { (matrix,Matrix),
     TT "matrix f", " -- produce the matrix of a map f.",
     PARA,
     "If the source and target of f are free, then the result is
     f itself.  Otherwise, the source and target will be replaced by
     the free modules whose basis elements correspond to the generators
     of the modules.",
     SEEALSO {"map", "matrix"}
     }

document { (matrix,Ring,List),
     TT "matrix(R,v)", " -- create a matrix over R from a doubly-nested list of
     ring elements or matrices.",
     PARA,
     "This is essentially the same as ", TO (matrix,List), " together with
     the specification of the ring.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..f]",
      	  "matrix(R, {{a,b,0},{d,0,f}})",
	  },
     SEEALSO {"map", "matrix"}
     }

document { (map,Module,Module),
     TT "map(M,N)", " -- constructs the natural map from N to M.",
     PARA,
     "The modules M and N should be subquotient modules of the same
     free module",
     SEEALSO {"map", "isWellDefined"}
     }

document { (map,Module,Matrix),
     TT "map(M,p)", " -- recasts a matrix p to a map whose target is M by
     tensoring p with a graded free module of rank 1.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "p = matrix{{x,y}}",
      	  "q = map(R^{3},p)",
      	  "degrees target q",
      	  "degrees source q",
	  },
     SEEALSO {"map", "matrix"}
     }

document { (map,Module,Module,List),
     TT "map(M,N,v)", " -- produces a map (matrix) from the module N
     to the module M whose entries are obtained from the doubly-nested list
     v of ring elements.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = map(R^2,R^{-2,-2},{{x^2,0},{0,y^2}})",
      	  "isHomogeneous p",
	  },
     SEEALSO {"map", "matrix"}
     }
document { (map,Module,Module,Matrix),
     TT "map(M,N,p)", " -- recasts the matrix p as a map (matrix) from
     the module N to the module M.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = matrix {{x,y,z}}",
      	  "q = map(R^1,R^3,p)",
      	  "degrees source p",
      	  "degrees source q",
	  },
     SEEALSO {"map", "matrix"}
     }
document { (map,Module,Module,RingElement),
     TT "map(M,N,r)", " -- construct a map from a module ", TT "N", " to ", TT "M", " which is provided
     by the ring element ", TT "r", ".",
     PARA,
     "If ", TT "r", " is nonzero, then ", TT "M", " and ", TT "N", " should be equal, 
     or at least have the same number of generators.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "map(R^2,R^3,0)",
      	  "map(R^2,R^2,x)",
      	  "q = map(R^2,R^2,x,Degree=>1)",
      	  "isHomogeneous q",
	  },
     PARA,
     SEEALSO {(map,Module,Module,ZZ), "map", "matrix"}
     }
document { (map,Module,Module,ZZ),
     TT "map(M,N,k)", " -- construct a map from a module ", TT "N", " to ", TT "M", " 
     which is provided by the integer ", TT "k", ".",
     PARA,
     "If ", TT "k", " is ", TT "0", ", then the zero map is constructed.  If ", TT "k", " is 1,
     then ", TT "M", " and ", TT "N", " should have the same number and degrees of generators 
     in the sense that the modules ", TT "cover M", " and ", TT "cover N", " are equal, and then the map
     which sends the ", TT "i", "-th generator of ", TT "N", " to the ", TT "i", "-th generator 
     of ", TT "M", " is constructed (and it may not be well-defined).
     Otherwise, ", TT "M", " and ", TT "N", " should be equal, or 
     at least have the same number of generators.",
     PARA,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = image vars R",
	  "N = coker presentation M",
	  "f = map(M,N,1)",
	  "isWellDefined f",
	  "isIsomorphism f",
	  "g = map(M,cover M,1)",
	  "isWellDefined g",
	  "isIsomorphism g",
	  "h = map(cover M,M,1)",
	  "isWellDefined h",
	  },
     PARA,
     SEEALSO {(map,Module,Module,RingElement), "map", "matrix"}
     }
document { (map,Module),
     TT "map M", " -- construct the identity map from M to itself.",
     PARA,
     "This can also be accomplished with ", TT "id_M", " or ", TT "map(M,1)", ".",
     SEEALSO {"map", "id"}
     }
document { (map,Module,RingElement),
     TT "map(M,r)", " -- construct the map from M to itself which is provided
     by scalar multiplication by the ring element r.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "map(R^2,x)",
	  },
     SEEALSO {"map", "matrix"}
     }
document { quote Degree,
     TT "Degree => d", " -- an optional argument to ", TO "matrix", " that
     specifies that the degree of the map created should be ", TT "d", ".",
     PARA,
     "The degree may be an integer or a list of integers (multidegree).  The
     length of the list should be the same as the length of a degree for the
     ring, see ", TO "degreeLength", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "p = map(R^1, R^1, {{x^4}})",
      	  "isHomogeneous p",
      	  "q = map(R^1, R^1, {{x^4}}, Degree => 4)",
      	  "isHomogeneous q",
	  },
     SEEALSO {"map", "matrix", (inducedMap => Degree)}
     }

document { (map,Module,ZZ,Function),
     TT "map(M,n,f)", " -- construct a map from a free graded module of
     rank n to M whose entries are obtained from the function f by 
     evaluating f(i,j).",
     PARA,
     "The degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero."
     }

document { (map,Module,ZZ,List),
     TT "map(M,n,v)", " -- construct a map from a free graded module of
     rank n to M whose entries are in the doubly nested list v.",
     PARA,
     "The degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero."
     }

document { (map,Module,Nothing,List),
     TT "map(M,,v)", " -- construct a map from a free graded module to M
     whose entries are obtained from the doubly-nested list v of
     ring elements.",
     PARA,
     "The absence of the second argument indicates that the source of the map
     is to be a free module constructed with an attempt made to assign degrees
     to its basis elements so as to make the map homogeneous of degree zero.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "f = map(R^2,,{{x^2,y^2},{x*y,0}})",
      	  "degrees source f",
      	  "isHomogeneous f",
	  },
     SEEALSO {"map", "matrix"}
     }

matrix(List) := options -> (m) -> (
     if #m === 0 then error "expected nonempty list";
     m = apply(splice m,splice);
     types := unique apply(m,class);
     if #types === 1 then (
	  type := types#0;
	  if instance(type,Module) 
	  then map(type,,table(numgens type, #m, (i,j) -> m_j_i))
	  else if type === List then (
	       if isTable m then (matrixTable options)(m)
	       else error "expected rows all to be the same length"
	       )
	  else error "expected a table of ring elements or matrices")
     else error "expected a table of ring elements or matrices")
document { (matrix,List),
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
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = matrix {{x,y,z}}",
      	  "degrees source p",
      	  "isHomogeneous p",
	  },
     "Notice that the degrees were set up so that p is homogeneous, because
     the source module is not explicitly specified by the user.  The next
     example involves block matrices.",
     EXAMPLE {
	  "q = vars R",
      	  "matrix {{q,q,q}}",
      	  "matrix {{q},{q},{q}}",
	  },
     "Here we construct a matrix from column vectors.",
     EXAMPLE {
	  "F = R^3",
      	  "matrix {F_2, F_1, x*F_0 + y*F_1 + z*F_2}",
	  },
     SEEALSO {"map", "matrix"}
     }

--------------------------------------------------------------------------

Module#id = (M) -> map(M,1)

document { quote id,
     TT "id_M", " -- the identity homomorphism from M to M.",
     PARA,
     "M may be a ", TO "Module", " or a ", TO "ChainComplex", ".",
     PARA,
     SEEALSO{"Matrix", "ChainComplexMap", "ScriptedFunction"}
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
     R := ring m;
     F := target m;
     G := source m;
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     if numgens F === 1 
     then m
     else reshape(R^1, G ** dual F ** R^{- degree m}, m))

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
     M.numgens = (sendgg (ggPush M.handle, gglength); eePopInt());
     M#0 = (sendgg(ggPush M, ggzero); new M);
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
     M.numgens = (sendgg (ggPush M.handle, gglength); eePopInt());
     M#0 = (sendgg(ggPush M, ggzero); new M);
     M)
subquotient(Matrix,Matrix) := (subgens,relns) -> (
     E := target subgens;
     if E != target relns then error "expected maps with the same target";
     M := new Module of Vector;
     M.ring = ring subgens;
     M.handle = handle E;
     M.numgens = (sendgg (ggPush M.handle, gglength); eePopInt());
     M#0 = ( sendgg(ggPush M, ggzero); new M);
     if M == 0 then M
     else (
	  relns = matrix relns;
	  subgens = matrix subgens;
	  if E.?generators then (
	       relns = E.generators * relns;
	       subgens = E.generators * subgens;
	       );
	  if E.?relations then relns = relns | E.relations;
	  M.generators = subgens;
	  if relns != 0 then M.relations = relns;
	  M))
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
	  {TO "generators", " -- recover the generators"},
	  {TO "relations", "  -- recover the relations"},
	  {TO "prune", "      -- convert to a module with presentation"}
	  },
     "This is the general form in which modules are represented, and
     subquotient modules are often returned as values of computations.",
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "M = kernel vars R ++ cokernel vars R",
      	  "generators M",
      	  "relations M",
      	  "prune M",
	  },
     SEEALSO {"generators", "relations"}
     }

Matrix ** Matrix := (f,g) -> (
     R := ring target f;
     if ring target g =!= R 
     or ring source g =!= ring source f
     then error "expected matrices over the same ring";
     sendgg (ggPush f, ggPush g, ggtensor);
     h := getMatrix R;
     map(target f ** target g, source f ** source g, h, Degree => degree f + degree g))

document { (quote **, Matrix, Matrix),
     TT "f ** g", " -- computes the tensor product of two matrices.",
     PARA,
     SEEALSO "Matrix"
     }

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

compactMatrixForm = true

document { quote compactMatrixForm,
     TT "compactMatrixFormat", " -- a global flag which specifies whether to display
     matrices in compact form.",
     PARA,
     "The default value is ", TT "true", ".  The compact form is the form used by
     ", ITALIC "Macaulay", ", in which the multiplication and exponentiation operators
     are suppressed from the notation.",
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "f = random(R^{2},R^2)",
	  "compactMatrixForm = false;",
	  "f"
	  }
     }

net Matrix := f -> (
     if f == 0 
     then "0"
     else if compactMatrixForm then (
	  R := ring target f;
	  m := verticalJoin toSequence apply(
	       lines sendgg(ggPush f,ggsee,ggpop), x -> concatenate("| ",x,"|"));
	  if degreeLength R > 0 -- and isHomogeneous f
	  then m = horizontalJoin(verticalJoin(degrees cover target f / name), " ", m);
	  m)
     else net expression f				    -- add row labels somehow
     )

image Matrix := f -> (
     if f.?image then f.image else f.image = subquotient(f,)
     )
coimage Matrix := f -> (
     if f.?coimage then f.coimage else f.coimage = cokernel map(source f, kernel f)
     )
cokernel Matrix := m -> (
     if m.?cokernel then m.cokernel else m.cokernel = subquotient(,m)
     )

cokernel RingElement := f -> cokernel matrix {{f}}
image RingElement := f -> image matrix {{f}}

Ideal = new Type of MutableHashTable
expression Ideal := (I) -> new FunctionApplication from { 
     ideal,
     (
	  v := expression toSequence first entries generators I;
     	  if #v === 1 then v#0 else v
	  )
     }
net Ideal := (I) -> (
     if numgens I === 0 then "0"
     else net expression I
     )
name Ideal := (I) -> name expression I

isHomogeneous Ideal := (I) -> isHomogeneous I.generators
genera(Ideal) := (I) -> genera module I
euler(Ideal) := (I) -> euler module I

RingElement * Ideal := (r,I) -> ideal (r ** generators I)
ZZ * Ideal := (r,I) -> ideal (r * generators I)

generators Ideal := (I) -> I.generators
mingens Ideal := options -> (I) -> mingens(module I,options)
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
Ideal + Ideal := (I,J) -> ideal (generators I | generators J)
document { (quote +, Ideal, Ideal), 
     TT "I + J", " -- the sum of two ideals."
     }
degree Ideal := I -> degree cokernel generators I
trim Ideal := options -> (I) -> ideal trim(module I, options)
map(Ideal) := options -> (I) -> map(module I,options)
map(Ideal,Ideal) := options -> (I,J) -> map(module I,module J,options)
Ideal _ ZZ := (I,n) -> (generators I)_(0,n)
Matrix % Ideal := (f,I) -> f % gb I
numgens Ideal := (I) -> numgens source generators I
leadTerm Ideal := (I) -> leadTerm generators gb I
leadTerm(ZZ,Ideal) := (n,I) -> leadTerm(n,generators gb I)
jacobian Ideal := (I) -> jacobian generators I
poincare Ideal := (I) -> poincare module I
hilbertPolynomial Ideal := options -> (I) -> hilbertPolynomial(module I,options)

protect quote Order
assert( class infinity === InfiniteNumber )
hilbertSeries = method(Options => {
     	  Order => infinity
	  }
     )

hilbertSeries Ideal := options -> (I) -> hilbertSeries(module I,options)

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
	  TO "Grassmannian",
	  TO "ideal",
	  TO "quotient"
	  },
     "Operations on ideals:",
     MENU {
	  TO (quote +,Ideal,Ideal),
	  TO (quote *,Ideal, Ideal),
	  TO (quote ^,Ideal, ZZ),
	  TO "codim",
	  TO "decompose",
	  TO "dim",
	  TO "Fano",
	  TO "module",
	  TO "radical",
	  TO "removeLowestDimension",
	  TO "top"
	  }
     }

document { (quote *,Ideal,Ideal),
     TT "I * J", " -- the product of two ideals."
     }

document { (quote ^,Ideal,ZZ),
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
	  -- if isHomogeneous I and isHomogeneous J  -- can be removed later
	  -- then gb I == gb J 
	  -- else
	  isSubset(I,J) and isSubset(J,I)	  -- can be removed later
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
     R := ring f;
     if not isFreeModule target f or not isFreeModule source f 
     then error "expected map between free modules";
     f = flatten f;			  -- in case there is more than one row
     if target f != R^1 then (
     	  f = map(R^1,,f);
	  )
     else if not isHomogeneous f and isHomogeneous R then (
     	  g := map(R^1,,f);			  -- in case the degrees are wrong
     	  if isHomogeneous g then f = g;
	  );
     new Ideal from { quote generators => f, quote ring => R } )

ideal Module := (M) -> (
     F := ambient M;
     if isSubmodule M and rank F === 1 then ideal generators M
     else error "expected a submodule of a free module of rank 1"
     )
ideal List := ideal Sequence := v -> ideal matrix {toList v}
submodule List := submodule Sequence := v -> image matrix toList v
ideal RingElement := v -> ideal {v}
submodule(Vector) := (v) -> image matrix {v}
ideal ZZ := v -> ideal {v}
ideal QQ := v -> ideal {v}

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
     EXAMPLE {
	  "ZZ[a..i]",
      	  "ideal (c..h)"
	  },
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

kernel Matrix := options -> (g) -> if g.?kernel then g.kernel else g.kernel = (
     N := source g;
     P := target g;
     g = matrix g;
     if P.?generators then g = P.generators * g;
     h := modulo(g, if P.?relations then P.relations);
     if N.?generators then h = N.generators * h;
     subquotient( h, if N.?relations then N.relations))

kernel RingElement := options -> (g) -> kernel (matrix {{g}},options)

homology(Matrix,Matrix) := opts -> (g,f) -> (
     R := ring f;
     M := source f;
     N := target f;
     P := target g;
     if source g != N then error "expected maps to be composable";
     f = matrix f;
     if not all(degree f, i -> i === 0) then f = map(target f, source f ** R^{-degree f}, f);
     g = matrix g;
     if P.?generators then g = P.generators * g;
     h := modulo(g, if P.?relations then P.relations);
     if N.?generators then (
	  f = N.generators * f;
	  h = N.generators * h;
	  );
     subquotient(h, if N.?relations then f | N.relations else f))

document { (homology,Matrix,Matrix),
     TT "homology(g,f)", " -- computes the homology module ", TT "ker g/im f", ".",
     PARA,
     "Here ", TT "g", " and ", TT "f", " should be composable maps with ", TT "g*f", "
     equal to zero.",
     SEEALSO "homology"
     }

Hom(Matrix, Module) := (f,N) -> (
     if isFreeModule source f and isFreeModule target f
     then transpose f ** N
     else notImplemented())

Hom(Module, Matrix) := (N,f) -> (
     if isFreeModule N 
     then dual N ** f
     else notImplemented())

dual(Matrix) := f -> (
     R := ring f;
     Hom(f,R^1)
     )
document { (dual, Matrix),
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
document { (quote ^,Matrix,Array),
     TT "f^[i,j,k]", " -- extract some rows of blocks from a matrix ", TT "f", ".",
     PARA,
     "The target of ", TT "f", " should be a direct sum, and the result is obtained by
     composition with the projection onto the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA,
     EXAMPLE {
	  "f = map(ZZ^2 ++ ZZ^2, ZZ^2, {{1,2},{3,4},{5,6},{7,8}})",
      	  "f^[0]",
      	  "f^[1]",
      	  "f^[1,0]",
	  },
     SEEALSO {submatrix, (quote ^,Module,Array), (quote _,Matrix,Array)}
     }

document { (quote _,Matrix,Array),
     TT "f_[i,j,k]", " -- extract some columns of blocks from a matrix ", TT "f", ".",
     PARA,
     "The source of ", TT "f", " should be a direct sum, and the result is obtained by
     composition with the inclusion into the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA,
     EXAMPLE {
	  "f = map(ZZ^2 ++ ZZ^2, ZZ^2, {{1,2},{3,4},{5,6},{7,8}})",
      	  "f^[0]",
      	  "f^[1]",
      	  "f^[1,0]",
	  },
     SEEALSO {submatrix, (quote _,Module,Array), (quote ^,Matrix,Array)}
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
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = matrix {{x^2,y^2},{x*y*z, x^3-y^3}}",
      	  "entries p"
	  },
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
--assert( ff_0 == vector {a,b,c} )
--assert( ff_1 == vector {d,e,f} )
--assert( ff_2 == vector {g,h,i} )
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

cover(Matrix) := (f) -> matrix f

rank Matrix := (f) -> rank image f

erase quote reduce
erase quote newMatrix
erase quote concatRows
erase quote concatCols
erase quote samering
erase quote ggConcatBlocks

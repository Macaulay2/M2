------------------------------------------------------------
-- Code to test, at a low level, the new engine code
------------------------------------------------------------
clone = method()
targ = method()
src = method()
matrixType = method()
symm = method()
exterior = method()
submod = method()
inducedOrder = method()
degreeWeights = method()
isGraded = method()
getTerms = method()
exteriorProduct = method()
modtensor = method()

if ECoefficientRing === quote ECoefficientRing then
  ECoefficientRing = new Type of MutableHashTable
if MonOrder === quote MonOrder then
  MonOrder = new Type of MutableHashTable
if EMonoid === quote EMonoid then
  EMonoid = new Type of MutableHashTable
if ERing === quote ERing then
  ERing = new Type of MutableHashTable
if EFreeModule === quote EFreeModule then
  EFreeModule = new Type of MutableHashTable
if ERingElement === quote ERingElement then
  ERingElement = new Type of MutableHashTable
if EVector === quote EVector then
  EVector = new Type of MutableHashTable
if EMatrix === quote EMatrix then
  EMatrix = new Type of MutableHashTable
if ERingMap === quote ERingMap then
  ERingMap = new Type of MutableHashTable


------------------------------------------------------------
--BeforePrint ECoefficientRing := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint MonOrder := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint EMonoid := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint ERing := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint EFreeModule := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint ERingElement := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint EVector := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint EMatrix := (R) -> sendgg(ggPush R, ggsee)
--BeforePrint ERingMap := (R) -> sendgg(ggPush R, ggsee)
------------------------------------------------------------
net ECoefficientRing := (R) -> see R
net MonOrder := (R) -> see R
net EMonoid := (R) -> see R
net ERing := (R) -> see R
net EFreeModule := (R) -> see R 
net ERingElement := (R) -> see R
net EVector := (R) -> see R
net EMatrix := (R) -> see R
net ERingMap := (R) -> see R
------------------------------------------------------------
newECoefficientRing = () -> (
     v := new ECoefficientRing;
     v.handle = newHandle();
     v)

newMonOrder = () -> (
     v := new MonOrder;
     v.handle = newHandle();
     v)
     
newEMonoid = () -> (
     v := new EMonoid;
     v.handle = newHandle();
     v)

newERing = () -> (
     v := new ERing;
     v.handle = newHandle();
     v)

newEFreeModule = () -> (
     v := new EFreeModule;
     v.handle = newHandle();
     v)

newERingElement = () -> (
     v := new ERingElement;
     v.handle = newHandle();
     v)

newEVector = () -> (
     v := new EVector;
     v.handle = newHandle();
     v)

newEMatrix = () -> (
     v := new EMatrix;
     v.handle = newHandle();
     v)

newERingMap = () -> (
     v := new ERingMap;
     v.handle = newHandle();
     v)

engine = args -> (
     a := args_{0..#args-2};
     resulttype := args_(-1);
     callgg a;
     if resulttype === ZZ then
       eePopInt()
     else if resulttype === Boolean then
       eePopBool()
     else if resulttype === Intarray then
       eePopIntarray()
     else (
	  v := new resulttype;
	  v.handle = newHandle();
	  v))
------------------------------------------------------------
ZmodP = (p) -> engine(ggEcharp, p, ERing)
EZ = engine(ggEZZ, ERing)

degreeRing = (ndegs) -> (
    local names;
    if ndegs == 1 then names = "t"
	else 
      names = concatenate apply(ndegs, i -> " t" | i);
	D := emonoid(monomialOrder(GroupLex=>ndegs), toList(0..ndegs-1), names);
    polyring(ZmodP 32003, D, EZ, {}))

------------------------------------------------------------
monomialOrder = args -> (
     sendgg(ggMOinit);
     mo := newMonOrder();
     hasComponent := false;
     if class args =!= Sequence then
         args = sequence args;
     scan(args, val -> (
	 if class val === ZZ then (
	      sendgg(ggPush val, ggPush 0, ggPush mo, ggMOrevlex)
	      )
	 else if val === Component then (
	      if hasComponent then
	      	   error "At most one 'Component' allowed in monomial order"
	      else (
		   sendgg(ggPush mo,ggMOcomponent);
		   hasComponent = true;))
	 else if class val === Option then (
	      if val#0 === Lex then (
		   if class val#1 === ZZ then 
		       sendgg(ggPush val#1,ggPush 0, ggPush mo, ggMOlex)
		   else if class val#1 === List and all(val#1,i->class i === ZZ) then
		       sendgg(ggPush val#1,ggPush 0, ggPush mo,ggMOlex)
		   else
		       error "Expected Lex argument to be an integer or list of integers")
	      else if val#0 === GroupLex then (
		   if class val#1 === ZZ then 
		       sendgg(ggPush val#1, ggPush 1, ggPush mo,ggMOlex)
		   else if class val#1 === List and all(val#1,i->class i === ZZ) then
		       sendgg(ggPush val#1, ggPush 1, ggPush mo,ggMOlex)
		   else
		       error "Expected Lex argument to be an integer or list of integers")
	      else if val#0 === RevLex then (
		   if class val#1 === ZZ then 
		       sendgg(ggPush val#1,ggPush 0, ggPush mo,ggMOrevlex)
		   else if class val#1 === List and all(val#1,i->class i === ZZ) then
		       sendgg(ggPush val#1,ggPush 0, ggPush mo,ggMOrevlex)
		   else
		       error "Expected RevLex argument to be an integer or list of integers")
	      else if val#0 === NCLex then (
		   if class val#1 === ZZ then 
		       sendgg(ggPush val#1, ggPush mo,ggMONClex)
		   else
		       error "Expected NCLex argument to be an integer")
	      else if val#0 === Weights then (
		   if class val#1 === List and all(val#1,i->class i === ZZ) then
		       sendgg(ggPush val#1,ggPush mo,ggMOwtfcn)
		   else
		       error "Expected 'Weights' argument to be a list of integers"))));
    if not hasComponent then sendgg(ggPush mo, ggMOcomponent);
    mo)
     

clone MonOrder := (mo) -> engine(ggMOclone, mo, EMonOrder)

MonOrder ** MonOrder := (mo,mo2) -> (
     result := clone mo;
     sendgg(ggPush result,ggPush mo2,ggMOproduct);
     result)
------------------------------------------------------------
emonoid = (mo,printorder,names) -> engine(ggmonoid, mo, printorder, names, EMonoid)

stats EMonoid := (M) -> sendgg(ggPush M, ggstats)
------------------------------------------------------------
polyring = (K,M,ZD,degs) -> engine(ggpolyring, ZD, degs, K, M, ERing)

weyl = (K,M,comms,diffs,ZD,degs) -> engine(ggweylalgebra, ZD, degs, K, M, comms, diffs, -1, ERing)

weylhom = (K,M,comms,diffs,homvar,ZD,degs) ->
    engine(ggweylalgebra, ZD, degs, K, M, comms, diffs, homvar, ERing)
skewpolyring = (K,M,skews,ZD,degs) -> engine(ggskewpolyring, ZD, degs, K, M, skews, ERing)

-----------------
-- EFreeModule --
-----------------

ERing ^ ZZ                 := (R,n) -> engine(ggfree, R, n, EFreeModule)
ERing ^ List               := (R,a) -> engine(ggfree, R, a, EFreeModule)
ERing ^ EMatrix            := (R,m) -> engine(ggfree, R, m, EFreeModule)

rank EFreeModule           := (F) -> engine(ggrank, F, ZZ)
ring EFreeModule           := (F) -> engine(gggetring, F, ERing)
degrees EFreeModule        := (F) -> engine(ggdegree, F, Intarray)
inducedOrder EFreeModule   := (F) -> engine(gggetcols, F, EMatrix)
EFreeModule == EFreeModule := (F,G) -> engine(ggisequal, F, G, Boolean)
EFreeModule ++ EFreeModule := (F,G) -> engine(ggadd, F, G, EFreeModule)
EFreeModule ** EFreeModule := (F,G) -> engine(ggmult, F, G, EFreeModule)
dual EFreeModule           := (F) -> engine(ggtranspose, F, EFreeModule)
submod(EFreeModule,List)   := (F,a) -> engine(ggsubmodule, F, a, EFreeModule)
symm (ZZ, EFreeModule)     := (p,F) -> engine(ggsymm,F,p,EFreeModule)
exterior (ZZ, EFreeModule) := (p,F) -> engine(ggexterior,F,p,EFreeModule)
EFreeModule _ ZZ           := (F,x) -> engine(ggbasisElement,F,x,EVector)

EFreeModule _ Sequence := (F,arg) -> (
     if #arg =!= 2 then error "expected (Integer Array, Integer)";
     if class arg#0 =!= List and class arg#0 =!= Sequence
       then error "expected (Integer Array, Integer)";
     if not all(arg#0, i -> class i === ZZ)
       then error "expected (Integer Array, Integer)";
     if class arg#1 =!= ZZ 
       then error "expected (Integer Array, Integer)";
     engine(ggterm, F, toList(arg#0), arg#1, EVector))

-------------------
-- Ring Elements --
-------------------

ring ERingElement := (a) -> engine(gggetring, a, ERing)

ZZ _ ERing := (n,R) -> engine(ggfromint, R, n, ERingElement)

ERing _ Sequence := (R,a) -> (
     if class (a#1) === ZZ then
       engine(ggvar,a#0,a#1,R,ERingElement)
     else
       engine(ggterm,R,a#0,a#1,ERingElement))

ERing _ List := (R,a) -> (
     -- check: a is a list of integers of even length.
     one := 1_EZ;
     engine(ggterm, R, one, a, ERingElement))

ERingElement == ERingElement := (v,w) -> engine(ggisequal, v, w, Boolean)
ERingElement == ZZ := (v,n) -> (
     if n =!= 0 then error "cannot compare ring element to non-zero integer";
     sendgg(ggPush v, ggiszero);
     eePopBool())
- ERingElement := (v) -> engine(ggnegate, v, ERingElement)
ERingElement + ERingElement := (v,w) -> (
     sendgg(ggPush v, ggPush w, ggadd);
     newERingElement())
ERingElement - ERingElement := (v,w) -> (
     sendgg(ggPush v, ggPush w, ggsubtract);
     newERingElement())
ZZ * ERingElement := (n,v) -> (
     sendgg(ggPush n, ggPush v, ggmult);
     newERingElement())
ERingElement * ERingElement := (v,w) -> (
     sendgg(ggPush v, ggPush w, ggmult);
     newERingElement())
ERingElement ^ ZZ := (v,n) -> (callgg(ggpower,v,n); newERingElement())

leadCoefficient ERingElement := (v) -> (
    sendgg(ggPush v, ggleadcoeff);
    newERingElement())
leadMonomial ERingElement := (v) -> (
     sendgg(ggPush v, ggleadmonom);
     eePopIntarray())
leadTerm(ERingElement) := (v) -> (
    sendgg(ggPush v, ggPush (-1), ggleadterm);
    newERingElement())
leadTerm(ERingElement,ZZ) := (v,n) -> (
    sendgg(ggPush v, ggPush n, ggleadterm);
    newERingElement())

degree ERingElement := (v) -> (
    sendgg(ggPush v, ggdegree);
	eePopIntarray())
degreeWeights(ERingElement,List) := (v,wts) -> (
    -- check: wts is a vector of length = #vars in the ring.
    sendgg(ggPush v, ggPush wts, ggdegree);
    hi := eePopInt();
    lo := eePopInt();
    {lo, hi})
isGraded ERingElement := (v) -> (
    sendgg(ggPush v, ggishomogeneous);
    eePopBool())

size ERingElement := (v) -> (
    sendgg(ggPush v, gglength);
    eePopInt())
getTerms (ERingElement, ZZ, ZZ) := (v,lo,hi) ->
    engine(gggetterms, v, lo, hi, ERingElement)

promote(ERingElement, ERing) := (f,R) -> engine(ggpromote,R,f,ERingElement)
lift(ERingElement, ERing) := (f,R) -> engine(gglift,R,f,ERingElement)
-------------
-- EVector --
-------------
ambient EVector := (v) -> engine(gggetFreeModule, v, EFreeModule)
EVector == EVector := (v,w) -> engine(ggisequal, v, w, Boolean)
EVector == ZZ := (v,n) -> (
     if n =!= 0 then error "cannot compare vector to non-zero integer";
     sendgg(ggPush v, ggiszero);
     eePopBool())
- EVector := (v) -> (
     sendgg(ggPush v, ggnegate);
     newEVector())
EVector + EVector := (v,w) -> (
     sendgg(ggPush v, ggPush w, ggadd);
     newEVector())
EVector - EVector := (v,w) -> (
     sendgg(ggPush v, ggPush w, ggsubtract);
     newEVector())
ZZ * EVector := (n,v) -> (
     sendgg(ggPush n, ggPush v, ggmult);
     newEVector())
ERingElement * EVector := (f,w) -> (
     sendgg(ggPush f, ggPush w, ggmult);
     newEVector())
EVector * ERingElement := (v,f) -> (     
     sendgg(ggPush v, ggPush f, ggrightmult);
     newEVector())

EVector _ ZZ := (v,n) -> (
    sendgg(ggPush v, ggPush n, ggelem);
    newEVector())
getTerms (EVector, ZZ, ZZ) := (v,lo,hi) -> (
    sendgg(ggPush v, ggPush lo, ggPush hi, gggetterms);
    newEVector())
evector = (F,elems) -> (
    n := rank F;
	scan(n, i -> sendgg ggPush (elems#i));
	sendgg(ggPush F, ggvector);
	newEVector())
esparseVector = (F,elems) -> (
    n := #elems;
	scan(n, i -> sendgg ggPush (elems#i#0));
	r := apply(elems, i -> i#1);
	sendgg(ggPush F, ggPush r, ggsparsevector);
	newEVector())
size EVector := (v) -> (
    sendgg(ggPush v, gglength);
	eePopInt())
degree EVector := (v) -> (
    sendgg(ggPush v, ggdegree);
	eePopIntarray())
degreeWeights(EVector,List) := (v,wts) -> (
    -- check: wts is a vector of length = #vars in the ring.
    sendgg(ggPush v, ggPush wts, ggdegree);
    hi := eePopInt();
    lo := eePopInt();
    {lo, hi})
isGraded EVector := (v) -> (
    sendgg(ggPush v, ggishomogeneous);
    eePopBool())
leadComponent EVector := (v) -> (
    sendgg(ggPush v, ggleadcomp);
    eePopInt())
leadCoefficient EVector := (v) -> (
    sendgg(ggPush v, ggleadcoeff);
    newERingElement())
leadMonomial EVector := (v) -> (
     sendgg(ggPush v, ggleadmonom);
     eePopIntarray())
leadTerm(EVector) := (v) -> (
    sendgg(ggPush v, ggPush (-1), ggPush 1, ggleadterm);
    newEVector())
leadTerm(EVector,ZZ) := (v,n) -> (
    sendgg(ggPush v, ggPush n, ggPush 1, ggleadterm);
    newEVector())
leadTerm(EVector,ZZ,ZZ) := (v,n,samecomp) -> (
    sendgg(ggPush v, ggPush n, ggPush samecomp, ggleadterm);
    newEVector())
homogenize(EVector,ZZ,List) := (v,var,wts) -> (
    -- check: var is in range
    -- check: wts is of length #vars in ring
    sendgg(ggPush v, ggPush var, ggPush wts, gghomogenize);
    newEVector())
homogenize(EVector,List,List) := (v,var'd,wts) -> (
    -- check: var is in range
    -- check: wts is of length #vars in ring
    sendgg(ggPush v, ggPush var'd#0, ggPush var'd#1, ggPush wts, gghomogenize);
    newEVector())

promote(EVector, EFreeModule) := (v,F) -> engine(ggpromote,F,v,EVector)
lift(EVector, EFreeModule) := (v,F) -> engine(gglift,F,v,EVector)

--------------
-- ERingMap --
--------------
eringmap =               (m) -> engine(ggringmap, m, ERingMap)
ring ERingMap         := (f) -> engine(gggetring, f, ERing)
ERingMap ERingElement := (f,r) -> engine(ggev, f, r, ERingElement)
ERingMap EVector :=      (f,r) -> (
     F1 := ambient r;
     R := ring F;
     F := R^(rank F1);
     engine(ggev, f, F, r, EVector))
ERingMap EMatrix :=      (f,r) -> (
     F1 := targ r;
     R := ring f;
     F := R^(rank F1);
     engine(ggev, f, F, r, EMatrix))

-------------
-- EMatrix --
-------------
ematrix = (R,elems) -> (
    rank := #elems;
    elems = transpose elems;
    F := R^rank;
    vecs := apply(elems, v -> evector(F,v));
    scan(vecs, v -> sendgg ggPush v);
    sendgg(ggPush F, ggPush (#elems), ggPush 3, ggPush {}, ggmatrix);
    newEMatrix())

targ EMatrix := (m) -> (sendgg(ggPush m, gggetrows); newEFreeModule())
src EMatrix := (m) -> (sendgg(ggPush m, gggetcols); newEFreeModule())
matrixType EMatrix := (m) -> (sendgg(ggPush m, ggsetshift); eePopInt())
degree EMatrix := (m) -> (sendgg(ggPush m, gggetshift); eePopIntarray())
     
EMatrix _ ZZ := (m,c) -> (sendgg(ggPush m, ggPush c, ggelem); newEVector())
EMatrix _ Sequence := (m,x) -> (
    sendgg(ggPush m, ggPush x#0, ggPush x#1, ggelem);
    newERingElement())

EMatrix == EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggisequal);
    eePopBool())
EMatrix == ZZ := (m,n) -> (
    if n =!= 0 then error "cannot compare matrix to integer";
    sendgg(ggPush m, ggiszero);
    eePopBool())

EMatrix + EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggadd);
    newEMatrix())
- EMatrix := (m) -> (
    sendgg(ggPush m, ggnegate);
    newEMatrix())
EMatrix - EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggsubtract);
    newEMatrix())
ERingElement * EMatrix := (a,m) -> engine(ggmult,a,m,EMatrix)
EMatrix * ERingElement := (m,a) -> engine(ggmult,m,a,EMatrix)
     
EMatrix * EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggmult);
    newEMatrix())
ZZ * EMatrix := (a,m) -> (
    sendgg(ggPush a, ggPush m, ggmult);
    newEMatrix())
transpose EMatrix := (m) -> (
    sendgg(ggPush m, ggtranspose);
    newEMatrix())

ZZ _ EFreeModule := (i,F) -> (
    if i === 1 then (
	 sendgg(ggPush F, ggiden);
	 newEMatrix())
    else if i === 0 then (
	 sendgg(ggPush F, ggPush F, ggzeromat);
	 newEMatrix()))

EMatrix ** EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggtensor);
    newEMatrix())
EMatrix ++ EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggdirectsum);
    newEMatrix())

submatrix(EMatrix,List) := (m,a) -> (
    -- test: a is an array of integers, all within the range 0..c-1, where
    -- c is the number of columns of m.
    sendgg(ggPush m, ggPush a, ggsubmatrix);
    newEMatrix())
submatrix(EMatrix,List,List) := (m,rows,cols) -> (
    -- test: rows is an array of integers, all within the range 0..r-1, where
    -- r is the number of rows, and cols is an integer array  all within the
    -- rank 0..c-1, where c is the number of columns of m.
    sendgg(ggPush m, ggPush rows, ggPush cols, ggsubmatrix);
    newEMatrix())

isGraded EMatrix := (m) -> (
    sendgg(ggPush m, ggishomogeneous);
    eePopBool())
EMatrix * EVector := (m,v) -> (
    sendgg(ggPush m, ggPush v, ggmult);
    newEVector())
reshape(EMatrix,EFreeModule,EFreeModule) := (m,F,G) -> (
    sendgg(ggPush m, ggPush F, ggPush G, ggreshape);
    newEMatrix())
flip(EFreeModule,EFreeModule) := (F,G) -> (
    sendgg(ggPush F, ggPush G, ggflip);
    newEMatrix())
koszul(ZZ,EMatrix) := (p,m) -> (
    sendgg(ggPush m, ggPush p, ggkoszul);
    newEMatrix())
koszul(EMatrix,EMatrix) := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggkoszul);
    newEMatrix())
leadTerm(EMatrix) := (m) -> leadTerm(m,-1,true)
leadTerm(EMatrix,ZZ,Boolean) := (m,n,samecomp) -> (
    samecomp = if samecomp then 1 else 0;
    sendgg(ggPush m, ggPush n, ggPush samecomp, ggleadterm);
    newEMatrix())
random(ERing,ZZ,ZZ) := (R,r,c) -> (
    sendgg(ggPush R, ggPush r, ggPush c, ggrandom);
    newEMatrix())
modtensor(EMatrix,EMatrix) := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggmontensor);
    newEMatrix())
exteriorProduct(ZZ,ZZ,EFreeModule) := (p,q,F) -> (
    sendgg(ggPush p, ggPush q, ggPush F, ggexteriorproduct);
    newEMatrix())
EMatrix | EMatrix := (m,n) -> (
    sendgg(ggPush m, ggPush n, ggPush 2, ggconcat);
    newEMatrix())

diff (EMatrix,EMatrix) := (m,n) -> engine(ggdiff,m,n,EMatrix)
contract (EMatrix,EMatrix) := (m,n) -> engine(ggcontract,m,n,EMatrix)
sortcols = (m,degorder,monorder) -> engine(ggsortcolumns,m,degorder,monorder,Intarray)
coefficients(List, EMatrix) := (a,m) -> (
     sendgg(ggPush m, ggPush a, ggcoeffs);
     res1 := newEMatrix();
     res2 := newEMatrix();
     {res1, res2})

promote(EMatrix,EFreeModule) := (m,F) -> engine(ggpromote,F,m,EMatrix)
lift(EMatrix,EFreeModule) := (m,F) -> engine(gglift,F,m,EMatrix)
---------------------------
-- Useful little diddies --
---------------------------
makeRing = (mo) -> (
  K = ZmodP 101;
  M = emonoid(mo, toList(0..5), "a b c d e f");
  R = polyring(K, M, degreeRing 1, {1,1,1,1,1,1});
  a = R_(1_K,{0,1});
  b = R_(1_K,{1,1});
  c = R_(1_K,{2,1});
  d = R_(1_K,{3,1});
  e = R_(1_K,{4,1});
  f = R_(1_K,{5,1});
  R
  )

getentries = (m) -> (
     nrows := rank targ m;
     ncols := rank src m;
     apply(nrows, r -> apply(ncols, c -> m_(r,c))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
-- End:

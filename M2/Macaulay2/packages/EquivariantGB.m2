newPackage(
     "EquivariantGB",
     Version =>"0.2",
     Date => "2014",
     Headline => "Equivariant Groebner bases and related algorithms",
     HomePage => "",
     Authors => {
    	  {Name => "Chris Hillar", Email => "chillar@msri.org"},
	  {Name => "Robert Krone", Email => "krone@math.gatech.edu"},
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Keywords => {"Groebner Basis Algorithms"},
     PackageImports => {"FourTiTwo"},
     AuxiliaryFiles => true, -- set to true if package comes with auxiliary files
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     AuxiliaryFiles => true
     )

export {
     "egb",
     "egbToric",
     "buildERing",
     "buildEMonomialMap",
     "Symmetrize",
     "Completely",
     "Signature",
     "Buchberger",
     "Incremental",
     "reduce",
     "OutFile",
     "PrincipalSyzygies",
     "exponentMatrix",
     "incOrbit",
     --priority queue
     "PriorityQueue",
     "priorityQueue",
     "mergePQ",
     "deleteMin",
     "pop",
     "Shift",
     "shift"
     }

protect \ { symbols, varIndices, varTable, varPosTable, semigroup, indexBound, rings, 
    Seed, Extend, Sh, Roots, Min, Value, Children, pos, shM, polynomial, len, degreesList, 
    CompleteReduce, PrincipalSyzygies, Diagonal, basisList }

--ERing = new Type of PolynomialRing
Shift = new Type of BasicList
ShiftMonomial = new Type of HashTable
PriorityQueue = new Type of MutableHashTable
Node = new Type of HashTable
MPair = new Type of HashTable

-- In:
-- Out: 
egb = method(Options=>{Symmetrize=>false, OutFile=>null, Algorithm=>Buchberger, PrincipalSyzygies=>false})
egb (List) := o -> F -> (
     if o.Algorithm == Incremental then return egbIncremental(F,Symmetrize=>o.Symmetrize,OutFile=>o.OutFile);
     if o.Algorithm == Signature   then return egbSignature(F,OutFile=>o.OutFile,PrincipalSyzygies=>o.PrincipalSyzygies);
     g := o.OutFile;
     n := width ring first F;
     k := 0;
     while k < n do (
	  if k == 0 then (
	       if o.Symmetrize then (
		    g << "--   symmetrize " << flush;
		    F = printT(timing interreduce'symmetrize F, g);
		    );
	       g << (sort F) << endl;
	       g << (toExternalString sort F) << endl;
	       g << "-- gens: " << #F
	       << "; indices: " <<  width F
	       << "; max deg: " << max((F / degree) / first)
	       << "." << endl;
	       );
	  g << "-- " << k << " gap Spairs  " << flush;
	  newF := printT(timing processSpairs(F,k, Symmetrize => o.Symmetrize), g);
	  g << "--   " << (#newF - #F) << " new" << endl;
	  g << "--   interreduce ";
	  newF = printT(timing interreduce newF, g);
	  g << "--   reduce tails";
	  newF = printT(timing reduceTails newF, g);
          newF = pruneERing(newF,o.Symmetrize);
	  isNew := gensEqual(F, newF);
	  F = newF;
	  if isNew then k = 0
	  else k = k+1;
	  n = width ring first F;
	  );
     F
     )


--version of reduce2 to export
reduce = method(Options=>{Completely=>false})
reduce (RingElement,List) := o -> (f,F) -> (
     (r,R,g,G) := reduce2(f,F);
     r
     )

--In: X, a list of symbols to name each block of variables
--    s, a list of sequences of integers describing the action on each block of variables
--    K, a coefficient field
--    n, an integer
--Out: a polynomial ring over K with variable indices determined by s.
--     All "infinite" indices are included up to n-1. 
buildERing = method(Options=>{MonomialOrder=>Lex,Degrees=>{}})
buildERing (List,List,Ring,ZZ) := o -> (X,s,K,n) -> buildERing((X,s,K),ZZ,n, MonomialOrder=>o.MonomialOrder, Degrees=>o.Degrees)
buildERing (Ring,ZZ) := o -> (R,n) -> (
    if (R.rings)#?n then (R.rings)#n
    else buildERing((R.symbols,R.semigroup,coefficientRing R),R,n, MonomialOrder=>R.MonomialOrder, Degrees=>R.degreesList)
    )
buildERing (Sequence,Ring,ZZ) := o -> (seq,oldR,n) -> (
     (X,s,K) := seq;
     variableIndices := s / (b->(toList ((b:0)..(b:n-1))));
     mo := o.MonomialOrder;
     degreesList := if #o.Degrees == 0 then toList (#s:1) else o.Degrees;
     if mo == Diagonal then (
	  variableIndices = apply(variableIndices, l->(
		    dPartition := partition(i->any(i, j->(#select(i, k->(k==j)) > 1)), l);
		    (dPartition#false)|(dPartition#true)));
	  mo = Lex;
	  );
     degreesLongList := flatten apply(#s, b->toList (#(variableIndices#b):degreesList#b));
     variableIndices = flatten apply(#s, b->(reverse apply(variableIndices#b, i->((1:b)|i))));
     moList := apply(s, b->(mo=>(n^b)));
     R := K[apply(variableIndices, i->(
		    if #i == 1 then X#(i#0)            --if block has only one variable, use no index
		    else if #i == 2 then X#(i#0)_(i#1) --if block has only one index, index by integers
		    else (X#(i#0))_(take(i,1-#i))      --if block has several indices, index by sequences
		    )),
	    MonomialOrder => moList,
	    Degrees => degreesLongList
	    ];
     R.symbols = X;
     R.varIndices = variableIndices;
     R.varTable = new HashTable from apply(#(R.varIndices), n->(R.varIndices#n => (gens R)#n));
     R.varPosTable = new HashTable from apply(#(R.varIndices), n->(R.varIndices#n => n));
     R.semigroup = s;
     R.indexBound = n;
     R.MonomialOrder = o.MonomialOrder;
     R.degreesList = degreesList;
     if oldR === ZZ then R.rings = new MutableHashTable
     else R.rings = oldR.rings;
     (R.rings)#n = R;
     R
     )
 
exponentMatrix = method()
exponentMatrix RingElement := m -> (
     R := ring m;
     assert(all(R.semigroup, i->i==1));
     L := first first listForm leadMonomial m;
     Lpos := R.varIndices;
     M := mutableMatrix(ZZ,#(R.semigroup),R.indexBound);
     for i from 0 to #L-1 do M_(Lpos#i) = L#i;
     matrix M
     )

gensEqual = (F,G) -> (
     R := ring first F; 
     S := ring first G;
     numgens R != numgens S or sort (F / ringMap(S,R)) != sort G
     )
     
spoly = (f,g) -> (
     l := lcm(leadMonomial f,leadMonomial g);
     return (l//(leadTerm f))*f - (l//(leadTerm g))*g;
     )
 
jpoly = (f,g) -> (
    l := lcm(leadMonomial f,leadMonomial g);
    l//(leadMonomial f)
    )

-- In: v, a polynomial
--     w, a polynomial 
-- Out: (B, M)
--     B, a boolean, whether there is a shift M s.t. M*in(v) divides in(w)
divWitness = method(Options=>{Seed=>null})
divWitness (RingElement,RingElement) := o -> (v,w) ->
    divWitness(shiftMonomial(v,shift{}), shiftMonomial(w,shift{}), Seed=>o.Seed)
divWitness (Shift,Shift) := o -> (I,J) -> (
    d := listDivWitness({{}},{{}},gaps I,gaps J, Seed=>o.Seed);
    (d#0, shift d#1)
    )

divWitness (ShiftMonomial,ShiftMonomial) := o -> (s,t) -> (
    (v,I,w,J) := (s.Monomial,s.Sh,t.Monomial,t.Sh);
    vl := (listForm leadTerm v)#0#0;
    wl := (listForm leadTerm w)#0#0;
    (vhints,whints) := (v,w)/hints;
    sigma := o.Seed;
    local isDiv;
    while true do (
	(isDiv, sigma) = listDivWitness(vhints,whints,gaps I,gaps J, Seed=>sigma);
	if not isDiv then break;
	if checkDiv(vl,wl,sigma,ring v,ring w) then return (true, shift sigma);
	);
    (false, shift{})
    )

divQuotient = method(Options=>{Seed=>null})
divQuotient (RingElement,RingElement) := o -> (v,w) ->
    divQuotient(shiftMonomial(v,shift{}), shiftMonomial(w,shift{}), Seed=>o.Seed)
divQuotient (Shift,Shift) := o -> (I,J) -> (
    d := listDivWitness({{}},{{}},gaps I,gaps J, Seed=>o.Seed);
    (d#0, shift d#1)
    )
divQuotient (ShiftMonomial,ShiftMonomial) := o -> (s,t) -> (
    (isDiv, I) := divWitness(s,t,Seed=>o.Seed);
    if not isDiv then return (false, 0);
    Is := I*s.Monomial;
    Is = (ringMap(ring t,ring Is))Is;
    (true, shiftMonomial(t.Monomial//Is,I))
    )
    

listDivWitness = method(Options=>{Seed=>null})
listDivWitness (List,List,List,List) := o -> (v,w,vgaps,wgaps) -> (
    (vgdeg,wgdeg) := (vgaps,wgaps)/sum;
    if vgdeg > wgdeg or any(#v, i->(sum v#i > sum w#i)) then return (false, new MutableList);
    (vmax,wmax) := (v,w)/maxEntry;
    if vgdeg == 0 and wgdeg == 0 then (
    	if vmax == -1 then return (true, new MutableList);
	) else (
	wmax = max{wmax, maxEntry {wgaps} + 1, 0};
	vmax = max{vmax, wmax + vgdeg - wgdeg, 0};
	);
    local sigma; local k;
    if o.Seed === null then (
	sigma = new MutableList from {-1};
	k = 0;
	) else (
	sigma = take(new MutableList from o.Seed,vmax+1);
    	for l from #sigma to vmax do sigma#l = if #sigma == 0 then 0 else 1 + last sigma;
    	k = vmax;
	);
    while true do (
	i := k;
	for j from (sigma#k)+1 to wmax do (
	    if listEl(vgaps,i) > 0 and listEl(wgaps,j) == 0 then break;
	    if all(#v, s->(listEl(v#s,i) <= listEl(w#s,j)))
	    and listEl(vgaps,i) == listEl(wgaps,j) then (
		sigma#i = j;
		i = i+1;
		);
	    );
	if i <= vmax then k = k-1 else break;
	if k < 0 then return (false, new MutableList);
	);
    return (true, sigma);
    )

listEl = (L,n) -> if L#?n then L#n else 0

hints = v -> (
    R := ring v;
    n := width R;
    vl := (listForm leadTerm v)#0#0;
    h := toList apply(#R.semigroup, b->toList (R.semigroup#b: new MutableList from (n:0)));
    for ind in R.varIndices do
	for j from 0 to R.semigroup#(ind#0)-1 do
	    h#(ind#0)#j#(ind#(j+1)) = h#(ind#0)#j#(ind#(j+1)) + vl#(R.varPosTable#ind);
    (flatten flatten h) / toList
    )

checkDiv = (vl,wl,sigma,R,S) -> (
    all(R.varIndices, ind->(
	    sind := (1:ind#0)|apply(1..#ind-1, j->sigmaEntry(sigma,ind#j));
	    (not S.varPosTable#? sind and vl#(R.varPosTable#ind) == 0)
	    or vl#(R.varPosTable#ind) <= wl#(S.varPosTable#sind)))
    )
sigmaEntry = (sigma,k) -> if sigma#?k then sigma#k else if #sigma == 0 then k else last sigma + k - #sigma + 1

basicReduce = method(Options=>{Completely=>false})
basicReduce (RingElement, BasicList) := o -> (f,B) -> (
     B = select(toList B,b->b!=0);
     R := ring f;
     oldf := f;
     n := width R;
     divisible := true;
     while divisible and f != 0 do (
	  divisible = false;
	  local sigma; local divisor;
	  for b in B do (
	       (divisible, sigma) = divWitness(b,f);
	       if divisible then (
		    divisor = b;
		    break;
	       );
	  );
	  if divisible then (
	       iS := indexSupport({divisor});
	       maxi := position(iS, i->(i != 0), Reverse=>true);
	       if sigma#maxi >= n then return null;
	       sd := (shiftMap(R,sigma)) sub(divisor,R);
	       f = f - (leadTerm f//leadTerm sd)*sd;
	       --print("reduce:",g,f,divisor,sd);
	       );
	  );
     if not o.Completely or f == 0 then f
     else leadTerm f + basicReduce(f - leadTerm f,B,Completely=>true) 
     )

--runs basicReduce, but possibly expands the ring in the process
reduce2 = method(Options=>{Completely=>false})
reduce2 (RingElement,List) := o -> (f,F) -> (
     R := ring f;
     r := basicReduce(f,F,Completely=>o.Completely);
     while r === null do (
     	  Rnew := buildERing(R,(width R)+1);
     	  RtoRnew := ringMap(Rnew,R);
     	  F = RtoRnew\F;
     	  f = RtoRnew f;
     	  r = basicReduce(f,F,Completely=>o.Completely);
	  R = Rnew
	  );
     (r,R,f,F)
     )


-- In: F, current generators list
--     k, ZZ (the number of "gaps")  
-- Out: the updated list of generators
-- Description: takes all s-pairs using k "gaps" when "interlacing", reduces, and interreduces. 
processSpairs = method(Options=>{Symmetrize=>false})
processSpairs (List,ZZ) := o -> (F,k) -> (
     --if o.Symmetrize then F = interreduce'symmetrize F; 
     R := ring F#0;
     n := width R;
     S := buildERing(R,n+k);
     F = F / ringMap(S,R);
     maxIndices := apply(F, f->(width f));
     --sp := shiftPairs(n,n,k);
     --print apply(sp,t->matrix first t||matrix last t);
     Fnew := {};
     for i from 0 to #F-1 do (
	  for j from 0 to i do (
	       sp := shiftPairs(maxIndices#i, maxIndices#j, k);
	       for st in sp do (
		    (s,t) := st;
		    f := spoly((shiftMap(S,s)) F#i, (shiftMap(S,t)) F#j);
		    local r;
		    (r,S,f,F) = reduce2(f,F);
		    --print (f,i,j,s,t,(shiftMap(S,s)) F#i, (shiftMap(S,t)) F#j);
		    if r != 0 then (
			 --<< "(n)";
			 --print(F#i,F#j,r); 
			 Fnew = append(Fnew,r)
			 );
		    );
	       );
	  );
     Fnew = apply(Fnew, f->((ringMap(S,ring f)) f));
     F|Fnew
     )

-- In: n0, ZZ
--     n1, ZZ
--     k,  ZZ
-- Out: a List of all interlacing pairs of subsets of [max{n0,n1}+k] of size n0 and n1 (with k gaps)
shiftPairs = (n0,n1,k) -> (
     --assert(k==1); -- assume k=1
     (small,big) := toSequence sort{n0,n1};
     --if k >= small then return {};
     flatten apply(subsets(big + k, n0), sImage->(
	       apply(subsets(sImage, small - k), stImage->(
			 sPos := 0;
			 stPos := 0;
			 tImage := select(big + k, i->(
				   sPos >= #sImage or i != sImage#sPos or (
					sPos = sPos+1;
					b := stPos < #stImage and i == stImage#stPos;
					if b then stPos = stPos+1;
					b
					)
				   ));
			 (shift sImage, shift tImage)
			 ))
	       ))			 
     )

--In: R, a Ring
--    s, a Shift
--Out: a map from R to S where index i is mapped to s#i.
--     If Extend => false then S = R, otherwise S has index bound equal to the max entry of s -1.
--     If s#i == -1 then all variables with index i go to 0_S.
shiftMap = method(Options=>{Extend=>false})
shiftMap(Ring,List) := o -> (R,L) -> shiftMap(R,shift L,Extend=>o.Extend)
shiftMap(Ring,Shift) := o -> (R,I) -> (
    I = crop(I,width R);
    S := R;
    if o.Extend then S = buildERing(R, (max apply(width R,i->I#i))+1);
    mapList := apply(R.varIndices, ind->(
	    indnew := new MutableList from ind;
	    for j from 1 to #ind-1 do (
	       	if ind#j >= #I or I#(ind#j) < 0 or I#(ind#j) >= width S then return 0_S
	       	else indnew#j = I#(ind#j);
	       	);
	    S.varTable#(toSequence indnew)
	    ));
    --print(s,R.varIndices,mapList);
    map(S,R,mapList)
    )

ringMap = (S,R) -> map(S,R, apply(R.varIndices, i->(if S.varTable#? i then S.varTable#i else 0_S)))

-- In: F, a list of generators
-- Out: the symmetrization of F 
symmetrize = method()
symmetrize List := F -> flatten (F/symmetrize)
symmetrize RingElement := f -> (
     R := ring f;
     is := indexSupport {f};
     IS := select(#is, j->(is#j > 0)); -- list of indices present in f
     apply(permutations IS, p->((shiftMap(R,shift p)) f))
     )


-- In: F, a list of generators
-- Out: F, a list of generators reduced with respect to itself
interreduce = method()
interreduce (List) := F -> (
     --Reduce elements of F with respect to each other.
     --print "-- starting \"slow\" interreduction";
     R := ring first F;
     n := width R;
     i := 0;
     while i < #F do (
	  if F#i == 0 then (i = i+1; continue);
	  j := 0;
	  while j < i do (
	       --print (i,j);
	       if F#j == 0 then (j = j+1; continue);
	       k := -1;
	       if first divWitness(F#j,F#i) then k = i
	       else if first divWitness(F#i,F#j) then k = j;
	       if k != -1 then (
		    Fout := drop(F,{k,k});
	       	    local r; local f;
	       	    (r,R,f,Fout) = reduce2(F#k,Fout);
	       	    F = append(Fout, makeMonic r);
		    i = i-1;
		    );
	       if k == i+1 then break;
	       if k == -1 then j = j+1;
	       );
	  i = i+1;
	  );
     F = select(F, f->f!=0);
     F
     )

reduceTails = F -> (
     R := ring first F;
     apply(F, f->(
	       g := f - leadTerm(f);
	       local r;
	       (r,R,g,F) = reduce2(g,F,Completely=>true);
	       makeMonic(leadTerm(f) + r)
	       ))
     )
    

--Prune unused variables from R.
--In: F, a list of polynomials
--    sym, a boolean whether to symmetrize.
--Out: F, the same polynomials mapped to a ring with minimal indexBound.
pruneERing = (F,sym) -> (
     R := ring first F;
     newn := 0;
     if sym then (
	  iS := indexSupport F;
	  s := shift apply(iS, j->(if j > 0 then (newn = newn+1; newn-1) else -1));
	  F = F / shiftMap(R,s);
	  )
     else newn = width F;
     S := buildERing(R,newn);
     F / ringMap(S,R)
     )

--In: F, a list of polynomials
--Out: a list, the number of variables represented in F which have an index equal to i for each i from 0 to n-1
indexSupport = F -> (
    R := ring first F;
    n := width R;
    F = select(F, f-> f != 0);
    if #F == 0 then return toList (n:0);
    vSupport := sum(apply(F, f->sum(listForm(f)/first))); --list of # occurrances of each variable in newF
    nSupport := new MutableList from (n:0); --list of # occurances of each index value in variable support
    for i from 0 to #(R.varIndices)-1 do (
	if vSupport#i > 0 then (
	    ind := R.varIndices#i;
	    for j from 1 to #ind-1 do nSupport#(ind#j) = nSupport#(ind#j)+1;
	    );
	);
    toList nSupport
    )
 
maxEntry = V -> (
    W := apply(V, v->position(v, i->(i > 0), Reverse=>true));
    max (apply(W, i-> if i === null then 0 else i)|{0})
    )

-- should run faster if the reduction is done with the fast internal gb routine
-- ??? is there a function that just interreduces ???
interreduce'symmetrize = F -> ( 
     F' := symmetrize F;
     --F' = flatten entries gens gb ideal F';
     --print F';
     interreduce F'
     ) 

makeMonic = f -> if f== 0 then 0 else f/leadCoefficient f 

printT = (T,f) -> (
     t := T#0;
     s := " seconds";
     if t > 120 then (
	  t = t/60;
	  s = " minutes";
	  if t > 120 then (
	       t = t/60;
	       s = " hours";
	       );
	  );
     f << "     -- used " << t << s << endl;
     T#1
     )



--builds an equivariant monomial map from ERing R to S.
--F is a list storing the image of y_(0,1,...,k-1) for each block of variables in R.
buildEMonomialMap = method()
buildEMonomialMap(Ring,Ring,List) := (S,R,F) -> (
     if width S != width R then (
	  S' := buildERing(S,width R);
          F = F / ringMap(S',S);
	  S = S';
	  );
     Fsupp := F / support;
     mapList := apply(R.varIndices, I -> (
	       subs := apply(Fsupp#(I#0), v->(
			 J := new MutableList from S.varIndices#(index v);
			 for k from 1 to #J-1 do J#k = I#(J#k + 1);
			 v => S.varTable#(toSequence J)
			 ));
	       sub(F#(I#0), subs)
	       ));
     map(S,R,mapList)
     )

--returns the exponent matrix associated to a monomial map
matrixFromMap = m -> (
     A := flatten entries m(vars source m);
     A = A / exponents / flatten;
     transpose matrix A
     )

egbToric = method(Options=>{OutFile=>null})
egbToric(RingMap) := o -> M -> (
     R := source M;
     S := target M;
     out := o.OutFile;
     r := R.semigroup;
     F := apply(#r, p -> M(R.varTable#((1:p)|0..(r#p-1))));
     k := width R;
     lastNewk := k;
     T := buildEMonomialMap(S,R,F);
     G := transpose toricGroebner matrixFromMap T;
     lastNewG := G;
     while 2*lastNewk > k+1 do (
	  collectGarbage();
	  k = k+1;
	  out << k << endl;
	  Rnew := buildERing(R,k);
	  Tnew := buildEMonomialMap(S,Rnew,F);
	  Gnew := printT(timing transpose toricGroebner matrixFromMap Tnew, out);
	  shifts := subsets(k,k-1);
	  sList := apply(shifts, s -> (
		    m := shiftMap(Rnew,s)*ringMap(Rnew,R);
		    matrixFromMap m
		    ));
	  L1 := matrix{apply(sList, s->s*G)};
	  L1 = sort L1;
	  uniqueCols := printT(timing select(numgens source L1, i->(i == 0 or L1_{i} != L1_{i-1})), out);
	  L1 = L1_uniqueCols;
	  out << (numgens target Gnew,numgens source Gnew) << endl;
	  L2 := sort Gnew;
	  if L1 != L2 then (out << "new stuff found" << endl; lastNewk = k; lastNewG = Gnew);
	  R = Rnew;
	  T = Tnew;
	  G = Gnew;
	  );
     k = lastNewk;
     R = buildERing(R,lastNewk);
     GBtrad := flatten entries sort gens toBinomial(transpose lastNewG, R);
     minIncGens GBtrad
     )

minIncGens = F -> (
    n := width F;
    seen := new MutableHashTable;
    select(F, g -> (
	    if not seen#?g then (
	        for f in incOrbit(g,n,Symmetrize=>false) do (seen#f = true; seen#(-1*f) = true);
		true
		)
	    else false
	    ))
    )

incOrbit = method(Options=>{Symmetrize => false})
incOrbit(RingElement,ZZ) := o -> (f,n) -> incOrbit({f},n,Symmetrize=>o.Symmetrize)
incOrbit(List,ZZ) := o -> (F,n) -> (
    F = apply(F, f->matchRing(f,n));
    R := ring first F;
    flatten for f in F list (
	k := width f;
	shifts := subsets(n,k);
	if o.Symmetrize then shifts = flatten apply(shifts, permutations);
	shifts = apply(shifts, shift);
	for s in shifts list (shiftMap(R,s))f
	)
    )

egbIncremental = method(Options=>{Symmetrize=>false, OutFile=>null})
egbIncremental(List) := o -> F -> (
    if #F == 0 then return F;
    k := width F;
    lastNewk := k;
    while 2*lastNewk > k do (
	F = sort apply(F, f->matchRing(f,k));
	incF := incOrbit(F,k,Symmetrize=>o.Symmetrize);
	incG := flatten entries gens gb ideal incF;
	G := sort minIncGens incG;
	if G != F then (
	    F = G;
	    lastNewk = k;
	    );
	k = k+1;
	);
    n := width G;
    apply(G, g->matchRing(g,n))
    )

 
-----------------------------------------------------
--Shift and ShiftMonomial for egbSignature
-----------------------------------------------------
net Shift := S -> net toList S 
shift = method()
shift List := L -> new Shift from L
shift MutableList := L -> new Shift from L
shift Shift := S -> S
Shift*Shift := (I,J) -> (
    if #J < #I - degree J then J = crop(J,#I - degree J);
    apply(J, j->(if I#?j then I#j else j + degree I))
    )
gaps = I -> (
    k := #I + degree I;
    g := new MutableList from (k:1);
    for i in I do g#i = 0;
    toList g
    )
crop = method()
crop (Shift,ZZ) := (I,n) -> (
    k := #I;
    if k < n then return shift((toList I) | (toList ((k + degree I)..(n-1 + degree I))));
    if k > n then return shift take(I,n);
    I
    )
degree (Shift) := I -> if #I == 0 then 0 else max(I) + 1 - #I
Shift ? Shift := (I,J) -> (
    if degree I == degree J then (
	(gapsI,gapsJ) := (I,J)/gaps;
	k := max{#gapsI,#gapsJ};
	(toSequence gaps I)|((k-#gapsI):0) ? (toSequence gaps J)|((k-#gapsJ):0)
	)
    else degree I ? degree J
    )
Shift == Shift := (I,J) -> (I ? J) === symbol ==
Shift*RingElement := (I,f) -> (shiftMap(ring f, I, Extend=>true))f
Shift*Number := (I,n) -> if I#?n then I#n else if n >= 0 then n + degree I else n
max Shift := I -> max ((toList I)|{0})

net ShiftMonomial := S -> net toString S.Monomial | "*" | net S.Sh
shiftMonomial = method()
shiftMonomial (RingElement,Shift) := (p,I) -> (
    I = crop(I,width I);
    new ShiftMonomial from hashTable {Monomial=>(leadTerm p), Sh=>I}
    )
ShiftMonomial * ShiftMonomial := (S,T) -> (
    (p,I) := (S.Monomial, S.Sh);
    (q,J) := (T.Monomial, T.Sh);
    R := ring q;
    Iq := (shiftMap(R,crop(I,width R),Extend=>true))q;
    (p,Iq) = matchRing(p,Iq);
    shiftMonomial(p*Iq, I*J)
    )
ShiftMonomial * RingElement := (S,p) -> (
    p = S.Sh*p;
    (a,b) := matchRing(S.Monomial,p);
    a*b
    )
Shift * ShiftMonomial := (I,S) -> shiftMonomial(I*S.Monomial,I*S.Sh)
width (ShiftMonomial) := S -> max {width S.Monomial, width S.Sh}
ring (ShiftMonomial) := S -> ring S.Monomial
ShiftMonomial ? ShiftMonomial := (S,T) -> (
    if width S == width T then (
	if S.Sh == T.Sh then (
	    compare(S.Monomial,T.Monomial)
	    ) else S.Sh ? T.Sh
	) else width S ? width T
    )
ShiftMonomial == ShiftMonomial := (S,T) -> (S ? T) === symbol ==

net MPair := M -> net "(" | net toString M.polynomial | ", " | net M.shM | "*[" | net M.pos | "])"   
mPair = method()
mPair (ShiftMonomial,ZZ,MutableList,RingElement) := (S,i,F,v) -> new MPair from hashTable{shM=>S,pos=>i,basisList=>F,polynomial=>v}
--MPair ? MPair := (m,n) -> (
--    memon := leadMonomial((m.basisList)#(m.pos));
--    nemon := leadMonomial((m.basisList)#(m.pos));
--    L := matchRing {memon,m.polynomial,nemon,n.polynomial};
--    (ms,ns) := (L#0*L#1, L#2*L#3);
--    if ms == ns then m.pos ? n.pos else ms ? ns
--    )
MPair ? MPair := (m,n) -> if width m == width n then (
    if degree m.polynomial == degree n.polynomial then (
    	-- if m.pos == n.pos then m.shM ? n.shM else m.pos ? n.pos
    	if m.shM == n.shM then m.pos ? n.pos else m.shM ? n.shM
    	) else degree m.polynomial ? degree n.polynomial
    ) else width m ? width n
MPair == MPair := (M,N) -> (M ? N) === symbol ==
ShiftMonomial * MPair := (S,M) -> mPair(S*M.shM,M.pos,M.basisList,S*M.polynomial)
Shift * MPair := (I,M) -> mPair(I*M.shM,M.pos,M.basisList,I*M.polynomial)
width MPair := M -> max {width M.shM, width M.polynomial}

matchRing = method()
matchRing(RingElement,RingElement) := (p,q) -> (
    (R,S) := (p,q)/ring;
    if width R < width S then p = (ringMap(S,R))p;
    if width R > width S then q = (ringMap(R,S))q;
    (p,q)
    )
matchRing(RingElement,ZZ) := (p,n) -> (
    S := buildERing(ring p, n);
    (ringMap(S,ring p))p
    )
matchRing(RingElement,Ring) := (p,S) -> (ringMap(S,ring p))p
matchRing List := L -> (
    n := max (width@@ring\L);
    apply(L, p->(matchRing(p,n)))
    )
compare = method()
compare(RingElement,RingElement) := (p,q) -> (
    (p,q) = matchRing(p,q);
    p ? q
    )
width Ring := R -> R.indexBound
width RingElement := p -> (
    n := position(indexSupport{p}, i->(i > 0), Reverse=>true);
    if n =!= null then n+1 else 0
    )
width Shift := I -> (
    n := position(gaps I, i->i==1, Reverse=>true);
    if n === null then 0 else n+1
    )
width List := L -> max(L / width)


egbSignature = method(Options=>{PrincipalSyzygies=>false, CompleteReduce=>true, OutFile=>null})
egbSignature (List) := o -> F -> (
    F = new MutableList from F;
    R := ring first F;
    Fwidths := apply(F,width@@leadMonomial);
    JP := priorityQueue toList apply(#F, i -> mPair(shiftMonomial(1_R,shift{}),i,F,F#i));
    out := o.OutFile;
    H := {};
    G := {};
    coveredCount := 0;
    while min JP =!= null do (
	j := min JP;
	deleteMin JP;
	if width j > 7 then error "breakpoint!!!"; 
	if isCovered(j,G) or 
	   isCovered(j,H) -* perhaps this is not needed... 
	                  but there are duplicates in JP at the moment.
			  Another question: why do we check if a j-pair is covered by syzygies only when we insert it in JP? 
	                  *- 
	   then (
	    out << "  covered pair in JP: " << j << endl;
	    coveredCount = coveredCount + 1;
	    continue
	    );
	out << "  processing pair: " << j << endl;
	j = regularTopReduce(j,G);
	p := j.polynomial;
	if p == 0 then ( 
	    H = append(H,j); 
	    out << "-- " << #H << "th syzygy is: " << j << endl;
	    )
	else (
	    if o.CompleteReduce then p = completeReduce(p,apply(G,g->g.polynomial));
	    if p == 0 then continue; -- p==0 is not enough to record a syzygy
	    if p =!= j.polynomial then (
	      F#(#F) = p;
	      Fwidths#(#Fwidths) = width leadMonomial p;
	      j = mPair(shiftMonomial(1_R,shift{}),#F,F,p);
	      << "-- F contains " << #F << " elements now" << endl;
	      );
	    G = append(G,j);
	    out << "-- " << #G << "th basis element is: " << j << endl;
	    for g in G do (
		(newJP,newPS) := jPairs(j,g);
		out << "   new J-pairs: " << #newJP << endl;
		newJP = select(newJP, j->not isCoveredByTrivSyg(j,Fwidths#(j.pos)) and not isCovered(j,H));
		scan(newJP, j->insert(JP,j));
		out << "   new NOT covered J-pairs: " << #newJP << endl;
		coveredCount = coveredCount - #newJP;
		if o.PrincipalSyzygies then (
		  newPS = select(newPS, s->not isCoveredByTrivSyg(s,Fwidths#(s.pos)) and not isCovered(s,H));
		  H = H | newPS;
		  )
		);
	    out << "  JP queue length: " << length JP << endl;
	    out << "  syzygies in H: " << #H << endl;
	    );
	);
    << "-- TOTAL covered pairs = " << coveredCount << endl;
    apply(G, g->g.polynomial)
    )

jPairs = (j,g) -> (
    newJP := new MutableList;
    newPS := new MutableList; -- principal syzygies
    (jw,gw) := (j,g)/width;
    for k from 0 to max(min(jw,gw)-1,0) do (
	for p in shiftPairs(jw,gw,k) do (
	    (jI,gI) := p;
	    jshift := jI*(j.polynomial);
	    gshift := gI*(g.polynomial);
	    (jshift,gshift) = matchRing(jshift,gshift);
	    jSP := shiftMonomial(jpoly(jshift,gshift),jI)*j;
	    gSP := shiftMonomial(jpoly(gshift,jshift),gI)*g;
	    if jSP == gSP then continue;
	    JP := if jSP > gSP then jSP else gSP;
	    assert((JP.shM).Monomial != 0);
	    newJP#(#newJP) = JP;
	    jSP = shiftMonomial(leadMonomial gshift,jI)*j;
	    gSP = shiftMonomial(leadMonomial jshift,gI)*g;
	    if jSP == gSP then continue;
	    PS := if jSP > gSP then jSP else gSP;
	    assert((PS.shM).Monomial != 0);
	    newPS#(#newPS) = PS;
	    );
	);
    (toList newJP, toList newPS)
    )

regularTopReduce = (j,G) -> (
    if j.polynomial == 0 then return j;
    for g in G do (
	isDiv := true; local Q; seed := null;
	while isDiv do (
	    (isDiv,Q) = divQuotient(g.polynomial,j.polynomial,Seed=>seed);
	    if isDiv and Q*g < j then (
		-- << "  reducing j = " << j << endl << "  by Q*g = " << Q << "*" << g << endl;
		v := reduction(j.polynomial,Q*g.polynomial);
		return regularTopReduce(mPair(j.shM,j.pos,j.basisList,v),G);
		);
	    if isDiv then seed = Q.Sh;
	    );
	);
    return j;
    )

--copied from regularTopReduce and simplified
completeReduce = (p,G) -> (
    if p == 0 then return p;
    for g in G do (
	(isDiv,Q) := divQuotient(g,p); 
	if isDiv then return completeReduce(reduction(p,Q*g),G);
	);
    p
    )

reduction = (v,w) -> (
    (v,w) = matchRing(v,w);
    v - (leadTerm v//leadTerm w)*w
    )

isCoveredByTrivSyg = (j,k) -> (
    J := j.shM.Sh;
    Jwidth := position(0..<#J, i->(J#i > if i == 0 then 0 else J#(i-1)+1), Reverse=>true);
    Jwidth = if Jwidth === null then 0 else Jwidth+1;
    Jwidth > k
    )

isCovered = (j,G) -> (
    for g in G do (
	if j.pos != g.pos then continue;
	isDiv := true; local Q; seed := null;
	while isDiv do (
	    (isDiv,Q) = divQuotient(g.shM,j.shM,Seed=>seed);
	    if isDiv and (g.polynomial == 0 or compare(Q*(g.polynomial),j.polynomial) === symbol <) then return true;
	    if isDiv then seed = Q.Sh;
	    );
	);
    false
    )

------------------------------
-- PriorityQueue
------------------------------
-- PriorityQueue is a mutable priority queue implemented as a binomial heap, prioritizing the minimum.

priorityQueue = method()
--Instantiates an empty priority queue.
installMethod(priorityQueue, () -> new PriorityQueue from new MutableHashTable from {
	Min=>null,
	Roots=>new MutableList,
	Degree=>0,
	len=>0
	})
--Instantiates a priority queue with elements from a list L.
priorityQueue (List) := L -> (
    Q := priorityQueue();
    for l in L do (
	n := node l;
	R := new PriorityQueue from new MutableHashTable from {
	    Min=>n,
	    Roots=>new MutableList from {n},
	    Degree=>1,
	    len=>1
	    };
	Q = mergePQ(Q,R);
	);
    Q
    )

--Inserts x into the priority queue Q.  Returns Q.
insert (PriorityQueue,Thing) := (Q,x) -> mergePQ(Q,priorityQueue({x}))

--Merges priority queue R into priority queue Q.  Returns Q.
mergePQ = method()	
mergePQ (PriorityQueue,PriorityQueue) := (Q,R) -> (
    Q.len = Q.len + R.len;
    if R.Min === null then return Q;
    if Q.Min === null or min R < min Q then Q.Min = R.Min;
    cr := null;
    k := 0;
    while k < R.Degree or cr =!= null do (
	rr := if R.Roots#?k then R.Roots#k else null;
	qr := if Q.Roots#?k then Q.Roots#k else null;
	roots := new MutableList from select({qr,rr,cr}, r->(r =!= null));
	if #roots == 1 then Q.Roots#k = roots#0;
	if #roots < 2 then cr = null else (
	    if roots#1 < roots#0 or roots#1 === Q.Min then (
		(a,b) := (roots#0,roots#1);
		roots#0 = b;
		roots#1 = a;
		);
	    (roots#0).Children#(#(roots#0).Children) = roots#1;
	    cr = roots#0;
	    Q.Roots#k = if #roots == 3 then roots#2 else null;
	    );
	k = k+1;
	);
    deg := position(toList Q.Roots, r->(r =!= null), Reverse=>true);
    Q.Degree = if deg === null then 0 else deg + 1;
    --print apply(Q.Degree, i->(if Q.Roots#i === null then null else (Q.Roots#i).Value));
    Q	
    )

--Returns the minimum element in Q.
min PriorityQueue := Q -> if Q.Min === null then null else nValue Q.Min

--Deletes the minimum element from priority queue Q.  Returns Q.
deleteMin = method()
deleteMin (PriorityQueue) := Q -> (
    if Q.Min === null then return null;
    l := Q.len;
    m := min Q;
    k := position(toList Q.Roots, r->(r === Q.Min));
    Rroots := (Q.Roots#k).Children;
    Rmin := if #Rroots == 0 then null else min toList Rroots;
    R := new PriorityQueue from new MutableHashTable from {Min=>Rmin,Roots=>Rroots,Degree=>#Rroots,len=>2^k-1};
    Q.Roots#k = null;
    Qroots := delete(null, toList Q.Roots);
    Q.Min = if #Qroots == 0 then null else min toList Qroots;
    deg := position(toList Q.Roots, r->(r =!= null), Reverse=>true);
    Q.Degree = if deg === null then 0 else deg + 1;
    mergePQ(Q,R);
    Q.len = l-1;
    Q
    )

pop = method()
pop PriorityQueue := Q -> (
    m := min Q;
    deleteMin Q;
    m
    )

length PriorityQueue := Q -> Q.len
node = f -> new Node from hashTable {Value=>f,Children=>new MutableList}
nValue = N -> N.Value
Node ? Node := (n,m) -> nValue n ? nValue m
    

beginDocumentation()

doc ///
     Key
          EquivariantGB
     Headline
          a package for computing equivariant Gröbner bases
     Description
          Text
	       EquivariantGB is a package for computing in polynomial rings with an infinite 
	       number of variables, but with an action of the infinite symmetric group.
	       Alternatively such a ring can be considered as the limit of a family of rings 
	       with symmetric action.  A representation of such a ring can be created using
	       the method @TO buildERing@.
	       
	       For example consider the ring R = $\mathbb{Q}[x_i,y_i \mid i,j\in \mathbb{Z}_{\geq 0}]$,
	       the coordinate ring of 2 by infinite matrices.  The infinite symmetric group acts
	       by permuting columns.
	  Example
	       R = buildERing({symbol x,symbol y},{1,1},QQ,4)
	       vars R
	  Text
	       Here the output ring stores only a truncation of the set of variables, with indices
	       from 0 to 3, but this bound will be adjusted as necessary in the computations.
	       
	       We now consider ideals of R that are closed under the symmetric group action.
	       For example, let I be the set of vanishing equations of the rank 1 matrices.
	       I is generated by all 2 by 2 minors $x_iy_j - x_jy_i$.
     Subnodes
          egb
	  buildERing
///

doc ///
     Key
          buildERing
	  (buildERing,List,List,Ring,ZZ)
	  [buildERing,MonomialOrder]
	  [buildERing,Degrees]
     Headline
          creates a ring to be used with other functions in the EquivariantGB package
     Usage
          R = buildERing(X,I,F,n)
     Inputs
          X:List
	       a list of symbols, one for each block of variables
	  I:List
	       a list of integers, the number of indices for each variable block
	  F:Ring
	       the coefficient ring
	  n:ZZ
	       the width bound
     Outputs
          R:Ring
	       a polynomial ring with coefficient field {\tt F}, and a block of variables for each entry of {\tt X}.
	       The coresponding integers in {\tt I} determine how many indices each block of variables has.
	       Variables with all index values from {\tt 0} to {\tt n-1} are included.
     Description
          Text
	       The object produced by {\tt buildERing} is a @TO Ring@ with additional stored information.
	       It is used to represent a polynomial ring {\tt F[Y]} with an infinite set of variables {\tt Y}, and
	       an action of the infinte symmetric group on {\tt Y} such that it is composed of a finite number
	       of orbits.  Macaulay2 cannot store a ring with an infinite number of variables, so the ring produced
	       contains only some of the variables.
	       
	       We assume {\tt Y} decomposes into a finite number of "blocks",  each block consisting of variables
	       of the form {\tt x_{(i_1,...,i_k)}} for some {\tt k} and each index {\tt i_j} ranging over all non-negative
	       integers.  The symbol {\tt x} to be used for each block is listed in {\tt X}.  The value of {\tt k}
	       for each block is listed in {\tt I}.  The variables in the ring output by {\tt buildERing} are those
	       with index values ranging from {\tt 0} to {\tt n-1}.  The same ring with a different index bound {\tt n} can
	       be produced by @TO (buildERing,Ring,ZZ)@.
	  Example
	       S = buildERing({symbol z}, {1}, QQ, 4)
	       vars S
	       coefficientRing S
          Example
               R = buildERing({symbol y, symbol x}, {2,1}, QQ, 3)
               vars R
	  Text
	       The monomial order can be chosen with optional argument @TO MonomialOrder@.
	       Currently valid choices are @TO Lex@, @TO GLex@ or @TO GRevLex@.
	       The variables are always ordered from the last
	       block to the first, with larger indices before smaller indices.  For blocks with multiple indices
	       the order of the variables is also lexicographic with first index most significant, followed by
	       the second, etc.
	       
	       The grading of the ring can be chosen with optional argument @TO Degrees@.
	       The grading is specified by listing the degree of each variable block.
	       Because the degree function must be invariant under the symmetric action,
	       every variable in the same block must have the same degree.
///
	       
doc ///
     Key
	  (buildERing,Ring,ZZ)
     Headline
          creates a ERing from existing ERing
     Usage
	  R = buildERing(S,n)
     Inputs
	  S:Ring
	       a Ring built by @TO buildERing@
	  n:ZZ
     Outputs
          R:Ring
	       a Ring with the same variable structure as {\tt S}, but with different index ranges.
     Description
     	  Example
	       S = buildERing({symbol z}, {1}, QQ, 2)
	       vars S
	       R = buildERing(S,5)
	       vars R
///

doc ///
     Key
          egb
	  (egb,List)
     Headline
          computes equivariant Gröbner bases
     Usage
          G = egb F
     Inputs
          F:List
	       a list of polynomials in a ring created by @TO buildERing@.
     Outputs
          G:List
	       an equivariant Gröbner basis for the equivariant ideal generated by F.
     Description
          Text
               {\tt egb} uses a variant of Buchberger's algorithm.
          Example
               R = buildERing({symbol x}, {1}, QQ, 2);
               egb {x_0 + x_1}
     Caveat
	  The output does not necessarily belong to the same ring as the input.
///

doc ///
     Key
          egbToric
	  (egbToric,RingMap)
     Headline
          computes the kernel of an equivariant monomial map
     Usage
          G = egbToric m
     Inputs
          m:RingMap
	       an equivariant monomial map between rings with symmetric action
     Outputs
          G:List
	       an equivariant Gröbner basis for the kernel of the map
     Description
          Text
               {\tt m} should be a monomial map between rings created by @TO buildERing@.  Such a map can be constructed with
	       @TO buildEMonomialMap@ but this is not required.
	  Text
	       For a map to ring {\tt R} from ring {\tt S}, the algorithm infers the entire equivariant map from where {\tt m} sends
	       the variable orbit generators of S.  In particular for each orbit of variables of the form {\tt x_{(i_1,...,i_k)}},
	       the image of {\tt x_{(0,...,k-1)}} is used.
	       
	       {\tt egbToric} uses an incremental strategy, computing Gröbner bases for truncations using @TO FourTiTwo@.  Because of FourTiTwo's
	       efficiency, this strategy tends to be much faster than general equivariant Gröbner basis algorithms such as @TO egb@.

	       In the following example we compute an equivariant Gröbner basis for the vanishing equations of the second Veronese of P^n,
	       i.e. the variety of n x n rank 1 symmetric matrices.
          Example
               R = buildERing({symbol x}, {1}, QQ, 2);
	       S = buildERing({symbol y}, {2}, QQ, 2);
               m = buildEMonomialMap(R,S,{x_0*x_1})
	       G = egbToric(m, OutFile=>stdio)
     Caveat
	  It is not checked if {\tt m} is equivariant.  Only the images of the orbit generators of the source ring are examined and
	  the rest of the map ignored.
     SeeAlso
          egb
	  buildEMonomialMap
///

doc ///
     Key
          buildEMonomialMap
	  (buildEMonomialMap,Ring,Ring,List)
     Headline
          builds an equivariant ring map
     Usage
          m = buildEMonomialMap
     Inputs
          R:Ring
	       the target ring with symmetric action, produced by @TO buildERing@
	  S:Ring
	       the source ring with symmetric action, produced by @TO buildERing@
	  L:List
	       a list of monomials in {\tt R}, the images of each of the variable orbit generators of {\tt S}
     Outputs
          m:RingMap
	       an equivariant monomial map to {\tt R} from {\tt S}
     Description
          Text
               {\tt R} and {\tt S} must be rings created by @TO buildERing@. Each orbit of variables in {\tt S} consists of variables of the form 
	       {\tt x_{(i_1,...,i_k)}} for some width {\tt k}.  {\tt L} is a list that for each such orbit gives the desired image of 
	       {\tt x_{(0,...,k-1)}}.  The rest of the map is extrapolated from these value.
          Example
               R = buildERing({symbol x, symbol y}, {1,1}, QQ, 3);
	       vars R
	       n = buildEMonomialMap(R,R,{x_0,x_0^3})
	       n(x_1^3*y_0)
	  Example
	       S = buildERing({symbol z}, {2}, QQ, 3);
	       vars S
               m = buildEMonomialMap(R,S,{x_0^2*y_1})
	       m(z_(1,2))

     Caveat
	  If a given variable orbit of {\tt S} has {\tt k} indices, then the target monomial list in {\tt L} should only use
	  indices between {\tt 0} and {\tt k-1}.
     SeeAlso
          egbToric
	  buildERing
///

doc ///
     Key
          reduce
	  (reduce,RingElement,List)
	  [reduce,Completely]
     Headline
          computes an equivariant normal form
     Usage
          r = reduce(f,F)
     Inputs
          f:RingElement
	       an element of a ring created by @TO buildERing@.
          F:List
	       a list of polynomials from the same ring as {\tt f}.
     Outputs
          r:RingElement
	       an equivariant normal form of {\tt f} with respect to {\tt F}.
     Description
          Text
	       Reduces {\tt f} by the Inc-orbits of the set {\tt F} until {\tt f} is in a normal form.
	       That is the lead monomial of {\tt f} is a standard monomial, not divisible by any monomial
	       in the orbit of any lead monomial of an element of {\tt F}.  If
	       {\tt F} is an equivariant Gröbner basis then {\tt r} is {\tt 0} if and only if {\tt f} is in the ideal
	       generated by the orbits of {\tt F}.
	       
	       If the optional argument {\tt Completely} is set to {\tt true} then normal form {\tt r} will
	       contain only standard monomials.  If {\tt F} is an equivariant Gröbner basis, then the completely
	       reduced normal form {\tt r} is uniquely determined, otherwise there is no such gaurantee.  
          Example
               R = buildERing({symbol x}, {1}, QQ, 3);
               reduce(x_0^2 + x_0*x_2, {x_1})
     Caveat
	  The output does not necessarily belong to the same ring as the input.
///

doc ///
     Key
          [egb,Algorithm]
	  Buchberger
	  Signature
	  Incremental
     Headline
          algorithm choice for egb
     Description
          Text
	       {\bf Buchberger:}  This is a top level implementation of the equivariant Buchberger algorithm.
	       
	       {\bf Incremental:} This strategy uses Macaulay2's built in Gröbner basis algorithm @TO gb@.
	       A Gröbner basis is computed for each truncated ideal.  If no new elements are discovered up
	       to Inc-action are discovered between the {\tt n} truncation and the {\tt 2n-1} truncation
	       for some {\tt n} larger than the width of the generators, then the result is returned.
	       
	       {\bf Signature:} This is an implementation of an equivariant variant of the Gao-Volny-Wang
	       signature based Gröbner basis algorithm.  Experimental!
          Example
               R = buildERing({symbol x}, {1}, QQ, 2);
               egb({x_0+x_1}, Algorithm=>Buchberger)
	       use R;
	       egb({x_0+x_1}, Algorithm=>Incremental)
	       use R;
	       egb({x_0+x_1}, Algorithm=>Signature)
///

doc ///
     Key
          OutFile
	  [egb,OutFile]
	  [egbToric,OutFile]
     Headline
          where to send messages
     Description
          Text
	       Both @TO egb@ and @TO egbToric@ have the option to print text while running,
	       describing what it is doing, giving timings of certain steps,
	       and printing intermediate outputs.
	       
	       Specify where to write this information.  Passing @TO null@ will discard the messages
	       Passing @TO stdio@ will print them in the Macaulay2 session.  You can also pass a file
	       to save this information.
///

doc ///
     Key
          exponentMatrix
	  (exponentMatrix,RingElement)
     Headline
          puts the exponent of a monomial into matrix form
     Usage
          A = exponentMatrix m
     Inputs
          m:RingElement
	       an element of a ring created by @TO buildERing@.
     Outputs
          A:Matrix
     Description
          Text
	       Let {\tt R} be a ring such that all variables have a single index on which the symmetric group acts.
	       Then monomials in {\tt R} can be represented by a {\tt k} by infinite exponent matrix where {\tt k}
	       is the number of variable orbits.
	       
	       This representation can be helpful for visualizing the structure of a monomial. 
          Example
               R = buildERing({symbol x, symbol y}, {1,1}, QQ, 4);
               exponentMatrix(x_0^3*y_2)
	       exponentMatrix(x_0*x_1*y_0*y_3)
     Caveat
	  The ring in which the monomial resides must have all variable orbits with exactly one index.
///

doc ///
     Key
          incOrbit
	  (incOrbit,RingElement,ZZ)
	  (incOrbit,List,ZZ)
	  [incOrbit,Symmetrize]
     Headline
          the increasing map orbit of an element
     Usage
          O = incOrbit(f,n)
	  O = incOrbit(F,n)
     Inputs
          f:RingElement
	       an element of a ring created by @TO buildERing@
	  F:List
	       a list of ring elements
	  n:ZZ
	       the index bound
     Outputs
          O:List
	       a list of all elements in the orbit of {\tt f} or {\tt F}
     Description
          Text
	       If {\tt F} is an equivariant Gröbner basis for invariant ideal {\tt I}
	       with respect to a width order then this method produces a traditional Gröbner basis
	       for the {\tt n}th truncation of {\tt I}.
	       
	       If the optional argument {\tt Symmetrize} is set to true, then the full S_n orbit
	       is produced.
          Example
               R = buildERing({symbol x}, {1}, QQ, 2);
	       O = incOrbit(x_0^2, 4)
               P = incOrbit(x_0 + x_1^2, 3, Symmetrize=>true)
     Caveat
	  The output is not necessarily in the same ring as the input.  The width bound of the ring
	  of the output will always be {\tt n}.
///

-------------------------------------
-- PriorityQueue
-------------------------------------
doc ///
    Key
        PriorityQueue
    Headline
        an efficient mutable priority queue implementation
    Description
        Text
            A priority queue is a data structure for storing a collection of totally ordered 
	    objects and keeping track of the minimum. This binomial heap implementation allows 
	    for efficiently adding a new element to the queue, accessing or deleting the minimum 
	    element, or merging two queues.  Efficiently means time logarithmic in the size of the
	    queue, or better.
    Subnodes
        priorityQueue
	(insert,PriorityQueue,Thing)
	(min,PriorityQueue)
	deleteMin
	pop
	mergePQ
	(length,PriorityQueue)
    Description
        Example
	    Q = priorityQueue {3,7,1,5}
	    min Q
	    deleteMin Q;
	    insert(Q,2);
	    min Q
	    R = priorityQueue {4,6,8};
	    QR = mergePQ(Q,R);
	    length QR
///

doc ///
    Key
        priorityQueue
	(priorityQueue,List)
	1:(priorityQueue)
    Headline
        create a new PriorityQueue
    Usage
        Q = priorityQueue()
        Q = priorityQueue L
    Inputs
        L:List
     	    a list of comparable elements
    Outputs
        Q:PriorityQueue
            a priority queue storing the elements of L
    Description
        Example
            Q = priorityQueue {1,5,2,-3,0}
            min Q
///

doc ///
    Key
	(min,PriorityQueue)
    Headline
        return the minimum element of the queue
    Usage
        m = min Q
    Inputs
        Q:PriorityQueue
    Outputs
        m:Thing
            the minimum of {\tt Q}
    Description
        Example
            Q = priorityQueue {1,5,2,-3,0}
            min Q
    Caveat
        If the queue is empty, {\tt null} is returned.
///

doc ///
    Key
	(insert,PriorityQueue,Thing)
    Headline
        insert a new element into the queue
    Usage
        Q = insert(Q,x)
    Inputs
        Q:PriorityQueue
	x:Thing
    Outputs
        Q:PriorityQueue
            the same PriorityQueue as the input, now with a new element inserted
    Description
        Example
            Q = priorityQueue {1,2,3}
            insert(Q,0)
	    insert(Q,4)
	    min Q
    Caveat
        The priority queue {\tt Q} is mutable and is altered by insert.
	{\tt Q} is also the output of the function.
///

doc ///
    Key
        deleteMin
	(deleteMin,PriorityQueue)
    Headline
        deletes the minimum element of the queue
    Usage
        Q = deleteMin Q
    Inputs
        Q:PriorityQueue
    Outputs
        Q:PriorityQueue
            the same PriorityQueue as the input, now with the minimum deleted
    Description
        Example
            Q = priorityQueue {1,2,3}
            deleteMin Q
	    min Q
    Caveat
        The priority queue {\tt Q} is mutable and is altered by deleteMin.
	{\tt Q} is also the output of the function.
    SeeAlso
        pop
///

doc ///
    Key
        pop
	(pop,PriorityQueue)
    Headline
        returns the minimum element of the queue and deletes it
    Usage
        m = pop Q
    Inputs
        Q:PriorityQueue
    Outputs
        m:Thing
            the minimum element
    Description
        Text
	    pop both returns the minimum element and deletes it from the queue.
        Example
            Q = priorityQueue {1,2,3}
            pop Q
	    pop Q
	    pop Q
	    pop Q
    Caveat
        Both {\tt pop Q} and {\tt min Q} return the same value, but Q is altered by pop.
    SeeAlso
        (min,PriorityQueue)
	deleteMin
///

doc ///
    Key
	(length,PriorityQueue)
    Headline
        returns the number of elements in the queue
    Usage
        n = length Q
    Inputs
        Q:PriorityQueue
    Outputs
        n:ZZ
    Description
        Example
            Q = priorityQueue {1,2,3}
            length Q
	    insert(Q,0)
	    length Q
///

doc ///
    Key
        mergePQ
	(mergePQ,PriorityQueue,PriorityQueue)
    Headline
        merges two queues
    Usage
        Q = mergePQ(Q,R)
    Inputs
        Q:PriorityQueue
	R:PriorityQueue
    Outputs
        Q:PriorityQueue
            the same PriorityQueue as the first input, now with the second merged into it
    Description
        Text
	    The output of the function is the priority queue whose elements are the disjoint union of
	    the elements of {\tt Q} and {\tt R}.  Note that {\tt Q} is altered in the process to become
	    the merged queue.
        Example
            Q = priorityQueue {2,4,6}
	    R = priorityQueue {1,3,5}
            mergePQ(Q,R)
	    pop Q
	    pop Q
    Caveat
        The priority queue {\tt Q} is mutable and is altered by mergePQ.
        The elements of {\tt Q} and {\tt R} must be comparable to each other.
///

undocumented {(buildERing,Sequence,Ring,ZZ)}

TEST ///
R = buildERing({symbol x, symbol y, symbol z}, {0,1,2}, QQ, 3);
assert(numgens R == 13)
S = buildERing(R,1)
assert(numgens S == 3)
T = buildERing(S,0)
assert(numgens T == 1)
///


TEST ///
R = buildERing({symbol a, symbol b}, {1,1}, QQ, 3);
S = buildERing({symbol c}, {2}, QQ, 3);
m = buildEMonomialMap(R,S,{a_0*b_1})
assert(m(S_0) == a_2*b_2)
assert(m(S_1) == a_2*b_1)
///

TEST ///
R = buildERing({symbol x}, {1}, QQ, 2);
G = egb({x_0 + x_1}, Algorithm=>Incremental)
assert(G == {x_0})
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2);
egb({y_(0,0) - x_0^2, y_(1,0) - x_1*x_0, y_(0,1) - x_0*x_1}, Algorithm=>Incremental)
-- egb({y_(0,1) - x_0^2*x_1, y_(1,0) - x_1^2*x_0}, Algorithm=>Incremental) -- takes too long
///

TEST ///
R = buildERing({symbol x, symbol y}, {1,1}, QQ, 3);
M = exponentMatrix(x_1^2*y_2)
assert(M == matrix{{0,2,0},{0,0,1}})
///


end

restart
needsPackage "EquivariantGB"
help egb
check "EquivariantGB"
uninstallPackage "EquivariantGB"
installPackage "EquivariantGB"

debug EquivariantGB

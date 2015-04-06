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
     PackageImports => {"FourTiTwo"},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => true 
     )

export {
     egb,
     egbToric,
     buildERing,
     buildEMonomialMap,
     Symmetrize,
     Completely,
     Diagonal,
     reduce,
     OutFile,
     Shift,
     shift,
     ShiftMonomial,
     shiftMonomial,
     divWitness
     }

protect \ { symbols, varIndices, varTable, varPosTable, semigroup, indexBound, rings, Seed, Extend, Sh }
     
--ERing = new Type of PolynomialRing
Shift = new Type of BasicList
ShiftMonomial = new Type of HashTable

-- In:
-- Out: 
egb = method(Options=>{Symmetrize=>false, OutFile=>null})
egb (List) := o -> F -> (
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
          newF = contractERing(newF,o.Symmetrize);
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
buildERing = method(Options=>{MonomialOrder=>Lex})
buildERing (List,List,Ring,ZZ) := o -> (X,s,K,n) -> buildERing((X,s,K),ZZ,n, MonomialOrder=>o.MonomialOrder)
buildERing (Ring,ZZ) := o -> (R,n) -> (
    if (R.rings)#?n then (R.rings)#n
    else buildERing((R.symbols,R.semigroup,coefficientRing R),R,n, MonomialOrder=>R.MonomialOrder)
    )
buildERing (Sequence,Ring,ZZ) := o -> (seq,oldR,n) -> (
     (X,s,K) := seq;
     variableIndices := s / (b->(toList ((b:0)..(b:n-1))));
     mo := o.MonomialOrder;
     if mo == Diagonal then (
	  variableIndices = apply(variableIndices, l->(
		    dPartition := partition(i->any(i, j->(#select(i, k->(k==j)) > 1)), l);
		    (dPartition#false)|(dPartition#true)));
	  mo = Lex;
	  );
     variableIndices = flatten apply(#s, b->(reverse apply(variableIndices#b, i->((1:b)|i))));
     moList := apply(s, b->(mo=>(n^b)));
     R := K[apply(variableIndices, i->(
		    if #i == 1 then X#(i#0)            --if block has only one variable, use no index
		    else if #i == 2 then X#(i#0)_(i#1) --if block has only one index, index by integers
		    else (X#(i#0))_(take(i,1-#i))      --if block has several indices, index by sequences
		    )), MonomialOrder => moList];
     R.symbols = X;
     R.varIndices = variableIndices;
     R.varTable = new HashTable from apply(#(R.varIndices), n->(R.varIndices#n => (gens R)#n));
     R.varPosTable = new HashTable from apply(#(R.varIndices), n->(R.varIndices#n => n));
     R.semigroup = s;
     R.indexBound = n;
     R.MonomialOrder = o.MonomialOrder;
     if oldR === ZZ then R.rings = new MutableHashTable
     else R.rings = oldR.rings;
     (R.rings)#n = R;
     R
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

listDivWitness = method(Options=>{Seed=>null})
listDivWitness (List,List,List,List) := o -> (v,w,vgaps,wgaps) -> (
    (vgdeg,wgdeg) := (vgaps,wgaps)/sum;
    if vgdeg > wgdeg or any(#v, i->(sum v#i > sum w#i)) then return (false, new MutableList);
    (vmax,wmax) := (v,w)/maxEntry;
    if vgdeg == 0 and wgdeg == 0 then (
    	if vmax == -1 then return (true, new MutableList);
	) else (
	wmax = max{wmax, maxEntry {wgaps} + 1};
	vmax = max{vmax, wmax + vgdeg - wgdeg};
	);
    sigma := if o.Seed =!= null then o.Seed else new MutableList from {-1};
    k     := if o.Seed =!= null then vmax   else 0;
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
	    sind := (1:ind#0)|apply(1..#ind-1, j->sigma#(ind#j));
	    (not S.varPosTable#? sind and vl#(R.varPosTable#ind) == 0)
	    or vl#(R.varPosTable#ind) <= wl#(S.varPosTable#sind)))
    )

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
	       sd := (shiftMap(R,sigma)) divisor;
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
     (big,small) := if n0 >= n1 then (0,1) else (1,0);
     n := (n0,n1);
     if k >= n#small then return {};
     flatten apply(subsets(n#big + k, n0), sImage->(
	       apply(subsets(sImage, n#small - k), stImage->(
			 sPos := 0;
			 stPos := 0;
			 tImage := select(n#big + k, i->(
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
shiftMap(Ring,Shift) := o -> (R,s) -> (
    s = take(s,width R);
    S := R;
    if o.Extend then (
	n := width R;
	s = s*shift(0:n-1);
	S = buildERing(R,max s);
	);
    mapList := apply(R.varIndices, ind->(
	    indnew := new MutableList from ind;
	    for j from 1 to #ind-1 do (
	       	if ind#j >= #s or s#(ind#j) < 0 or s#(ind#j) >= width S then return 0_S
	       	else indnew#j = s#(ind#j);
	       	);
	    S.varTable#(toSequence indnew)
	    ));
    --print(s,R.varIndices,mapList);
    map(R,S,mapList)
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
     apply(permutations IS, p->((shiftMap(R,p)) f))
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
contractERing = (F,sym) -> (
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

--In: F, a list of polynomials
--Out: p, ZZ: the largest index appearing in F.  If all elements of F are 0, return -1.
maxIndex = F -> (
     p := position(indexSupport(F), i->(i > 0), Reverse=>true);
     if p === null then p = -1;
     p
     )
 
maxEntry = V -> (
    W := apply(V, v->position(v, i->(i > 0), Reverse=>true));
    max apply(W, i-> if i === null then 0 else i)
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
buildEMonomialMap = (S,R,F) -> (
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

egbToric = M -> (
     R := source M;
     S := target M;
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
	  print k;
	  Rnew := buildERing(R,k);
	  Tnew := buildEMonomialMap(S,Rnew,F);
	  Gnew := transpose toricGroebner matrixFromMap Tnew;
	  shifts := subsets(k,k-1);
	  sList := apply(shifts, s -> (
		    m := shiftMap(Rnew,s)*ringMap(Rnew,R);
		    matrixFromMap m
		    ));
	  L1 := matrix{apply(sList, s->s*G)};
	  L1 = time sort L1;
	  uniqueCols := time select(numgens source L1, i->(i == 0 or L1_{i} != L1_{i-1}));
	  L1 = L1_uniqueCols;
	  print numgens target Gnew;
	  print numgens source Gnew;
	  L2 := time sort Gnew;
	  if L1 != L2 then (print "new stuff found"; lastNewk = k; lastNewG = Gnew);
	  R = Rnew;
	  T = Tnew;
	  G = Gnew;
	  );
     k = lastNewk;
     R = buildERing(R,lastNewk);
     GBtrad := flatten entries sort gens toBinomial(transpose lastNewG, R);
     seen := new MutableHashTable;
     GB := select(GBtrad, g -> (
	       if not seen#?g then (
	       	    n := width g;
	       	    shifts := subsets(k,n);
		    shifts = apply(shifts, s->(s|(toList ((k-n):(-1)))));
		    --print shiftMap(R,shifts#0);
	       	    for s in shifts do seen#((shiftMap(R,s))g) = true;
		    true
		    )
	       else false
	       ));
     GB
     )

shift = method()
shift (List) := L -> new Shift from L
shift (MutableList) := L -> new Shift from L
shift (Sequence) := S -> shift toList S
Shift * Shift := (I,J) ->
    apply(J, j->(if I#?j then I#j else j + (last I) - #I + 1))
gaps = I -> (
    k := width I;
    g := new MutableList from (k:1);
    for i in I do g#i = 0;
    toList g
    )
take (Shift,ZZ) := (I,n) -> (
    k := length I;
    if k < n then return shift (toList I) | (toList (k + degree I)..(n-1 + degree I));
    if k > n then return shift take(I,n);
    I
    )
degree (Shift) := I ->
    if #I == 0 then 0 else last I + 1 - #I

shiftMonomial = method()
shiftMonomial (RingElement,Shift) := (p,I) ->
    new ShiftMonomial from hashTable {Monomial=>(leadTerm p), Sh=>I}
ShiftMonomial * ShiftMonomial := (S,T) -> (
    (p,I) := (S.Monomial, S.Sh);
    (q,J) := (T.Monomial, T.Sh);
    R := ring q;
    Iq := shiftMap(R,take(I,width R))*q;
    (p,Iq) = matchRing(p,Iq);
    shiftMonomial(p*Iq, I*J)
    )
matchRing = method()
matchRing (RingElement,RingElement) := (p,q) -> (
    (R,S) := (p,q)/ring;
    if width R < width S then p = ringMap(S,R)*p;
    if width R > width S then q = ringMap(R,S)*q;
    (p,q)
    )
matchRing (RingElement,ZZ) := (p,n) -> (
    S := buildERing(ring p, n);
    ringMap(S,ring p)*p
    )
    
width Ring := R -> R.indexBound
width RingElement := p -> (
    n := position(indexSupport{p}, i->(i > 0), Reverse=>true) + 1;
    if n === null then p = n;
    n
    )
width Shift := I -> if #I == 0 then 0 else last I + 1
width List := L -> max(L / width)

beginDocumentation()

doc ///
     Key
          EquivariantGB
     Headline
          a package for computing equivariant Gröbner bases
     Subnodes
          egb
	  buildERing
///

doc ///
     Key
          buildERing
	  (buildERing,List,List,Ring,ZZ)
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
	  n:ZZ
     Outputs
          R:Ring
	       a Ring with coefficient field {\tt F}, and a block of variables for each entry of {\tt X}.
	       The coresponding integers in {\tt I} determine how many indices each block of variables has.
	       Variables with all index values from {\tt 0} to {\tt n-1} are included.
     Description
          Text
	       For now the monomial order on {\tt R} is always @TO Lex@, with variables ordered from the last
	       block to the first, with larger indices before smaller indices.  For blocks with multiple indices
	       the first index is most significant, followed by the second, etc.
          Example
               R = buildERing({symbol y, symbol x}, {2,1}, QQ, 2)
               vars R
               coefficientRing R
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
	       a Ring built by {\tt buildERing}
	  n:ZZ
     Outputs
          R:Ring
	       a Ring with the same variable structure as {\tt S}, but with different index ranges.
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
          reduce
	  (reduce,RingElement,List)
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
          Example
               R = buildERing({symbol x}, {1}, QQ, 3);
               reduce(x_0^2 + x_0*x_2, {x_1})
     Caveat
	  The output does not necessarily belong to the same ring as the input.
///

undocumented {Symmetrize, Completely, [egb,Symmetrize]}

TEST ///
needs concatenate(EquivariantGB#"source directory","./examples.m2")
I = exampleISSAC()
assert(toString egb(I,Symmetrize=>true) == toString {x_1*x_0^3, x_1^2*x_0^2, x_1^3*x_0, x_2*x_1*x_0^2, x_2*x_1^2-x_2*x_0^2, x_2^2*x_0-x_1^2*x_0, x_2^2*x_1-x_1*x_0^2})
///

end

restart
needsPackage "EquivariantGB"
help egb
check EquivariantGB
installPackage "EquivariantGB"

debug EquivariantGB

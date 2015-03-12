newPackage(
     "EquivariantGB",
     Version =>"0.1",
     Date => "2013",
     Headline => "Equivariant Groebner bases and related algorithms",
     HomePage => "",
     Authors => {
    	  {Name => "Chris Hillar", Email => "chillar@msri.org"},
	  {Name => "Robert Krone", Email => "krone@math.gatech.edu"},
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     DebuggingMode => true 
     )

export {
     egb,
     buildERing,
     Symmetrize,
     Completely,
     Diagonal,
     reduce,
     OutFile
     }

protect \ { symbols, varIndices, varTable, varPosTable, semigroup, indexBound, diagonalSlices }
     
--ERing = new Type of PolynomialRing
     
spoly = (f,g) -> (
     l := lcm(leadMonomial f,leadMonomial g);
     return (l//(leadTerm f))*f - (l//(leadTerm g))*g;
     )

-- In: v, a polynomial
--     w, a polynomial 
-- Out: (b, M)
--     b, a boolean, whether there is a semigroup element M s.t. M*in(v) divides in(w)
divWitness = (v,w) -> (
     R := ring v;
     assert(numgens ring v == numgens ring w);
     n := R.indexBound;
     vl := (listForm leadTerm v)#0#0;
     wl := (listForm leadTerm w)#0#0;
     diag := (vl,b,i) -> vl#(R.varPosTable#((1:b)|(R.semigroup#b:i)));
     vmax := maxIndex {leadTerm v};
     wmax := maxIndex {leadTerm w};
     if vmax == -1 then return (true, toList(0..n-1));
     if wmax == -1 then return (false, {});
     sigma := new MutableList from (n:-1);
     k := 0;
     while true do (
	  while true do (
	       i := k;
	       for j from (sigma#k)+1 to wmax do (
		    if all(#(R.semigroup), b->(diag(vl,b,i) <= diag(wl,b,j))) then (
			 sigma#i = j;
			 i = i+1;
			 );
		    );
	       if i <= vmax then k = k-1 else break;
	       if k < 0 then return (false, {});
	       );
	  for i from vmax+1 to n-1 do sigma#i = sigma#vmax + i - vmax;
	  if all(R.varIndices, ind->(
		    sind := (1:ind#0)|apply(1..#ind-1, j->sigma#(ind#j));
		    (not R.varPosTable#? sind and vl#(R.varPosTable#ind) == 0)
		    or vl#(R.varPosTable#ind) <= wl#(R.varPosTable#sind))) 
		    then break;
	  k = vmax;
	  );
     return (true, toList sigma);
     )

basicReduce = method(Options=>{Completely=>false})
basicReduce (RingElement, BasicList) := o -> (f,B) -> (
     B = select(toList B,b->b!=0);
     R := ring f;
     n := R.indexBound;
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
     	  Rnew := buildERing(R,(R.indexBound)+1);
     	  RtoRnew := ringMap(Rnew,R);
     	  F = RtoRnew\F;
     	  f = RtoRnew f;
     	  r = basicReduce(f,F,Completely=>o.Completely);
	  R = Rnew
	  );
     (r,R,f,F)
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
buildERing (Ring,ZZ) := o -> (R,n) -> buildERing(R.symbols, R.semigroup, coefficientRing R, n, MonomialOrder=>R.MonomialOrder)
buildERing (List,List,Ring,ZZ) := o -> (X,s,K,n) -> (
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
     R
     )

-- In: F, current generators list
--     k, ZZ (the number of "gaps")  
-- Out: the updated list of generators
-- Description: takes all s-pairs using k "gaps" when "interlacing", reduces, and interreduces. 
processSpairs = method(Options=>{Symmetrize=>false})
processSpairs (List,ZZ) := o -> (F,k) -> (
     --if o.Symmetrize then F = interreduce'symmetrize F; 
     R := ring F#0;
     n := R.indexBound;
     S := buildERing(R,n+k);
     F = F / ringMap(S,R);
     maxIndices := apply(F, f->(maxIndex {f}));
     --sp := shiftPairs(n,n,k);
     --print apply(sp,t->matrix first t||matrix last t);
     Fnew := {};
     for i from 0 to #F-1 do (
	  for j from 0 to i do (
	       sp := shiftPairs(maxIndices#i + 1, maxIndices#j + 1, k);
	       for st in sp do (
		    (s,t) := st;
		    f := spoly((shiftMap(S,s)) F#i, (shiftMap(S,t)) F#j);
		    local r;
		    (r,S,f,F) = reduce2(f,F);
		    --if f != 0 then print (i,j,s,t);
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
			 (sImage,tImage)
			 ))
	       ))			 
     )

--In: R, a ring
--    shift, a list of integers
--Out: a map from R to R where index i is mapped to shift#i.
--     If shift#i == -1 then all variables with index i go to 0_R.
shiftMap = (R,shift) -> (
     mapList := apply(R#varIndices, ind->(
	       indnew := new MutableList from ind;
	       for j from 1 to #ind-1 do (
		    if ind#j >= #shift or shift#(ind#j) < 0 or shift#(ind#j) >= R.indexBound then return 0_R
		    else indnew#j = shift#(ind#j);
		    );
	       R.varTable#(toSequence indnew)
	       ));
     map(R,R,mapList)
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
-- Out: ...
interreduce = method(Options=>{Symmetrize=>false})
interreduce (List) := o -> F -> (
     --Reduce elements of F with respect to each other.
     --print "-- starting \"slow\" interreduction";
     R := ring first F;
     n := R.indexBound;
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
--   while true do (
--	  F = select(F, f->f!=0);
--     	  i := position(0..#F-1, k-> 
--	       any(#F, j-> j != k and first divWitness(F#j,F#k)));
--	  if i =!= null then (
--	       Fout := drop(F,{i,i});
--	       local r; local f;
--	       (r,R,f,Fout) = reduce2(F#i,Fout);
--	       F = insert(i, makeMonic r, Fout);
--	       )
--	  else break;
--	  );
     F = apply(F, f->(
	       g := f - leadTerm(f);
	       local r;
	       (r,R,g,F) = reduce2(g,F,Completely=>true);
	       makeMonic(leadTerm(f) + r)
	       ));
     --Prune unused variables from R.
     newn := 0;
     if o.Symmetrize then (
	  iS := indexSupport F;
	  shift := toList apply(iS, j->(if j > 0 then (newn = newn+1; newn-1) else -1));
	  F = F / shiftMap(R,shift);
	  )
     else newn = maxIndex(F) + 1;
     S := buildERing(R,newn);
     F / ringMap(S,R)
     ) 

--In: F, a list of polynomials
--Out: a list, the number of variables represented in F which have an index equal to i for each i from 0 to n-1
indexSupport = F -> (
     R := ring first F;
     n := R.indexBound;
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

-- In:
-- Out: 
egb = method(Options=>{Symmetrize=>false, OutFile=>null})
egb (List) := o -> F -> (
     g := o.OutFile;
     n := (ring first F)#indexBound;
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
	       << "; indices: " << (maxIndex F) + 1
	       << "; max deg: " << max((F / degree) / first)
	       << "." << endl;
	       );
	  g << "-- " << k << " gap Spairs " << flush;
	  newF := printT(timing processSpairs(F,k, Symmetrize => o.Symmetrize), g);
	  g << "--   " << (#newF - #F) << " new" << endl;
	  g << "--   interreduce";
	  newF = printT(timing interreduce(newF, Symmetrize => o.Symmetrize), g);
	  R := ring first F; 
	  S := ring first newF;
	  newstuff := numgens R != numgens S or sort (F / ringMap(S,R)) != sort newF;
	  F = newF;
	  if newstuff then k = 0
	  else k = k+1;
	  n = S.indexBound;
	  );
     F
     )

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

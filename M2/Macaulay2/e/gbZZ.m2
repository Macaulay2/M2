-- Toplevel implementation of Groebner basis of an ideal or submodule
-- in ZZ[x1..xn].  For now, ignore quotient ring.

-- spair handling

-- termideal

-- GB

-----------------------------
-- Data types ---------------
-----------------------------
-- s pair events:
-- {GENERATOR, f} stored by degree
-- {SPAIR, gsyz}
--
-- term ideal element
-- {coefficient, exponent vector, gsyz, bag, deg}
-- 
-- groebner basis element
--   {coefficient, exponent vector, gsyz, f, deg}

view = method()

myAutoReduce = (v) -> (
     -- v should be a list of vectors
     if #v <= 1 then v else (
     m := matrix v;
     p := sortColumns m;
     -- now make the inverse map
     pinv := new MutableList from splice {#p : -1};
     scan(#p, i -> pinv#(p#i) = i);
     pinv = toList pinv;
     m1 = m_p;
     sendgg(ggPush m1, ggautoreduce);
     m2 = getMatrix ring v#0;
     m = map(target m, source m, m2);
     apply(pinv, i -> m_i)))
     
     
     
-----------------------------
-- SPairTable ---------------
-----------------------------

SPairTable = new Type of MutableHashTable
SPairEvent = new Type of List

makeSPairTable = () -> (
     result := new SPairTable;
     result.table = new MutableHashTable;
     result)

insert = method()
insert(SPairTable,SPairEvent,ZZ) := (SP, f, deg) -> (
     -- insert f as degree d.
     if SP.table#?deg then 
       SP.table#deg = prepend(f, SP.table#deg)
     else
       SP.table#deg = {f};
     )

next = method()
next(SPairTable) := (SP) -> (
     if #SP.currentSet === 0 then null
     else (
	  result := SP.currentSet#0;
	  SP.currentSet = drop(SP.currentSet,1);
	  result)
     )

nextDegree = method()
nextDegree(SPairTable) := (SP) -> (
     -- determine the lowest degree appearing in the table.
     -- sets this degree up for next* to work.
     -- possibly sorts these elements too, using 'compare'.
     k := keys SP.table;
     if #k === 0 then null 
     else (
       deg := SP.currentDegree = min k;
       SP.currentSet = SP.table#deg;
       remove(SP.table,deg);
       SP.currentSet = sort SP.currentSet;
       {deg, #SP.currentSet}
     ))

view SPairEvent := (e) -> toString e
view SPairTable := (SP) -> (
     degs := sort keys SP.table;
     scan(degs, d -> (
     	       << "degree " << d << " npairs " << #SP.table#d << endl;
	       scan(SP.table#d, e -> ( << "  " << toString e << endl;));)))
///
R = ZZ[a,b,c,d]
m = matrix{{2*a*b-1,3*b-5, 2*d-7}}
makeGB m
peek oo

S = makeSPairTable()
insert(S,new SPairEvent from {1},2)
insert(S,new SPairEvent from {a2},3)
insert(S,new SPairEvent from {GENERATOR,hi}, 2)
nextDegree S
next S
next S
next S
nextDegree S
next S
next S
nextDegree S
///

----------------------------------
-- TermIdeal ---------------------
----------------------------------

-- a sorted list of terms, each tagged with ???

load "LLL.m2"

TermIdeal = new Type of MutableHashTable

Term = new Type of MutableList
-- fields:
--   exponents
--   coefficient
--   vec
--   anything else...

makeTerm = (f,fsyz) -> (
     new Term from {
	  first exponents leadTerm f,
	  leadCoefficient f,
	  fsyz})

insert(TermIdeal, Term) := (T,e) -> (
     -- e should be a list {exponents, coeff, vec, ...}
     T.elements = append(T.elements, e))

replace = method()
replace(TermIdeal, ZZ, Vector) := (T,c,gsyz) -> (
     -- Replace the last element of T with a new coeff/vector.
     last := #T.elements - 1;
     oldelem := T.elements#-1;
     newelem := new Term from {oldelem#0, c, gsyz};
     T.elements = append(drop(T.elements, {last,last}), newelem);
     )

replaceTerm = (T,exp,c,gsyz) -> (
     telem := select(1, T.elements, t -> t#0 === exp);
     telem = first telem;
     telem#1 = c;
     telem#2 = gsyz;
     )

exactMonomial = (T,exp) -> (
     telem := select(1, T.elements, t -> t#0 === exp);
     #telem > 0)

findAllMonomialDivisors = method()
findAllMonomialDivisors(TermIdeal, List) := (T,fexp) -> (
     tmdivides := (gexp, fexp) -> all(0..#gexp-1, i -> gexp_i <= fexp_i);
     select(T.elements, g -> tmdivides(g#0,fexp))
     )

mygcd = (listg) -> (
     coeffg := apply(listg, g -> g#1);
     ans := gcdLLL coeffg;
     {ans#0, flatten entries ans#1_{numgens source ans#1 - 1}})

search = method()     
search2 = method()     
search2(TermIdeal, ZZ, List) := (T,fcoeff, fexp) -> (
     R := ring T;
     t := findAllMonomialDivisors(T,fexp);
     if #t === 0 then FALSE
     else (
	  g = mygcd t;
	  if fcoeff % (g#0) === 0 then (
	       -- have a divisor
	       c := fcoeff // g#0;
	      {TERM, sum apply(#t, i -> (
			     exp := fexp - t#i#0;
			     tvec := t#i#2;
			     c * g#1#i * R_exp * tvec))})
	  else (
	       c = gcdCoefficients(fcoeff, g#0);
	      {MONOMIAL, gcd(fcoeff,g#0), c#0, sum apply(#t, i -> (
			     exp := fexp - t#i#0;
			     tvec := t#i#2;
			     c#1 * g#1#i * R_exp * tvec))}
	      )))
     
search(TermIdeal, ZZ, List) := (T,fcoeff, fexp) -> (
     R := ring T;
     t := findAllMonomialDivisors(T,fexp);
     if #t === 0 then FALSE
     else (
	  g = mygcd t;
	       -- have a divisor
	       c := fcoeff // g#0;
	       v := sum apply(#t, i -> (
			 exp := fexp - t#i#0;
			 tvec := t#i#2;
			 c * g#1#i * R_exp * tvec));
	      {if fcoeff % (g#0) === 0 then TERM else MONOMIAL,
	       v}
	  ))

search(TermIdeal, ZZ, List) := (T,fcoeff, fexp) -> (
     R := ring T;
     t := findAllMonomialDivisors(T,fexp);
     if #t === 0 then FALSE
     else (
	  g = mygcd t;
	  typ := if fcoeff % (g#0) === 0 then TERM else MONOMIAL;
	  if typ === MONOMIAL then (
	       telem := select(1, t, t1 -> t1#0 === fexp);
	       if #telem > 0 then 
	            return {EXACTMONOMIAL, telem#0#2});

	  if typ === TERM then (
	       -- have a divisor
	       c := fcoeff // g#0;
	      {TERM, sum apply(#t, i -> (
			     exp := fexp - t#i#0;
			     tvec := t#i#2;
			     c * g#1#i * R_exp * tvec))})
	  else (
	       c = gcdCoefficients(g#0, fcoeff);
	       e := gcd(fcoeff,g#0);
	      {MONOMIAL, sum apply(#t, i -> (
			     exp := fexp - t#i#0;
			     tvec := t#i#2;
			     g#1#i * R_exp * tvec)), 
		       c, 
		       {fcoeff//e, - g#0 // e}}
	  ))
     )

makeTermIdeal = method()

makeTermIdeal Ring := (R) -> (
     T := new TermIdeal;
     T.elements = {};
     T.ring = R;
     T
     )

makeTermIdeal Matrix := (m) -> (
     -- m is a 1 row matrix.  Make the corresponding term ideal,
     -- via minimal generators...
     Gsyz := source m;
     idGsyz := id_Gsyz;
     n := numgens Gsyz;     
     melements := first entries m;
     elems = apply(n, i -> (
	       makeTerm(melements#i, idGsyz_i)
	     ));
     makeMinimalTermIdeal(ring m, elems))

makeMinimalTermIdeal = method()
makeMinimalTermIdeal(Ring, List) := (R,elements) -> (
     -- elements should be a list of Term's
     --   {exponent vector, coeff, degree, vector, anything else}
     elems := sort elements;
     T = makeTermIdeal R;
     scan(elems, e -> (
	       eexp := e#0;
	       ecoeff := e#1;
	       evec := e#2;
	       ans := search2(T,ecoeff, eexp);
	       if ans === FALSE then 
	           insert(T,e)
	       else if ans#0 === MONOMIAL then (
		    if exactMonomial(T,eexp) then (
	       	    	 newcoeff := ans#1;
	       	    	 replace(T,newcoeff,ans#2 * evec + ans#3)
			 )
		    else (
			 -- 
			 newcoeff = ans#1;
			 newvec := ans#2 * evec + ans#3;
			 enew := new Term from {eexp, newcoeff, newvec};
			 insert(T,enew)
			 ))));
     T
     )

insert(TermIdeal,RingElement,Vector) := (T,f,v) -> (
     insert(T,makeTerm(f,v)))
     
search(TermIdeal,RingElement) := (T,f) -> (
     search(T,leadCoefficient f, first exponents leadTerm f))

search(TermIdeal,Vector) := (T,f) -> (
     f0 := (leadTerm f)_(leadComponent f);
     search(T,leadCoefficient f0, first exponents f0))

findAllMonomialDivisors(TermIdeal, RingElement) := (T,f) -> (
     findAllMonomialDivisors(T, first exponents leadTerm f))

view TermIdeal := (T) -> (
     scan(T.elements, t -> ( << "  " << toString toList t << endl;)))

--------------------------------
-- Groebner basis computation --
--------------------------------
GBZZComputation = new Type of MutableHashTable

GBElement = new Type of MutableHashTable
-- fields:
--    g -- polynomial
--    coefficient -- leadCoefficient g
--    exponents -- first exponents leadTerm g
--    gsyz -- Gsyz_i, if this is the i-th element

makeGenerators = (SP, m) -> (
     -- SP = SPairTable
     -- m  = Matrix
     for i from 0 to numgens source m - 1 do (
          v := m_i;
	  d := first degree v;
	  e := new SPairEvent from {GENERATOR, v};
	  insert(SP,e,d);
	  );
     )

makeGB = (m) -> (
     -- m is a matrix in a polynomial ring over ZZ.
     R := ring m;
     nrows := numgens target m;
     result = new GBZZComputation;
     result.ring = R;
     result.gens = m;
     result.Gsyz = R^3000; -- an awful hack...
     result.spairs = makeSPairTable();
     makeGenerators(result.spairs, m);
     --
     result.searchtable = new MutableList from {nrows : null};
     for i from 0 to nrows-1 do result.searchtable#i = makeTermIdeal R;
     -- set up Groebner basis
     result.gb = new MutableHashTable;
     result.ngb = 0;
     -- Set up original state
     result.currentDegree = min apply(degrees target m, first);
     result.firstInDegree = 0;
     result.state = StateNewDegree;
     result.returncode = NotDone;
     result.nextpair = 0;
     result)

coeffsyz = (a,b) -> (
     -- a, b should be integers
     g := gcd(a,b);
     {b//g, -a//g})

monsyz = (a,b) -> (
     {
     apply(#a, i -> (c := b#i - a#i; if c < 0 then 0 else c)),
     apply(#a, i -> (c := a#i - b#i; if c < 0 then 0 else c))
     }
     -- a and b should be lists of integers of the same length
     )

degreeHack = (G,v) -> (
     x := leadComponent v;
     f := (leadTerm v)_x;
     first (degree f) + G.gb#x.sugar)

updatePairs = (G,n) -> (
     -- Find the pairs coming from the n-th element.
     R := G.ring;
     h := G.gb#n;  -- Each is {coeff,exponents,vec,gb elem}
     p = for i from 0 to n-1 list (
	  -- Only do those elements with same component...
	  g := G.gb#i;
	  mons := monsyz(h.exponents, g.exponents);
	  coeffs := coeffsyz(h.coefficient, g.coefficient);
	  e := coeffs#0 * R_(mons#0) * h.gsyz
	              + coeffs#1 * R_(mons#1) * g.gsyz;
	  new Term from {mons#0,coeffs#0,e}
	  );
     T = makeMinimalTermIdeal(ring G,p);
     scan(T.elements, f -> (
	       e := new SPairEvent from {SPAIR,f#2};
	       insert(G.spairs,e,degreeHack(G,e#1))));
     )

applyMap = (G, vec) -> (
     -- elems should be a GBElement: a hash table with a field 'g'
     vecvec = vec;
     result := 0_(target G.gens);
     while vec != 0 do (
	  v := leadTerm vec;
	  vec = vec - v;
	  x := leadComponent v;
	  f := v_x;
	  result = result + f * (G.gb#x.g));
     result)

reduce = (G,f) -> (
     ffff = f;
     -- Only reduce f until the lead term is not in in(G).
     while f != 0 do (
	  x := leadComponent f;
	  ans := search(G.searchtable#x, f);
	  --<< " f = " << f << endl << endl;
	  --<< "ans = " << ans << endl << endl;
	  if ans === FALSE then
	      return f
	  else (
	       if ans#0 === TERM then (
		    -- The whole term is in the term ideal
		    -- ans#1 is a vector describing the reduction
		    -- so there is nothing else to do.
	       	    g = applyMap(G,ans#1);
	       	    f = f-g;
		    )
	       else if ans#0 === MONOMIAL then (
		    -- The monomial is in the term ideal, but
		    -- this is a lead term not in there.  So
		    -- at this point, we should return f.
	       	    g = applyMap(G,ans#1);
		    f0 = ans#2#0 * g + ans#2#1 * f;
		    f = ans#3#0 * g + ans#3#1 * f;
		    insertGB(G,f0,false);
		    -- Now f will reduce next time...
		    -- MES: f0 may be a minimal generator...
	       	    --f = f-g;
		    --return f;
		    )
	       else (
		    -- ans#0 === EXACTMONOMIAL
		    -- We must do a replacement, and then 
		    -- continue the reduction.  Here ans#1 is
		    -- a vector, which in this case should be a
		    -- unit vector.
		    x = leadComponent (ans#1);
		    f = replaceGB(G,x,f);
		    ));
	  );
     f
     )

halfdivide = (a,b) -> (
     -- assumed: b >= 0, a != 0.
     r := a % b;
     c := a // b;
     if r > b // 2 then c=c+1;
     c)
     
reduceTail = (G,f) -> (
     ffff = f;
     -- Reduce the tail of f completely.
     --<< "-- reducing " << f << endl;
     result := leadTerm f;
     f = f - result;
     while f != 0 do (
	  x := leadComponent f;
	  ans := search(G.searchtable#x, f);
	  --<< "  f = " << f << endl << endl;
	  --<< "  ans = " << ans << endl << endl;
	  --<< "  result = " << result << endl;
	  --<< "----" << endl;
	  if ans === FALSE then (
	      result = result + leadTerm f;
	      f = f - leadTerm f;
	      )
	  else (
	       if ans#0 === TERM then (
		    -- The whole term is in the term ideal
		    -- ans#1 is a vector describing the reduction
		    -- so there is nothing else to do.
	       	    g = applyMap(G,ans#1);
	       	    f = f-g;
		    )
	       else if ans#0 === MONOMIAL then (
		    -- The monomial is in the term ideal, but
		    -- this is a lead term not in there.  So
		    -- at this point, we should return f.
		    g = applyMap(G,ans#1);
		    e := leadCoefficient g;
		    c := halfdivide(leadCoefficient f, e);
		    if c =!= 0 then (
			 f = f - c*g;
			 );
		    result = result + leadTerm f;
		    f = f - leadTerm f;
		    )
	       else (
		    -- ans#0 === EXACTMONOMIAL
		    x = leadComponent (ans#1);
		    g = (G.gb#x).g;
		    e = (G.gb#x).coefficient;
		    c = halfdivide(leadCoefficient f, e);
		    if c =!= 0 then (
			 f = f - c*g;
			 );
		    result = result + leadTerm f;
		    f = f - leadTerm f;
		    ));
	  );
     result
     )

insertGB = (G,f,ismin) -> (
     if leadCoefficient f < 0 then f = -f;
     f = reduceTail(G,f);
     makeGBElement := (n,f,ismin) -> (
     	  elem := new GBElement;
     	  elem.g = f;
     	  elem.isMinimal = ismin;
	  elem.coefficient = leadCoefficient f;
	  elem.exponents = first exponents (leadTerm f)_(leadComponent f);
	  elem.gsyz = G.Gsyz_n;
	  elem.sugar = G.currentDegree;
	  elem);
     elem := makeGBElement(G.ngb,f,ismin);
     t := new Term from {elem.exponents, elem.coefficient, elem.gsyz,
	                    G.currentDegree};
     G.gb#(G.ngb) = elem;
     G.ngb = G.ngb + 1;
     insert(G.searchtable#(leadComponent f), t);
     time autoReduceStep(G,G.firstInDegree,G.ngb-1);
     )


replaceGB = (G,x,f) -> (
     -- G: GB
     -- x: the integer index to change
     -- f: the new polynomial
     -- leadmonom(f) should be identical to leadmonom(gb(x)),
     --   but have a smaller lead coeff
     gelem := G.gb#x;
     g := gelem.g;
     c := leadCoefficient g;
     d := leadCoefficient f;
     xy := gcdCoefficients(c,d);
     e := gcd(c,d);
     c' := c // e;
     d' := d // e;
     fnew := d' * g - c' * f;
     gnew := xy#0 * g + xy#1 * f;
     gelem.g = gnew;
     gelem.coefficient = leadCoefficient gnew;
     replaceTerm(G.searchtable#(leadComponent g), 
	  gelem.exponents,
	  gelem.coefficient,
	  gelem.gsyz);
     fnew
     )

spairStep = (G) -> (
     sp := next(G.spairs);
     if sp === null then return false;
     local f;
     if sp#0 == SPAIR then 
       f = applyMap(G,sp#1) -- computes s pair
     else
       f = sp#1;
     --
     << "." << see leadTerm f << flush;
     f = reduce(G,f);  -- note: this may update some elements of the GB.
     if f != 0 then insertGB(G,f,sp#0 === GENERATOR)
     else << ",0" << flush;
     true
     )

newPairsStep = (G) -> (
     n := G.nextpair;
     if n >= G.ngb then 
       false
     else (
       updatePairs(G,n);
       G.nextpair = G.nextpair + 1;
       true
     ))

autoReduceStep = (G,lo,hi) -> (
     -- grab all of the elements of degree G.currentDegree
     -- and call myAutoReduce.  Put these elements back.
     g := apply(toList(lo..hi), j -> (G.gb#j).g);
     g = myAutoReduce g;
     scan(#g, j -> (G.gb#(lo+j)).g = g#j);
     )

step = (G) -> (
     -- Returns what??
     if G.state === StateNewDegree then (
	  -- grab the current set of pairs
	  olddegree := G.currentDegree;
	  ans := nextDegree G.spairs;
	  if ans === null then return false;
	  G.currentDegree = ans#0;
	  if G.currentDegree =!= olddegree then (
	       -- a new degree
	       G.firstInDegree = G.ngb;
	       );
	  << " degree " << G.currentDegree << " #pairs = " << ans#1 << endl;
	  G.state = StateSPairs;
	  -- Tail reduce each element so far found?
	  --scan(keys G.gb, i -> G.gb#i.g = reduceTail(G,G.gb#i.g));
	  --<< "   tail reduced every element" << endl;
	  return false;
	  --if G.currentDegree >= 3 then return false;
	  )
     else if G.state === StateSPairs then (
	  -- grab the next spair, and deal with it.
	  if not spairStep G then (
	       G.state = StateNewPairs;
	       -- XXX: sort for auto reduction?
	       );
	  )
     else if G.state === StateNewPairs then (
	  if not newPairsStep G
	  then (
	       G.state = StateAutoReduce;
	       );
	  )
     else if G.state === StateAutoReduce then (
	  lo := G.firstInDegree;
	  hi := G.ngb-1;
	  autoReduceStep(G,lo,hi);
	  G.state = StateNewDegree;
	  );
     true
     )

grabsyz = (G) -> (
     << "." << flush;
     sps := while (
     	  sp = next(G.spairs);
	  sp =!= null) list (
	       local f;
	       if sp#0 == SPAIR then 
       	            f = applyMap(G,sp#1) -- computes s pair
     	       else
       	            f = sp#1;
	  f);
     matrix sps
     )
     
calc = method()
calc GBZZComputation := (G) -> while step(G) do ();

getmat = (G) -> (
     matrix apply(sort keys G.gb, d -> reduceTail(G,(G.gb#d).g)))
getmat = (G) -> (
     matrix apply(sort keys G.gb, d -> (G.gb#d).g))

view GBZZComputation := (G) -> (
     -- view the GB itself
     << "# elements in GB = " << G.ngb << endl;
     scan(sort keys G.gb, d -> (<< "  " << peek G.gb#d << endl;)))

///
restart
load "gbZZ.m2"
R = ZZ[a..d]
F = R^3
T = makeTermIdeal R
insert(T,15*a*b, F_0)
insert(T,10*a*c, F_1)
insert(T,6*b*c, F_2)
view T
search(T,24*b*c)
findAllMonomialDivisors(T,a*b*d)
findAllMonomialDivisors(T,a*b*c)
search(T,a*b*c)
search(T,b*c)
search(T,14*b*c)
search(T,b*d)

m = matrix{{15*a*b,10*a*c,6*b*c,5*a*d,7*a*d,a*b*c}}
T = makeTermIdeal m
view T

restart
load "gbZZ.m2"
R = ZZ[a..d]
m = matrix{{5*a*d,7*a*d}}
m = matrix{{15*a*b,10*a*c,6*b*c,5*a*d,7*a*d,a*b*c}}
m = matrix{{5*a*c-3*b*d,7*a*d}}
R = ZZ[x,y,z]
m = matrix{{3*x-4*y,7*x-12*z,41*y-12*z}}
m = matrix{{3*x^2-4*y*z,7*x*z-12*z^2,41*y^2*z-12*z^3}}
G = makeGB m

step G
view G.spairs
view G
peek G

time calc G;n
nn


<n



<<sync
s
transpose getmat G
view G
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
-- End:

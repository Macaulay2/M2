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
		    -- XXX Will FAIL
	       	    -- In this case, replace the last element...
	       	    newcoeff := ans#1;
	       	    replace(T,newcoeff,ans#2 * evec + ans#3)
	       	    )));
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
     first (degree f + degree (G.gb#x.g)))

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
	  --<< " f = " << f << endl << endl;
	  x := leadComponent f;
	  ans := search(G.searchtable#x, f);
	  --<< "ans = " << ans << endl << endl;
	  if ans === FALSE then
	      return f
	  else (
	       if ans#0 === MONOMIAL then (
		    g = applyMap(G,ans#1);;
		    --<< "    g        = " << g << endl << endl;
		    --<< "    reductor = " << ans#1 << endl << endl;
	       	    f = f - g;
		    exp := first exponents (leadTerm f)_(leadComponent f);
		    if exactMonomial(G.searchtable#x, exp)
		      then (
	         	   x = leadComponent (ans#1);
	         	   f = replaceGB(G,x,f);
			   )
		      else return f;)
	       else (
		    -- ans#0 === TERM
		    --<< "    reductxr = " << ans#1 << endl << endl;
		    f = f - applyMap(G,ans#1);
	       ));
	  );
     f
     )

insertGB = (G,f,ismin) -> (
     makeGBElement := (n,f,ismin) -> (
     	  elem := new GBElement;
     	  elem.g = f;
     	  elem.isMinimal = ismin;
	  elem.coefficient = leadCoefficient f;
	  elem.exponents = first exponents (leadTerm f)_(leadComponent f);
	  elem.gsyz = G.Gsyz_n;
	  elem);
     elem := makeGBElement(G.ngb,f,ismin);
     t := new Term from {elem.exponents, elem.coefficient, elem.gsyz};
     G.gb#(G.ngb) = elem;
     G.ngb = G.ngb + 1;
     insert(G.searchtable#(leadComponent f), t);
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
     c' := c // xy#0;
     d' := d // xy#0;
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
     f = reduce(G,f);  -- note: this may update some elements of the GB.
     if leadCoefficient f < 0 then f = -f;
     if f != 0 then insertGB(G,f,sp#0 === GENERATOR);
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

autoReduce = (G) -> (
     )

step = (G) -> (
     -- Returns what??
     if G.state === StateNewDegree then (
	  -- grab the current set of pairs
	  ans := nextDegree G.spairs;
	  if ans === null then return false;
	  G.currentDegree = ans#0;
	  << " degree " << G.currentDegree << " #pairs = " << ans#1 << endl;
	  G.state = StateSPairs;
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
	  autoReduce G;
	  G.state = StateNewDegree;
	  );
     true
     )

calc = method()
calc GBZZComputation := (G) -> (
     while step(G) do ();
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
G = makeGB m
view G
step G
view G.spairs
calc G
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
-- End:

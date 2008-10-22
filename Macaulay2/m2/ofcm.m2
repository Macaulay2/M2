--		Copyright 1993-2002 by Daniel R. Grayson

MonoidElement = new Type of HashTable
MonoidElement.synonym = "monoid element"
new MonoidElement from RawMonomial := (MonoidElement, f) -> hashTable{ symbol RawMonomial => f }
raw MonoidElement := x -> x.RawMonomial

MonoidElement == MonoidElement := (x,y) -> x === y

MonoidElement ? ZZ := (x,n) -> x ? n_(class x)
ZZ ? MonoidElement := (n,x) -> n_(class x) ? x

ZZ _ Monoid := MonoidElement => (i,M) -> (
     if i === 1 then M#1
     else error "expected integer to be 1"
     )

rawLeadMonomialR = method()
rawLeadMonomialR RingElement := RawMonomial => (f) -> rawLeadMonomial(numgens monoid ring f, raw f)

leadMonomial RingElement := RingElement => (f) -> (
     R := ring f;
     k := coefficientRing R;
     n := numgens monoid R;
     leadMonomial R := f -> new R from rawTerm(raw R, raw 1_k, rawLeadMonomial(n, raw f)); -- quicker the second time
     leadMonomial f)

makeSparse := (v) -> select(apply(#v, i -> (i,v#i)), (k,v) -> v != 0)

GeneralOrderedMonoid = new Type of OrderedMonoid
GeneralOrderedMonoid.synonym = "general ordered monoid"
GeneralOrderedMonoid.Engine = true
vars GeneralOrderedMonoid := M -> M.vars
options GeneralOrderedMonoid := M -> M.Options
degrees GeneralOrderedMonoid := M -> M.Options.Degrees
raw GeneralOrderedMonoid := M -> M.RawMonoid

monoidParts := (M) -> (
     O := monoidDefaults;
     -- o := M.Options;
     o := M#"original options";
     join(
	  if M.?generatorExpressions then M.generatorExpressions else {},
	  if any(o.Degrees, i -> i =!= {1}) then {Degrees => o.Degrees} else {},
	  if o.Heft =!= {1} then {Heft => o.Heft} else {},
	  select(
	       { MonomialOrder, MonomialSize, WeylAlgebra, SkewCommutative, Inverses }
	       / (key -> if o#key =!= O#key then key => o#key),
	       i -> i =!= null)))

expression GeneralOrderedMonoid := M -> (
     T := if (options M).Local === true then List else Array;
     new T from apply(monoidParts M,expression))
toExternalString GeneralOrderedMonoid := M -> toString expression M
toString GeneralOrderedMonoid := M -> (
     if hasAttribute(M,ReverseDictionary) then return toString getAttribute(M,ReverseDictionary);
     toExternalString M)
net GeneralOrderedMonoid := M -> (
     if hasAttribute(M,ReverseDictionary) then return toString getAttribute(M,ReverseDictionary);
     net expression M)
describe GeneralOrderedMonoid := M -> net expression M

-- this implementation is for sparse monomials, but it might
-- make sense to have a dense implementation

Monoid _ ZZ := MonoidElement => (M,i) -> M.generators#i

Monoid _ List := MonoidElement => (M,v) -> (
     if #v === 0 then 1_M
     else product(take(M.generators,#v),v,(x,i)->x^i)
     )
numgens GeneralOrderedMonoid := M -> # M.generators
degreeLength GeneralOrderedMonoid := M -> M.degreeLength

-- MES: I grabbed this from orderedmonoidrings.m2, to handle skew variables
-- in the monoid/ring.
indices := (M,vars) -> apply(vars, x -> (
	  x = baseName x;
	  if M.index#?x then M.index#x
	  else error "expected a variable of the ring"))

degreesMonoid ZZ := memoize(
     (n) -> (
	  T := getGlobalSymbol "T";
	  Zn := monoid [if n === 1 then T else T_0 .. T_(n-1),
	       Degrees => {n : {}}, 
	       MonomialOrder => RevLex,
	       Global => false,
	       Inverses=>true];
	  Zn))

monoidDefaults = (
     new OptionTable from {
	  Variables => null,
	  VariableBaseName => getGlobalSymbol "p",	    -- would be overridden by Variables => {...}
	  Weights => {},				    -- default weight is 1, unless Local=>true
	  Global => true,				    -- means that all variables are > 1
     	  Local => false, 				    -- means that all variables are < 1, default weight = -1, and implies Global => false
	  Degrees => null,
	  Inverses => false,
	  MonomialOrder => {GRevLex, Position => Up},
	  MonomialSize => 32,				    -- we had this set to null, but some of the code needs a number here...
	  SkewCommutative => {},
	  -- VariableOrder => null,		  -- not implemented yet
	  WeylAlgebra => {},
     	  Heft => null,
	  DegreeRank => null,				    -- specifying DegreeRank=>3 and no Degrees means degrees {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {0, 0, 1}, ...}
	  Join => true,					    -- whether the degrees in the new monoid ring will be obtained by joining the degrees in the coefficient with the degrees in the monoid
      	  DegreeMap => identity,			    -- the degree map to use, if Join=>false is specified, for converting degrees in the coefficient ring to degrees in the monoid
     	  ConstantCoefficients => true			    -- whether to set the coefficient variables to degree 0 in the new polynomial ring
	  }
     )

tensorDefaults = merge(monoidDefaults, 
     new OptionTable from {
	  MonomialOrder => null,
	  VariableBaseName => null			    -- monoids being tensored already have variable names
	  },
     (x,y) -> y)

monoid = method(Dispatch => Thing, Options => monoidDefaults, TypicalValue => GeneralOrderedMonoid)
monoid PolynomialRing := o -> R -> R.monoid
options PolynomialRing := options @@ monoid

generators GeneralOrderedMonoid := opts -> M -> M.generators
vars GeneralOrderedMonoid := M -> M.generators
degreesMonoid GeneralOrderedMonoid := Monoid => M -> degreesMonoid degreeLength M

standardForm(MonoidElement) := (m) -> new HashTable from rawSparseListFormMonomial raw m
exponents(MonoidElement) :=
listForm(MonoidElement) := (m) -> (
     x := numgens class m : 0;
     x = new MutableList from x;
     scan(rawSparseListFormMonomial raw m, (i,e) -> x#i = e);
     toList x)

MonoidElement _ GeneralOrderedMonoid := MonoidElement => (x,M) -> (baseName x)_M

Symbol _ GeneralOrderedMonoid := MonoidElement => (x,M) -> (
     if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
     else error "symbol not found in monoid"
     )
IndexedVariable _ GeneralOrderedMonoid := MonoidElement => (x,M) -> (
     if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
     else error "indexed variable not found in monoid"
     )

Symbol _ Ring := RingElement => (x,M) -> (
     if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
     else error "symbol not found in ring"
     )
IndexedVariable _ Ring := RingElement => (x,M) -> (
     if M.?indexSymbols and M.indexSymbols#?x then M.indexSymbols#x
     else error "indexed variable not found in ring"
     )

Number _ Ring := promote

RingElement _ Ring := (x,R) -> (
     try promote(x,R)
     else try (baseName x)_R
     )

madeTrivialMonoid := false

dotprod = (c,d) -> sum( min(#c, #d), i -> c#i * d#i )

makeit1 := (opts) -> (
     M := new GeneralOrderedMonoid of MonoidElement;
     M#"original options" = opts;
     M.Engine = true;
     varlist := baseName \ opts.Variables;
     numvars := # varlist;
     vardegs := M.degrees = opts.Degrees;
     degrk := M.degreeLength = opts.DegreeRank;
     assert( degrk =!= null );
     order := transpose vardegs;
     variableOrder := toList (0 .. numvars-1);
     M.generatorSymbols = varlist;
     M.generatorExpressions = apply(varlist,
	  x -> if instance(x, Symbol) then x else expression x
	  );
     scan(varlist, sym -> if not (instance(sym,Symbol) or null =!= lookup(symbol <-, class sym)) then error "expected variable or symbol");
     M.standardForm = somethingElse;
     expression M := x -> new Product from apply( 
	  rawSparseListFormMonomial x.RawMonomial,
	  (k,v) -> if v =!= 1 then Power{M.generatorExpressions#k, v} else M.generatorExpressions#k );
     w := reverse applyTable(order, minus);
     w = if # w === 0 then apply(numvars,i -> {}) else transpose w;
     w = apply(w, x -> apply(makeSparse x, (k,v) -> (k + numvars, v)));
     if #w =!= #varlist then error "expected same number of degrees as variables";
     M.vars = M.generators = apply(# varlist, i -> new M from rawVarMonomial(i,1));
     M.indexSymbols = hashTable apply(M.generatorSymbols,M.generators,(v,x) -> v => x);
     M.index = new MutableHashTable;
     scan(#varlist, i -> M.index#(varlist#i) = i);
     (MOopts,MOoptsint,rawMO,logMO) := makeMonomialOrdering (
     	  opts.MonomialSize,
	  opts.Inverses,
     	  #varlist,
	  if degreeLength M > 0 and opts.Heft =!= null then (
	       apply(vardegs,d -> (
			 w := dotprod(d,opts.Heft);
			 if w > 0 then w else 1
			 )))
	  else toList (#vardegs:1),
	  opts.Weights,
	  opts.MonomialOrder
	  );
     M.RawMonomialOrdering = rawMO;
     M#"raw creation log" = new Bag from {logMO};
     opts = new MutableHashTable from opts;
     opts.MonomialOrder = MOoptsint; -- these are the options given to rawMonomialOrdering, except Tiny and Small aren't there yet...
     M#"options" = MOopts; -- these are exactly the arguments given to rawMonomialOrdering
     remove(opts, MonomialSize);
     M.Options = new OptionTable from opts;
     toString M := toExternalString M := x -> toString expression x;
     M.RawMonoid = (
	  if numvars == 0 and not madeTrivialMonoid then (
	       madeTrivialMonoid = true;
	       rawMonoid())
	  else rawMonoid(
	       M.RawMonomialOrdering,
	       toSequence M.generators / toString,
	       raw degreesRing degrk,
	       flatten vardegs,
	       if opts.Heft === null then toList(degrk:0) else flatten opts.Heft));
     raw M := x -> x.RawMonomial;
     net M := x -> net expression x;
     M ? M := (x,y) -> rawCompareMonomial(raw M, raw x, raw y);
     M Array := (m,x) -> (
	  if # x != numvars then error (
	       "expected a list of length ", toString numvars
	       );
	  x = flatten toList x;
	  product(m, (k,v) -> if k < numvars then x#k^v else 1)
	  );
     M == ZZ := (m,i) -> (
	  if i === 1
	  then m == 1_M
	  else error "no method for '=='"
	  );
     ZZ == M := (i,m) -> m == i;
     M * ZZ := (x,i) -> (
	  if i === 1
	  then x
	  else error "no method for multiplying available"
	  );
     ZZ * M := (i,x) -> (
	  if i === 1
	  then x
	  else error "no method for multiplying available"
	  );
     M * M := (x,y) -> new M from x.RawMonomial * y.RawMonomial;
     M#1 = new M from rawMakeMonomial{};
     degree M := x -> notImplemented();
     baseName M := x -> (
	  s := rawSparseListFormMonomial raw x;
	  if #s == 1 and s#0#1 == 1 then M.generatorSymbols#(s#0#0) else error "expected a generator"
	  );
     M / M := (x,y) -> new M from somethingElse();		    -- there will be no remainder, and it will depend on the monoid, too!
     M : M := (x,y) -> new M from x.RawMonomial : y.RawMonomial;
     M ^ ZZ := (x,n) -> new M from x.RawMonomial ^ n;
     M.use = x -> scan(M.generatorSymbols,M.vars,(sym,val) -> sym <- val);
     if opts.Global and not opts.Inverses then scan(M.generators, x -> if x <= 1 then error "not all variables are > 1, and Global => true");
     M)

processDegrees := (degs,degrk,nvars) -> (
     degs = splice degs;
     if degs === null then degs = (
	  if degrk === null then (
	       degrk = 1;
	       apply(nvars,i->{1})
	       )
	  else apply(nvars, i -> apply(degrk, j -> if j === i or i >= degrk and j === degrk-1  then 1 else 0))
	  )
     else (
     	  degs = apply(degs, d -> if class d === ZZ then {d} else d);
     	  scan(degs, d -> if not (class d === List and all(d, i -> class i === ZZ)) then error "expected degree to be an integer or list of integers");
     	  if degrk === null then (
	       if not same(length \ degs) then error "expected degrees all of the same rank";
 	       degrk = if #degs > 0 then #degs#0 else 0;
	       )
	  else scan(degs, d -> if #d =!= degrk then error("expected degree of rank ",degrk));
	  );
     if nvars != #degs then error "expected length of list of degrees to equal the number of variables";
     (degs,degrk));

monoidIndex = (M,x) -> if class x === ZZ then x else (
	       x = baseName x;
	       if M.index#?x then M.index#x
	       else error("expected an integer or variable of the ring (or monoid): ", toString x))

monoidIndices = (M,varlist) -> (				    -- also used in orderedmonoidrings.m2, but we should phase that out
     apply(varlist, x -> monoidIndex(M,x))
     )

findHeft = (degrk,degs) -> (
     -- this function adapted from one written by Greg Smith; it appears in the FourierMotzkin package documentation
     -- we return the zero vector if no heft vector exists
     if degrk === 0 then return {};
     if degrk === 1 then return (
	  if all(degs,d->d#0 > 0) then {1}
	  else if all(degs,d->d#0 < 0) then {-1}
	  else {0}
	  );
     if (#degs === 0) then return toList(degrk : 0);
     A := transpose matrix degs;
     B := ((value getGlobalSymbol "fourierMotzkin") A)#0;
     r := rank source B;
     heft := first entries (matrix{toList(r:-1)} * transpose B);
     g := gcd heft;
     if g > 1 then heft = apply(heft, h -> h // g);
     heft);

makeMonoid := (opts) -> (
     -- check the options for consistency, and set everything to the correct defaults
     opts = new MutableHashTable from opts;

     if opts.Local === true then opts.Global = false;

     if class opts.Inverses =!= Boolean then error "expected true or false in option";
     
     if opts.SkewCommutative =!= {} and opts.Inverses then error "skew commutative ring with inverses requested";

     -- First check the variable names
     if class opts.Variables === ZZ 
     then (
	  x := baseName opts.VariableBaseName;
          opts.Variables = toList (x_0 .. x_(opts.Variables - 1)))
     else (
	  v := flatten toList apply(opts.Variables, x->if class x === MutableList then toList x else x);
          opts.Variables = apply(#v, 
	       i -> (
		    try baseName v#i
		    else (
			 msg := concatenate("encountered object not usable as variable at position ",toString i," in list:");
			 preX := "        ";
			 pw := max(printWidth,80);
			 error (msg,newline,toString (preX | silentRobustNetWithClass(pw - width  preX, 5, 3, v#i)))))));
     -- if length unique opts.Variables < length opts.Variables then error "at least one variable listed twice";

     (degs,degrk) := processDegrees( opts.Degrees, opts.DegreeRank, length opts.Variables );
     opts.Degrees = degs;
     opts.DegreeRank = degrk;

     if opts.Local === true and opts.Weights === {} then opts.Weights = toList ( #opts.Variables : -1 );

     num := # opts.Variables;

     skews := opts.SkewCommutative;
     opts.SkewCommutative = (
	  if skews === false then {}
	  else if skews === true then toList (0 .. num - 1)
     	  else (
	       if not instance(skews, List) then error "expected SkewCommutative option to a true, false, or a list";
	       apply(skews, i -> (
			 if instance(i,ZZ) or instance(i,Symbol) or instance(i,IndexedVariable) then i
			 else try baseName i else error("SkewCommutative option: expected base name for: ",toString i)
			 ))));

     heft := opts.Heft;
     if heft === null then (
	  heft = findHeft(degrk,degs);
	  )
     else (
	  if not instance(heft,List) or not all(heft,i -> instance(i,ZZ)) then error "expected Heft option to be a list of integers";
	  if #heft > degrk then error("expected Heft option to be of length at most the degree rank (", degrk, ")");
	  if #heft < degrk then heft = join(heft, degrk - #heft : 0);
	  );
     if heft =!= null then scan(degs, d -> if not sum apply(take(d,#heft), take(heft,#d), times) > 0 then (heft = null; break; ));
     opts.Heft = heft;
     opts = new OptionTable from opts;
     makeit1 opts)

monoid List := opts -> args -> monoid (new Array from args, opts, Local => true)
monoid Array := opts -> args -> (
     (opts,args) = override(opts,toSequence args);
     if opts.Variables === null
     then opts = merge(opts, new OptionTable from {Variables => deepSplice sequence args}, last)
     else if args =!= () then error "variables provided conflict with Variables option";
     makeMonoid opts)

tensor = method( Options => tensorDefaults)

Monoid ** Monoid := Monoid => (M,N) -> tensor(M,N)

tensoradj := (f,g,m,n) -> (
     if f === identity then (
	  if g === identity 
	  then identity
     	  else x -> join(take(x,m), g take(x,-n))
	  )
     else (
	  if g === identity 
	  then x -> join(f take(x,m), take(x,-n))
     	  else x -> join(f take(x,m), g take(x,-n))
	  ))

trimMO := o -> (
     numseen := 0;
     select(o, x -> not instance(x,Option) or x#0 =!= Position or 1 == (numseen = numseen + 1)))

tensor(Monoid, Monoid) := Monoid => options -> (M,N) -> (
     Mopts := M.Options;
     Nopts := N.Options;
     opts := new MutableHashTable from options;
     if opts.Variables === null 
     then opts.Variables = join(Mopts.Variables, Nopts.Variables)
     else opts.Variables = splice opts.Variables;
     if opts.VariableBaseName =!= null then (
	  x := opts.VariableBaseName;
	  opts.Variables = apply(#opts.Variables, i -> x_i);
	  );
     if opts.MonomialOrder === null 
     then opts.MonomialOrder = trimMO join(Mopts.MonomialOrder,Nopts.MonomialOrder); -- product order
     processDegrees(opts.Degrees, null, length opts.Variables);	-- just for the error messages
     if opts.Degrees === null and opts.DegreeRank === null then (
	  M0 := apply(Mopts.DegreeRank, i -> 0);
	  N0 := apply(Nopts.DegreeRank, i -> 0);
          opts.Degrees = join(
	       apply(Mopts.Degrees, d -> join(d,N0)),
	       apply(Nopts.Degrees, e -> join(M0,e))
	       );
	  if opts.Heft === null then (
	       assert( # Mopts.Heft == Mopts.DegreeRank );
	       assert( # Nopts.Heft == Nopts.DegreeRank );
	       opts.Heft = join(Mopts.Heft,Nopts.Heft);
	       );
	  );
     opts.Inverses = if opts.Inverses === null then Mopts.Inverses or Nopts.Inverses else opts.Inverses;
     wfix := (M,w,bump) -> (
	  if class w === Option then w = {w};
	  apply(w, o -> monoidIndex(M,o#0) + bump => monoidIndex(M,o#1) + bump));
     opts.WeylAlgebra = join(wfix(M, Mopts.WeylAlgebra, 0), wfix(N, Nopts.WeylAlgebra, numgens M));
     if Mopts.Global === false or Nopts.Global === false then opts.Global = false;
     oddp := x -> x#?0 and odd x#0;
     m := numgens M;
     opts.SkewCommutative = join(monoidIndices(M,M.Options.SkewCommutative), apply(monoidIndices(N,N.Options.SkewCommutative), i -> i+m));
     makeMonoid new OptionTable from opts)

-- delayed installation of methods for monoid elements

promote(MonoidElement, RingElement) := RingElement => (m,R) -> (
     M := monoid R;
     k := coefficientRing R;
     if not instance(m,M) then error "expected monomial from same ring";
     new R from rawTerm(R.RawRing, raw 1_k, m.RawMonomial))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

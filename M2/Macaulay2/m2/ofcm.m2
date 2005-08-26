--		Copyright 1993-2002 by Daniel R. Grayson

MonoidElement = new Type of HashTable
MonoidElement.synonym = "monoid element"
new MonoidElement from RawMonomial := (MonoidElement, f) -> hashTable{ symbol RawMonomial => f }
raw MonoidElement := x -> x.RawMonomial

MonoidElement == MonoidElement := (x,y) -> x === y

ZZ _ Monoid := MonoidElement => (i,M) -> (
     if i === 1 then M#1
     else error "expected integer to be 1"
     )

leadMonomial RingElement := MonoidElement => (f) -> (
     R := ring f;
     M := monoid R;
     n := numgens M;
     leadMonomial R := f -> new M from rawLeadMonomial(n, f.RawRingElement); -- the default method for most engine rings, but not Schur rings
     leadMonomial f)

makeSparse := (v) -> select(apply(#v, i -> (i,v#i)), (k,v) -> v != 0)

GeneralOrderedMonoid = new Type of OrderedMonoid
GeneralOrderedMonoid.synonym = "general ordered monoid"
GeneralOrderedMonoid.Engine = true
vars GeneralOrderedMonoid := M -> M.vars
options GeneralOrderedMonoid := M -> M.Options
raw GeneralOrderedMonoid := M -> M.RawMonoid

parts := (M) -> (
     O := monoidDefaults;
     -- o := M.Options;
     o := M#"original options";
     join(
	  if M.?generatorExpressions then M.generatorExpressions else {},
	  if any(o.Degrees, i -> i =!= {1}) then {Degrees => o.Degrees} else {},
	  select(
	       { MonomialOrder, MonomialSize, WeylAlgebra, SkewCommutative, Inverses }
	       / (key -> if o#key =!= O#key then key => o#key),
	       i -> i =!= null)))

expression GeneralOrderedMonoid := M -> new Array from apply(parts M,expression)
toExternalString GeneralOrderedMonoid := M -> toString expression M
toString GeneralOrderedMonoid := M -> (
     if ReverseDictionary#?M then return toString ReverseDictionary#M;
     toExternalString M)
net GeneralOrderedMonoid := M -> (
     if ReverseDictionary#?M then return toString ReverseDictionary#M;
     net expression M)

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
	  T := global T;
	  Zn := monoid [if n === 1 then T else T_0 .. T_(n-1),
	       Degrees => {}, 
	       MonomialOrder => RevLex,
	       Inverses=>true];
	  PrintNames#Zn = "ZZ^" | toString n;
	  Zn))

monoidDefaults = (
     new OptionTable from {
	  VariableBaseName => null,
	  Variables => null,
	  Degrees => null,
	  Weights => {},
	  Inverses => false,
	  MonomialOrder => GRevLex,
	  MonomialSize => 32,				    -- we had this set to null, but some of the code needs a number here...
	  SkewCommutative => false,
	  VariableOrder => null,		  -- not implemented yet
	  WeylAlgebra => {},
	  Adjust => identity,
	  Repair => identity,
     	  Heft => null,
	  DegreeRank => null
	  }
     )

tensorDefaults = merge(monoidDefaults, 
     new OptionTable from {MonomialOrder => null},
     (x,y) -> y)

monoid = method(SingleArgumentDispatch => true)
options PolynomialRing := options @@ monoid

generators GeneralOrderedMonoid := M -> M.generators
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
     if M.?generatorsTable and M.generatorsTable#?x then M.generatorsTable#x
     else error "symbol not found in monoid"
     )
IndexedVariable _ GeneralOrderedMonoid := MonoidElement => (x,M) -> (
     if M.?generatorsTable and M.generatorsTable#?x then M.generatorsTable#x
     else error "indexed variable not found in monoid"
     )

Symbol _ Ring := RingElement => (x,M) -> (
     if M.?generatorsTable and M.generatorsTable#?x then M.generatorsTable#x
     else error "symbol not found in ring"
     )
IndexedVariable _ Ring := RingElement => (x,M) -> (
     if M.?generatorsTable and M.generatorsTable#?x then M.generatorsTable#x
     else error "indexed variable not found in ring"
     )

QQ _ Ring :=
RingElement _ Ring := RingElement => (x,R) -> (
     if ring x === R then x
     else try x * 1_R
     else try (baseName x)_R
     ---- we might enable the line below if map(R,S) would insist on finding every variable
     -- else try (map(R,ring x)) x
     else error "failed to interpret ring element in ring"
     )

makeit1 := (opts) -> (
     M := new GeneralOrderedMonoid of MonoidElement;
     M#"original options" = opts;
     M.Engine = true;
     varlist := baseName \ opts.Variables;
     n := # varlist;
     externalDegrees := opts.Degrees;
     M.degrees = externalDegrees;
     M.degreeLength = if externalDegrees#?0 then # externalDegrees#0 else 0;
     internalDegrees := apply(externalDegrees,opts.Adjust);
     order := transpose internalDegrees;
--     primaryDegrees := if #order === 0 then toList(n:1) else order#0;
--     if not all(primaryDegrees, d -> d>0) then (
--	  if all(primaryDegrees, d -> d<0) then (
--	       -- I wonder whether this attempt at an easy adjustment was ever used
--	       adjust := if opts.Adjust === identity then minus else minus @@ opts.Adjust;
--	       repair := if opts.Repair === identity then minus else minus @@ opts.Repair;
--	       opts = new OptionTable from join ( pairs opts, {Adjust => adjust, Repair => repair} );
--	       internalDegrees = apply(externalDegrees,opts.Adjust);
--	       order = transpose internalDegrees;
--	       primaryDegrees = order#0;
--	       )
--	  else error "first component of each degree should be positive" -- this error message will go away
--	  );
     variableOrder := toList (0 .. n-1);
     wts := splice flatten opts.Weights;
     if not all(wts,i -> class i === ZZ)
     then error "expected Weights option to be a list or list of lists of integers";
     if n != 0 and #wts % n != 0 or n == 0 and #wts != 0
     then error "expected Weights option length to be a multiple of the number of variables";
     M.generatorSymbols = varlist;
     M.generatorExpressions = apply(varlist,
	  x -> if instance(x, Symbol) then x else expression x
	  );
     scan(varlist, 
	  sym -> (
	       if Symbol =!= basictype sym 
	       and IndexedVariable =!= class sym
	       then error "expected variable or symbol"));
     M.standardForm = somethingElse;
     expression M := x -> new Product from apply( 
	  rawSparseListFormMonomial x.RawMonomial,
	  (k,v) -> Power{M.generatorExpressions#k, v} );
     w := reverse applyTable(order, minus);
     w = if # w === 0 then apply(n,i -> {}) else transpose w;
     w = apply(w, x -> apply(makeSparse x, (k,v) -> (k + n, v)));
     if #w =!= #varlist then error "expected same number of degrees as variables";
     M.vars = M.generators = apply(# varlist, i -> new M from rawVarMonomial(i,1));
     M.generatorsTable = hashTable apply(M.generatorSymbols,M.generators,(v,x) -> v => x);
     M.index = new MutableHashTable;
     scan(#varlist, i -> M.index#(varlist#i) = i);
     M.internalDegrees = internalDegrees;
     M.internalDegreeLength = internalDegreeLength := if internalDegrees#?0 then #internalDegrees#0 else 0;
     (MOopts,rawMO) := makeMonomialOrdering(
     	  opts.MonomialSize,
	  opts.Inverses,
     	  #varlist,
	  if degreeLength M > 0 then internalDegrees/first else {},
	  wts,
	  opts.MonomialOrder
	  );
     M.RawMonomialOrdering = rawMO;
     opts = new MutableHashTable from opts;
     opts.MonomialOrder = MOopts;
     M.Options = new OptionTable from opts;
     toString M := toExternalString M := x -> toString expression x;
     M.RawMonoid = (
	  if n == 0 
	  then rawMonoid()
	  else rawMonoid(
	       M.RawMonomialOrdering,
	       toSequence M.generators / toString,
	       raw degreesRing internalDegreeLength,
	       flatten internalDegrees));
     raw M := x -> x.RawMonomial;
     net M := x -> net expression x;
     M ? M := (x,y) -> rawCompareMonomial(raw M, raw x, raw y);
     M Array := (m,x) -> (
	  if # x != n then error (
	       "expected a list of length ", toString n
	       );
	  x = flatten toList x;
	  product(m, (k,v) -> if k < n then x#k^v else 1)
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
     (degs,degrk));

makeMonoid := (options) -> (
     -- check the options for consistency, and set everything to the correct defaults
     options = new MutableHashTable from options;

     if class options.Inverses =!= Boolean then error "expected true or false in option";
     
     if options.SkewCommutative =!= false and options.Inverses then error "skew commutative ring with inverses requested";

     -- First check the variable names
     if class options.Variables === ZZ 
     then (
	  x := local "$x";
          options.Variables = toList (x_0 .. x_(options.Variables - 1)))
     else (
	  v := flatten toList apply(options.Variables, x->if class x === MutableList then toList x else x);
          options.Variables = apply(#v, 
	       i -> (
		    try baseName v#i
		    else (
			 msg := concatenate("encountered object not usable as variable at position ",toString i," in list:");
			 preX := "        ";
			 pw := max(printWidth,80) - promptWidth();
			 error (msg,newline,toString (preX | silentRobustNetWithClass(pw - width  preX, 5, 3, v#i)))))));
     -- if length unique options.Variables < length options.Variables then error "at least one variable listed twice";

     (degs,degrk) := processDegrees(options.Degrees,options.DegreeRank,length options.Variables);
     options.Degrees = degs;
     options.DegreeRank = degrk;

     if class options.Adjust =!= Function then error("expected 'Adjust' option to be a function");
     if class options.Repair =!= Function then error("expected 'Repair' option to be a function");
     heft := options.Heft;
     if heft =!= null then (
     	  if options.Adjust =!= identity or options.Repair =!= identity then error "encountered both Heft and Adjust or Repair options";
	  options.Adjust = x -> prepend(sum(heft,x,times),x); -- to internal degree
	  options.Repair = x -> drop(x,1);		      -- to external degree
	  scan(degs, d -> (
		    if degrk =!= length heft then error ("expect Heft option to be a list of length ",toString degrk, " to match the degree rank");
		    if not first options.Adjust d > 0 then error "Heft option doesn't yield a positive value for each variable";
		    )));

     options = new OptionTable from options;
     makeit1 options)

monoid Array := Monoid => (
     monoidDefaults >> options -> args -> (
     	  if options.Variables === null
     	  then options = merge(options, new OptionTable from {Variables => deepSplice sequence args}, last);
     	  makeMonoid options)
     ) @@ toSequence

monoid Ring := Monoid => R -> R.monoid

tensor = method( Options => tensorDefaults)

Monoid ** Monoid := Monoid => (M,N) -> tensor(M,N)

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
     then opts.MonomialOrder = join(Mopts.MonomialOrder,Nopts.MonomialOrder); -- product order
     processDegrees(opts.Degrees, Mopts.DegreeRank, length opts.Variables);	-- just for the error messages
     if opts.Degrees === null then (
	  nil := apply(Mopts.DegreeRank, i -> 0);
          opts.Degrees = join(Mopts.Degrees, apply(Nopts.Degrees, v -> nil)));
     opts.Inverses = if opts.Inverses === null then Mopts.Inverses or Nopts.Inverses else opts.Inverses;
     opts.WeylAlgebra = join(Mopts.WeylAlgebra, Nopts.WeylAlgebra);
     skews := Mopts -> (
	  sk := Mopts.Options.SkewCommutative;
	  if sk === true then sk = generators Mopts;
	  if sk === false then sk = {};
	  sk);
     opts.SkewCommutative = join(skews M, skews N);
     makeMonoid new OptionTable from opts)

-- delayed installation of methods for monoid elements

promote(MonoidElement, Ring) := RingElement => (m,R) -> promote(m,R#0)
promote(MonoidElement, RingElement) := RingElement => (m,o) -> (
     R := class o;
     M := monoid R;
     k := coefficientRing R;
     if M =!= class m then error "expected monomial from same ring";
     one := 1_k;
     promote(M,R) := (m,o) -> new R from rawTerm(R.RawRing, 
	                                         raw one,
						 m.RawMonomial);
     promote(m,o))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

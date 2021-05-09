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

rle = method(Dispatch => Thing)
rle VisibleList := x -> apply(runLengthEncode x, y -> if instance(y,Holder) then rle y#0 else y)
rle Option := x -> x#0 => rle x#1
rle Thing := identity

fixbasename = s -> if instance(s,String) then getSymbol s else s

monoidDefaults = (
     new OptionTable from {
	  Variables => null,
	  VariableBaseName => "p",     	    	 	    -- would be overridden by Variables => {...}
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
     	  Heft => null -* find one *-,
	  DegreeRank => null,				    -- specifying DegreeRank=>3 and no Degrees means degrees {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {0, 0, 1}, ...}
	  Join => null -* true *-,			    -- whether the degrees in the new monoid ring will be obtained by joining the degrees in the coefficient with the degrees in the monoid
      	  DegreeMap => null -* identity *-,		    -- the degree map to use, if Join=>false is specified, for converting degrees in the coefficient ring to degrees in the monoid
     	  DegreeLift => null,				    -- a function for lifting degrees from the monoid ring to the coefficient ring.  Length must be correct.  Gives an error if lifting is not possible.
	  Constants => false				    -- whether to use rawTowerRing when making a monoid ring
	  }
     )

monoidParts = (M) -> (
     O := monoidDefaults;
     o := M#"original options";	-- if we used M.Options we'd run into lots of long lists as in GRevLex => {1,1,1,1,1,1,1}
     o = M.Options;
     nonnull splice (
	  if M.?generatorExpressions then toSequence runLengthEncode M.generatorExpressions,
	  Degrees => runLengthEncode if o.DegreeRank === 1 then flatten o.Degrees else (x -> VerticalList x) \ o.Degrees,
	  if o.Heft =!= null then Heft => runLengthEncode o.Heft,
	  MonomialOrder => rle o.MonomialOrder,
	  ( DegreeRank, MonomialSize, WeylAlgebra, SkewCommutative, Inverses, Global ) / (key -> if o#?key and o#key =!= O#key then key => o#key)))

expressionMonoid = M -> (
     T := if (options M).Local === true then List else Array;
     new T from apply(monoidParts M,expression))
expression GeneralOrderedMonoid := M -> if hasAttribute(M,ReverseDictionary) then expression getAttribute(M,ReverseDictionary) else new Parenthesize from { (expression monoid) expressionMonoid M }
describe GeneralOrderedMonoid := M -> Describe new Parenthesize from { (expression monoid) expressionMonoid M }

toExternalString GeneralOrderedMonoid := toString @@ describe
toString GeneralOrderedMonoid := toString @@ expression
net GeneralOrderedMonoid := net @@ expression
texMath GeneralOrderedMonoid := x -> texMath expression x

degreesMonoid = method(TypicalValue => GeneralOrderedMonoid)
degreesMonoid PolynomialRing := R -> (
     if R.?degreesMonoid then R.degreesMonoid
     else error "no degreesMonoid for this ring")
degreesMonoid Ring := R -> error "no degreesMonoid for this ring"

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
     n -> (
	  T := getSymbol "T";
	  monoid [
	       if n === 1 then T else T_0 .. T_(n-1),
	       Degrees => {n : {}}, 
	       MonomialOrder => {Weights => {n:-1}, GroupLex => n},
	       Global => false,
	       Inverses=>true]))

degreesMonoid List := memoize(
     hft -> (
	  hft = deepSplice hft;
	  if not all(hft, i -> instance(i,ZZ)) then error "degreesMonoid: expected a list of integers";
	  n := # hft;
	  T := getSymbol "T";
	  monoid [
	       if n === 1 then T else T_0 .. T_(n-1),
	       Degrees => hft,
	       MonomialOrder => {Weights => -hft, GroupLex => n},
	       Global => false,
	       Inverses => true]))

tensorDefaults = merge(monoidDefaults, 
     new OptionTable from {
	  MonomialOrder => null,
	  VariableBaseName => null,			    -- monoids being tensored already have variable names
	  Inverses => null				    -- so we can detect a mixture and give an error
	  },
     (x,y) -> y)

monoid = method(Dispatch => Thing, Options => monoidDefaults, TypicalValue => GeneralOrderedMonoid)
monoid PolynomialRing := o -> R -> R.monoid
options PolynomialRing := options @@ monoid

generators GeneralOrderedMonoid := opts -> M -> M.generators
vars GeneralOrderedMonoid := M -> M.generators
degreesMonoid GeneralOrderedMonoid := GeneralOrderedMonoid => M -> if M.?degreesMonoid then M.degreesMonoid else error "no degrees monoid present"
degreesRing GeneralOrderedMonoid := PolynomialRing => M -> if M.?degreesRing then M.degreesRing else error "no degrees ring present"

GeneralOrderedMonoid_* := M -> generators M

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
RingElement ^ Ring := Number ^ Ring := (x,R) -> lift(x,R)
RingElement ^ RingFamily := Number ^ RingFamily := (x,R) -> lift(x, default R)
Constant ^ Ring := Constant ^ RingFamily := (x,R) -> lift(x,R)

RingElement _ Ring := promote

madeTrivialMonoid := false

dotprod = (c,d) -> sum( min(#c, #d), i -> c#i * d#i )

fix := s -> if instance(s,ZZ) then s else baseName s;
fixWA := diffs -> (
     if class diffs =!= List then diffs = {diffs};
     apply(diffs, x -> 
	  if instance(x,List) then (
	       if #x =!= 2 then error "WeylAlgebra option member: expected {x,dx}";
	       fix \ x)
	  else if instance(x,Option) then (
	       if #x =!= 2 then error "WeylAlgebra option member: expected x=>dx";
	       fix x#0 => fix x#1)
	  else fix x))

findSymbols = varlist -> (
    -- varlist is a list or sequence of items we wish to use for variable names.
    -- these may be: Symbol's, RingElement's (which are variables in a ring)
    -- or lists or sequences of such.
    -- Return value: a List of Symbol's and IndexVariable's (or an error message gets issued)
    varlist = deepSplice sequence varlist;
    v := flatten toList apply(varlist, x->if class x === MutableList then toList x else x);
    genList := for i from 0 to #v-1 list (
            try baseName v#i
            else if instance(v#i,String) and match("[[:alnum:]$]+",v#i) then getSymbol v#i
            else (
                msg := concatenate("encountered object not usable as variable at position ",toString i," in list:");
                preX := "        ";
                pw := max(printWidth,80);
                error (msg,newline,toString (preX | silentRobustNetWithClass(pw - width  preX, 5, 3, v#i)));
                )
            );
     -- is this next line needed?
     -- what is the purpose of this line?
     scan(genList, sym -> if not (instance(sym,Symbol) or null =!= lookup(symbol <-, class sym)) then error "expected variable or symbol");
     genList
    )

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
     scan(varlist, sym -> if not (instance(sym,Symbol) or null =!= lookup(symbol <-, class sym)) then error "expected variable or symbol");
     M.standardForm = somethingElse;
     w := reverse applyTable(order, minus);
     w = if # w === 0 then apply(numvars,i -> {}) else transpose w;
     w = apply(w, x -> apply(makeSparse x, (k,v) -> (k + numvars, v)));
     if #w =!= #varlist then error "expected same number of degrees as variables";
     M.vars = M.generators = apply(# varlist, i -> new M from rawVarMonomial(i,1));
     M.generatorSymbols = varlist;
     M.generatorExpressions = (
	  apply(varlist,
	       x -> if instance(x, Symbol) then x else expression x
	       )
	  -- apply(varlist,M.generators,(e,x) -> new Holder2 from {expression e,x})
	  );
     processTrm := (k,v) -> if v =!= 1 then Power{M.generatorExpressions#k, v} else M.generatorExpressions#k;
     processTrms := trms -> (
	  if # trms === 1
	  then processTrm trms#0
	  else new Product from apply(trms, processTrm));
     expression M := x -> (
	  hold processTrms rawSparseListFormMonomial x.RawMonomial -- hold needed if single variable
	  -- new Holder2 from { processTrms rawSparseListFormMonomial x.RawMonomial, x }
	  );
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
     opts.WeylAlgebra = fixWA opts.WeylAlgebra;
     opts.MonomialOrder = new VerticalList from MOoptsint; -- these are the options given to rawMonomialOrdering, except Tiny and Small aren't there yet and GRevLex=>n hasn't been expanded
     M#"options" = MOopts; -- these are exactly the arguments given to rawMonomialOrdering
     remove(opts, MonomialSize);
     remove(opts, Weights);
     remove(opts, VariableBaseName);
     M.Options = new OptionTable from opts;
     toString M := toExternalString M := x -> toString expression x;
     texMath M := x -> texMath expression x;
     if numvars == 0 and not madeTrivialMonoid then (
	  madeTrivialMonoid = true;
	  M.RawMonoid = rawMonoid();
	  )
     else (
     	  M.degreesRing = (
	       if opts.Heft =!= null 
	       then degreesRing opts.Heft 
	       else degreesRing degrk -* shouldn't really be needed *-
	       );
     	  M.degreesMonoid = monoid M.degreesRing;
	  M.RawMonoid = rawMonoid(
	       M.RawMonomialOrdering,
	       toSequence M.generators / toString,
	       raw M.degreesRing,
	       flatten vardegs,
	       if opts.Heft === null then toList(degrk:0) else flatten opts.Heft);
	  );
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

processDegrees = (degs,degrk,nvars) -> (
     if not (degrk === null or instance(degrk,ZZ)) then error("DegreeRank => ... : expected an integer or null");
     if degs === null then degs = (
	  if degrk === null then (
	       degrk = 1;
	       apply(nvars,i->{1})
	       )
	  else apply(nvars, i -> apply(degrk, j -> if j === i or i >= degrk and j === degrk-1  then 1 else 0))
	  )
     else (
     	  if not instance(degs,List) then error "Degrees: expected a list";
     	  degs = apply(spliceInside degs, d -> if class d === ZZ then {d} else spliceInside d);
     	  scan(degs, d -> if not (instance(d,List) and all(d, i -> instance(i,ZZ))) then error "expected degree to be an integer or list of integers");
     	  if degrk === null then (
	       if not same(length \ degs) then error "expected degrees all of the same rank";
 	       degrk = if #degs > 0 then #degs#0 else 1;
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

chkHeft = (degs,heft) -> all(degs, d -> sum apply(d,heft,times) > 0)

findHeft = method(Options => { DegreeRank => null }, TypicalValue => List )
findHeft List := opts -> (degs) -> (
     -- this function is adapted from one written by Greg Smith; it appears in the FourierMotzkin package documentation
     -- we return null if no heft vector exists
     degrk := opts.DegreeRank;
     if not isListOfListsOfIntegers degs then error "expected a list of degrees (lists of integers)";
     if degrk === null then (
	  if #degs === 0 then error "empty list requires DegreeRank to be made explicit";
	  degrk = #degs#0;
	  )
     else (
     	  if not instance(degrk,ZZ) then error "expected DegreeRank option to be an integer";
	  );
     if not all(degs, d -> #d === degrk) then error ("expected all degrees to be of length ", toString degrk);
     if #degs === 0 then return toList(degrk : 0);
     if degrk === 0 then return null;
     if degrk === 1 then return if all(degs,d->d#0 > 0) then {1} else if all(degs,d->d#0 < 0) then {-1} ;
     if all(degs,d->d#0 > 0) then return splice {  1, degrk-1:0 };
     if all(degs,d->d#0 < 0) then return splice { -1, degrk-1:0 };
     A := transpose matrix degs;
     needsPackage "FourierMotzkin";
     B := ((value getGlobalSymbol "fourierMotzkin") A)#0;
     r := rank source B;
     f := (matrix{toList(r:-1)} * transpose B);
     if f == 0 then return;
     heft := first entries f;
     if not chkHeft(degs,heft) then return;
     g := gcd heft;
     if g > 1 then heft = apply(heft, h -> h // g);
     heft);

processHeft = (degrk,degs,heft,inverses) -> (
     if inverses then return null;
     if heft === null then heft = findHeft(DegreeRank => degrk, degs)
     else (
	  if not instance(heft,List) or (
	       heft = deepSplice heft;
	       not all(heft,i -> instance(i,ZZ))
	       ) then error "expected Heft option to be a list of integers";
	  if #heft > degrk then error("expected Heft option to be of length at most the degree rank (", degrk, ")");
	  if #heft < degrk then heft = join(heft, degrk - #heft : 0);
	  if not chkHeft(degs,heft) then heft = findHeft(DegreeRank => degrk, degs);
	  );
     heft)

makeMonoid := (opts) -> (
     -- check the options for consistency, and set everything to the correct defaults
     opts = new MutableHashTable from opts;

     if opts.Local === true then opts.Global = false;

     if not member(opts.Join,{null,true,false}) then error "expected Join option to be true, false, or null";

     -- if opts.Join =!= false then (
     -- 	  if opts.DegreeMap =!= null then error "DegreeMap option provided without Join=>false";
     -- 	  if opts.DegreeLift =!= null then error "DegreeLift option provided without Join=>false";
     -- 	  );

     if class opts.Inverses =!= Boolean then error "expected true or false in option";
     
     if opts.SkewCommutative =!= {} and opts.Inverses then error "skew commutative ring with inverses requested";

     -- First check the variable names
     if class opts.Variables === ZZ 
     then (
	  x := baseName fixbasename opts.VariableBaseName;
          opts.Variables = toList (x_0 .. x_(opts.Variables - 1)))
     else (
	  v := flatten toList apply(opts.Variables, x->if class x === MutableList then toList x else x);
          opts.Variables = apply(#v, 
	       i -> (
		    try baseName v#i
		    else if instance(v#i,String) and match("[[:alnum:]$]+",v#i) then getSymbol v#i
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
     opts.Heft = processHeft(degrk,degs,opts.Heft,opts.Inverses);
     opts = new OptionTable from opts;
     makeit1 opts)

monoid List := opts -> args -> monoid (new Array from args, opts, Local => true)
monoid Array := opts -> args -> (
     (opts,args) = override(opts,toSequence args);
     if opts.Variables === null
     then opts = merge(opts, new OptionTable from {Variables => deepSplice sequence args}, last)
     else if args =!= () then error "variables provided conflict with Variables option";
     makeMonoid opts)

tensor = method( Options => tensorDefaults, Dispatch => Thing)

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

degreePad = (n,x) -> (
     if instance(x,ZZ) then x = {x};
     if not instance(x,List) or not all(x,i -> instance(i,ZZ)) then error "expected degree map to return a list of integers";
     if #x > n then error("with Join => false, expected degree map to return a list of length at most ",toString n);
     join(toList(n-#x:0),x));

degreeNoLift = () -> error "degree not liftable"

tensor(Monoid, Monoid) := Monoid => opts0 -> (M,N) -> (
     Mopts := M.Options;
     Nopts := N.Options;
     opts := new MutableHashTable from opts0;
     opts.Weights = {};
     if opts.Variables === null 
     then opts.Variables = join(Mopts.Variables, Nopts.Variables)
     else opts.Variables = spliceInside opts.Variables;
     if opts.VariableBaseName =!= null then (
	  x := fixbasename opts.VariableBaseName;
	  opts.Variables = apply(#opts.Variables, i -> x_i);
	  );
     if opts.MonomialOrder === null 
     then opts.MonomialOrder = trimMO join(Mopts.MonomialOrder,Nopts.MonomialOrder); -- product order
     if instance(opts.Degrees,List) then opts.Degrees = spliceInside opts.Degrees;
     if opts.Join === null then opts.Join = Mopts.Join;
     if opts.Degrees === null and opts.DegreeRank === null then (
	  M0 := apply(Mopts.DegreeRank, i -> 0);
	  N0 := apply(Nopts.DegreeRank, i -> 0);
	  if opts.Join === null or opts.Join === true then (
	       opts.DegreeRank = Mopts.DegreeRank + Nopts.DegreeRank;
	       opts.Degrees = join( apply(Mopts.Degrees, d -> join(d,N0)), apply(Nopts.Degrees, e -> join(M0,e)) );
	       if opts.Heft === null and Nopts.Heft =!= null and Mopts.Heft =!= null then opts.Heft = join(Mopts.Heft,Nopts.Heft);
	       opts.DegreeMap = d -> join(M0,d);
	       opts.DegreeLift = d -> (
		    for i to #M0-1 do if d#i =!= 0 then degreeNoLift();
		    drop(d,#M0));
	       )
	  else if opts.Join === false then (
	       opts.DegreeRank = Mopts.DegreeRank;
	       dm := if opts.DegreeMap =!= null then opts.DegreeMap else if Mopts.DegreeMap =!= null then Mopts.DegreeMap else identity;
	       opts.DegreeMap = d -> degreePad(opts.DegreeRank,dm d);
	       lm := if opts.DegreeLift =!= null then opts.DegreeLift else if Mopts.DegreeLift =!= null then Mopts.DegreeLift;
	       opts.DegreeLift = (
		    if lm === null then (
			 if dm === identity then (
		    	      d -> (
			 	   for i from #N0 to #M0-1 do if d#i =!= 0 then degreeNoLift();
			 	   drop(d,#M0-#N0)))
	       		 else x -> error "degree lift function not provided (DegreeLift option)")
		    else lm);
	       opts.Degrees = join(Mopts.Degrees, apply(Nopts.Degrees, opts.DegreeMap));
	       if opts.Heft === null and Mopts.Heft =!= null then opts.Heft = Mopts.Heft -* a hint *-;
	       )
	  else error "tensor: expected Join option to be true, false, or null")
     else (
     	  (degs,degrk) := processDegrees(opts.Degrees, opts.DegreeRank, length opts.Variables);
	  opts.Degrees = degs;
	  opts.DegreeRank = degrk;
	  if opts.DegreeMap === null then opts.DegreeMap = Mopts.DegreeMap;
	  if opts.DegreeLift === null then opts.DegreeLift = Mopts.DegreeLift;
	  );
     if opts.Inverses === null 
     then (
	  if Mopts.Inverses =!= Nopts.Inverses then error "in tensor product of two monoids, one has Inverses=>true and the other doesn't";
	  opts.Inverses = Mopts.Inverses;
	  )
     else opts.Inverses = opts.Inverses;
     opts.Heft = processHeft(opts.DegreeRank,opts.Degrees,opts.Heft,opts.Inverses);
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


----------------------------
-- monomial order code -----
-- this should go in its own file...
monomialOrderMatrix = method()
monomialOrderMatrix RawMonomialOrdering := (mo) -> (
     nvars := rawNumberOfVariables mo;
     mat := rawMonomialOrderingToMatrix mo;
     -- the last entry of 'mat' determines whether the tie breaker is Lex or RevLex.
     -- there may be no other elements of mat, so the next line needs to handle that case.
     ordermat := if #mat === 3 then map(ZZ^0, ZZ^nvars, 0) else matrix pack(drop(mat,-3),nvars);
     (ordermat, 
         if mat#-3 == 0 then Lex else RevLex,
         if mat#-2 == -1 then Position=>Down else if mat#-2 == 1 then Position=>Up else Position=>mat#-2,
         "ComponentBefore" => mat#-1
         )
     )
monomialOrderMatrix Monoid := (M) -> monomialOrderMatrix M.RawMonomialOrdering
monomialOrderMatrix Ring := (R) -> monomialOrderMatrix monoid R

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

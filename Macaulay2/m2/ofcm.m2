--		Copyright 1993-2002 by Daniel R. Grayson

MonoidElement = new Type of HashTable
MonoidElement.synonym = "monoid element"
new MonoidElement from RawMonomial := (MonoidElement, f) -> hashTable{ symbol RawMonomial => f }

ZZ _ Monoid := MonoidElement => (i,M) -> (
     if i === 1 then M#1
     else error "expected integer to be 1"
     )

leadMonomial RingElement := MonoidElement => (f) -> (
     R := ring f;
     M := monoid R;
     leadMonomial R := (
	  f -> new M from rawLeadMonomial f.RawRingElement
	  );
     leadMonomial f)

makeSparse := (v) -> select(apply(#v, i -> (i,v#i)), (k,v) -> v != 0)

GeneralOrderedMonoid = new Type of OrderedMonoid
GeneralOrderedMonoid.synonym = "general ordered monoid"
GeneralOrderedMonoid.Engine = true
vars GeneralOrderedMonoid := M -> M.vars
options GeneralOrderedMonoid := M -> M.Options

parts := (M) -> (
     O := monoidDefaults;
     o := M.Options;
     join(
	  if M.?generatorExpressions then M.generatorExpressions else {},
	  if any(o.Degrees, i -> i =!= {1}) then {Degrees => o.Degrees} else {},
	  select(
	       { MonomialOrder, MonomialSize, WeylAlgebra, SkewCommutative, Inverses }
	       / (key -> if o#key =!= O#key then key => o#key),
	       i -> i =!= null)))

expression GeneralOrderedMonoid := M -> if M.?name then hold M.name else new Array from apply(parts M,expression)
toString GeneralOrderedMonoid := M -> toString expression M
net GeneralOrderedMonoid := M -> net expression M

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

degreesMonoid2 := memoize(
     (n) -> (
	  T := local T;
	  Zn := monoid [if n === 1 then T else T_0 .. T_(n-1),
	       Degrees => {}, 
	       MonomialOrder => RevLex,
	       Inverses=>true];
	  Zn.name = "ZZ^" | toString n;
	  Zn))

degreesMonoid ZZ := Monoid => n -> use degreesMonoid2 n

monoidDefaults = (
     new OptionTable from {
	  VariableBaseName => null,
	  Variables => null,
	  Degrees => null,
	  Weights => {},
	  Inverses => false,
	  MonomialOrder => GRevLex,
	  MonomialSize => null,
	  SkewCommutative => false,
	  VariableOrder => null,		  -- not implemented yet
	  WeylAlgebra => {},
	  Adjust => identity,
	  Repair => identity
	  }
     )

monoid = method(SingleArgumentDispatch => true)
options PolynomialRing := options @@ monoid

generators GeneralOrderedMonoid := M -> M.generators
vars GeneralOrderedMonoid := M -> M.generators
degreesMonoid GeneralOrderedMonoid := Monoid => M -> degreesMonoid degreeLength M

standardForm(MonoidElement) := (m) -> (
     M := class m;
     convert(M.standardForm, sendgg(ggPush m, ggtonet)))
     
exponents(MonoidElement) :=
listForm(MonoidElement) := (m) -> (
     M := class m;
     convert(M.listForm, sendgg(ggPush m, ggtonet)))

MonoidElement _ GeneralOrderedMonoid := MonoidElement => (x,M) -> (baseName x)_M

Symbol _ GeneralOrderedMonoid := 
IndexedVariable _ GeneralOrderedMonoid := MonoidElement => (x,M) -> (
     if M.?generatorsTable and M.generatorsTable#?x then M.generatorsTable#x
     else error "symbol not found in monoid"
     )

Symbol _ Ring := 
IndexedVariable _ Ring := RingElement => (x,M) -> (
     if M.?generatorsTable and M.generatorsTable#?x then M.generatorsTable#x
     else error "symbol not found in monoid"
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

makeit1 := (options) -> (
     M := new GeneralOrderedMonoid of MonoidElement;
     M.Engine = true;
     varlist := options.Variables;
     n := # varlist;
     externalDegrees := options.Degrees;
     M.degrees = externalDegrees;
     M.degreeLength = if externalDegrees#?0 then # externalDegrees#0 else 0;
     internalDegrees := apply(externalDegrees,options.Adjust);
     order := transpose internalDegrees;
     primaryDegrees := if #order === 0 then toList(n:1) else order#0;
     if not all(primaryDegrees, d -> d>0) then (
	  if all(primaryDegrees, d -> d<0) then (
	       adjust := if options.Adjust === identity then minus else minus @@ options.Adjust;
	       repair := if options.Repair === identity then minus else minus @@ options.Repair;
	       options = new OptionTable from join ( pairs options, {Adjust => adjust, Repair => repair} );
	       internalDegrees = apply(externalDegrees,options.Adjust);
	       order = transpose internalDegrees;
	       primaryDegrees = order#0;
	       )
	  else error "first component of each degree should be positive"
	  );
     variableOrder := toList (0 .. n-1);
     wts := splice flatten options.Weights;
     if not all(wts,i -> class i === ZZ)
     then error "expected Weights option to be a list or list of lists of integers";
     if n != 0 and #wts % n != 0 or n == 0 and #wts != 0
     then error "expected Weights option length to be a multiple of the number of variables";
     M.generatorSymbols = varlist;
     M.generatorExpressions = apply(varlist,
	  x -> if class x === Symbol then x else expression x
	  );
     scan(varlist, 
	  sym -> (
	       if Symbol =!= basictype sym 
	       and IndexedVariable =!= class sym
	       then error "expected variable or symbol"));
     M.standardForm = ConvertApply(
	  v -> new HashTable from toList v,
	  ConvertRepeat( 
	       ConvertApply(
	       	    (i,e) -> i => e,
	       	    ConvertJoin(ConvertInteger, ConvertInteger))));
     expression M := x -> new Product from apply( 
	  rawMonomialExponents x.RawMonomial,
	  (k,v) -> Power{M.generatorExpressions#k, v} );
     M.Options = options;
     w := reverse applyTable(order, minus);
     w = if # w === 0 then apply(n,i -> {}) else transpose w;
     w = apply(w, x -> apply(makeSparse x, (k,v) -> (k + n, v)));
     if #w =!= #varlist then error "expected same number of degrees as variables";
     M.vars = M.generators = apply(# varlist, i -> new M from rawVar(i,1));
     M.generatorsTable = hashTable apply(M.generatorSymbols,M.generators,(v,x) -> v => x);
     M.index = new MutableHashTable;
     scan(#varlist, i -> M.index#(varlist#i) = i);
     M.RawMonomialOrdering = makeMonomialOrdering(
     	  options.MonomialSize,
	  options.Inverses,
     	  #varlist,
	  if degreeLength M > 0 then internalDegrees/first else {},
	  wts,
	  options.MonomialOrder
	  );
     toString M := toExternalString M := x -> toString expression x;
     M.RawMonoid = (
	  if n == 0 
	  then rawMonoid()
	  else rawMonoid(
	       M.RawMonomialOrdering,
	       M.generators / toString,
	       (degreesMonoid degreeLength M).RawMonoid,
	       flatten internalDegrees));
     net M := x -> net expression x;
     M ? M := (x,y) -> (
	  -- comparison of two monomials
	  i := 1;
	  while i <= # x and i <= # y and x#-i === y#-i do i = i+1;
	  if i > # x
	  then (
	       if i > # y 
	       then symbol ==
	       else y#-i#1 ? 0
	       )
	  else (
	       if i > # y 
	       then 0 ? x#-i#1
	       else (
		    if y#-i#0 === x#-i#0
		    then y#-i#1 ? x#-i#1
		    else y#-i#0 ? x#-i#0
		    )
	       )
	  );
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
     M#1 = new M from rawMonomialMake{};
     degree M := x -> degree x.RawMonomial;
     baseName M := x -> (
	  m := x.RawMonomial;
	  if 1 === degree x
	  then (M.generatorSymbols) # ( first first rawMonomialExponents m )
	  else error "expected a generator"
	  );
     M / M := (x,y) -> new M from x.RawMonomial / y.RawMonomial;
     M ^ ZZ := (x,n) -> new M from x.RawMonomial ^ n;
     M.use = x -> scan(M.generatorSymbols,M.vars,assign);
     M)

makeMonoid := (options) -> (
     -- check the options for consistency, and set everything to the correct defaults
     options = new MutableHashTable from options;

     if class options.SkewCommutative =!= Boolean
     or class options.Inverses =!= Boolean
     then error "expected true or false in option";
     
     if options.SkewCommutative and options.Inverses
     then error "skew commutative ring with inverses requested";

     -- First check the variable names
     if class options.Variables === ZZ 
     then (
	 x := symbol x;
         options.Variables = toList (x_0 .. x_(options.Variables - 1)))
     else (
         options.Variables =
	    apply(flatten toList apply(options.Variables,
		    x->if class x === MutableList then toList x else x), 
	       i -> (
		    try baseName i
		    else error ("'", toString i, "'", " can't be used as a variable" )
		    )
	       ));

     -- Check the degree list
     n := # options.Variables;
     degs := options.Degrees;
     if degs === null 
        then degs = apply(n,i->{1})
        else degs = splice degs;
     if # degs === 0 
        then degs = apply(n,i->{})
        else if instance(degs#0,ZZ)
            then degs = apply(degs,i -> {i});
     options.Degrees = degs;

     if class options.Adjust =!= Function then error("expected 'Adjust' option to be a function");
     if class options.Repair =!= Function then error("expected 'Repair' option to be a function");

     options = new OptionTable from options;
     makeit1 options)

monoid Array := Monoid => (
     monoidDefaults ==> options -> args -> (
     	  if options.Variables === null
     	  then options = merge(options, new OptionTable from {Variables => deepSplice sequence args}, last);
     	  makeMonoid options)
     ) @@ toSequence

monoid Ring := Monoid => R -> R.monoid

tensor = method( Options => monoidDefaults)

Monoid ** Monoid := Monoid => (M,N) -> tensor(M,N)

tensor(Monoid, Monoid) := Monoid => options -> (M,N) -> (
     M = M.Options;
     N = N.Options;
     opts := new MutableHashTable from options;
     if opts.Variables === null then
         opts.Variables = join(M.Variables, N.Variables)
	 else opts.Variables = splice opts.Variables;
     if opts.VariableBaseName =!= null then (
	  x := opts.VariableBaseName;
	  opts.Variables = apply(#opts.Variables, i -> x_i);
	  );
     if opts.MonomialOrder === null then
          -- use the product order
          opts.MonomialOrder = GRevLex;
     if opts.Degrees === null then (
	  ndegs := (
	       if # M.Degrees > 0
	       then if # N.Degrees > 0
	       then min(# M.Degrees#0, # N.Degrees#0)
	       else # M.Degrees#0
	       else if #N.Degrees > 0
	       then #N.Degrees#0
	       else 0
	       );
          opts.Degrees =
              join(apply(M.Degrees, v -> take(v,ndegs)),
                   apply(N.Degrees, v -> take(v,ndegs))));
     if opts.Inverses === null 
         then opts.Inverses = M.Inverses or N.Inverses
         else opts.Inverses = false;
     makeMonoid new OptionTable from opts)

-- delayed installation of methods for monoid elements

promote(MonoidElement, Ring) := RingElement => (m,R) -> promote(m,R#0)
promote(MonoidElement, RingElement) := RingElement => (m,o) -> (
     R := class o;
     M := monoid R;
     k := coefficientRing R;
     if M =!= class m then error "expected monomial from same ring";
     one := 1_k;
     promote(M,R) := (m,o) -> (
	  sendgg(ggPush R, ggPush one, ggPush m, ggterm);
	  new R);
     promote(m,o))

RingElement _ MonoidElement := RingElement => (f,m) -> (
     RM := ring f;
     R := coefficientRing RM;
     M := monoid RM;
     if M =!= class m then error "expected monomial from same ring";
     RM _ M := (f,m) -> new R from rawCoefficient(f.RawRingElement, m.RawMonomial);
     f _ m)

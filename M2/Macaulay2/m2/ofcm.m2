--		Copyright 1993-1999 by Daniel R. Grayson

MonoidElement = new Type of BasicList
MonoidElement.synonym = "monoid element"

ZZ _ Monoid := MonoidElement => (i,M) -> (
     if i === 1 then M#1
     else error "expected integer to be 1"
     )

leadMonomial RingElement := MonoidElement => (f) -> (
     R := ring f;
     leadMonomial R := if R.?newEngine then (
	  f -> (
	       sendgg(ggPush R, ggPush ((coefficientRing R)#1), ggPush f, ggleadmonom, ggterm);
	       R.pop()
	       )
	  )
     else (
     	  M := monoid R;
	  f -> (
	       sendgg(ggPush f, ggleadmonom);
	       M.pop()
	       )
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
	  if any(M.degrees, i -> i =!= {1}) then {Degrees => M.degrees} else {},
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

-- check #wts is divisible by #vars
-- Weights => {{2,3},{4,5}}
MOgrlex := (wts,degs) -> (sendgg(ggPush wts, ggPush degs, ggMOgrevlex); newHandle())
MOrlex := (wts,degs) -> (sendgg(ggPush wts, ggPush degs, ggMOrevlex); newHandle())
MOglex := (wts,degs) -> (sendgg(ggPush wts, ggPush degs, ggMOglex); newHandle())
MOlex := (wts,degs) -> (sendgg(ggPush wts, ggPush degs, ggMOlex); newHandle())
MOproduct := (wts,m1, m2) -> (sendgg(ggPush wts, ggPush m1, ggPush m2, ggMOproduct); newHandle())
MOelim := (wts,degs, i) -> (sendgg(ggPush wts, ggINTARRAY, gg degs, ggPush i, ggMOelim); newHandle())

MOwtfcn := (degs, wts) -> (
    sendgg(ggINTARRAY, gg degs, 
	   ggINTARRAY, gg wts, 
	   ggMOwtfcn);
    newHandle())
MOgeneral := (degs, ord, invord, invdegs) -> (
    sendgg(ggINTARRAY, gg degs, 
           ggINTARRAY, gg ord,
	   ggINTARRAY, gg invord,
	   ggINTARRAY, gg invdegs,
	   ggMOgeneral);
    newHandle())

degreesMonoid2 := memoize(
     (n) -> (
	  T := local T;
	  Zn := group [if n === 1 then T else T_0 .. T_(n-1), Degrees => {}];
	  Zn.name = "ZZ^" | toString n;
	  Zn))

degreesMonoid ZZ := Monoid => n -> use degreesMonoid2 n

newDegreesMonoid2 := memoize(
     (n) -> (
	  T := local T;
	  Zn := monoid [if n === 1 then T else T_0 .. T_(n-1), NewMonomialOrder => GroupLex => n, Degrees => {}];
	  Zn.name = "ZZ^" | toString n;
	  Zn))

newDegreesMonoid ZZ := n -> use newDegreesMonoid2 n

Eliminate = new SelfInitializingType of BasicList
new Eliminate from ZZ := (Eliminate,n) -> Eliminate {n}
expression Eliminate := v -> (
     if #v === 1 
     then new FunctionApplication from {Eliminate, v#0}
     else new FunctionApplication from {Eliminate, toList v})
ProductOrder = new SelfInitializingType of BasicList

monoidDefaults = if OLDENGINE then (
     new OptionTable from {
	  VariableBaseName => null,
	  Variables => null,
	  Degrees => null,
	  Weights => {},
	  Inverses => false,
	  MonomialOrder => GRevLex,
	  MonomialSize => 8,
	  SkewCommutative => false,
	  VariableOrder => null,		  -- not implemented yet
	  WeylAlgebra => {},
	  Adjust => identity,
	  Repair => identity
	  }
     ) else ( 
     new OptionTable from {
	  VariableBaseName => null,
	  Variables => null,
	  Degrees => null,
	  Weights => {},
	  Inverses => false,
	  MonomialOrder => GRevLex,
	  NewMonomialOrder => null,
	  MonomialSize => 8,
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

makeit1 := (options) -> (
     M := new (
	  if options.Inverses then GeneralOrderedGroup else GeneralOrderedMonoid 
	  ) of MonoidElement;
     M.Engine = true;
     M.Options = options;
     varlist := options.Variables;
     n := # varlist;
     degs := options.Degrees;
     M.degrees = degs;
     M.degreeLength = if degs#?0 then # degs#0 else 0;
     order := transpose degs;
     firstdeg := transpose degs;
     firstdeg = if #firstdeg === 0 then toList(n:1) else firstdeg#0;
     variableOrder := toList (0 .. n-1);
     wts := flatten options.Weights;
     if not all(wts,i -> class i === ZZ)
     then error "expected Weights option to be a list or list of lists of integers";
     if n != 0 and #wts % n != 0 or n == 0 and #wts != 0
     then error "expected Weights option length to be a multiple of the number of variables";
     if options.?NewMonomialOrder and options.NewMonomialOrder =!= null then (
	  M.newEngine = true;
	  M.MonomialOrder = monomialOrdering options.NewMonomialOrder;
	  )
     else (
	  mo := options.MonomialOrder;
	  M.MonomialOrder = (
	       if mo === symbol RevLex then MOrlex (wts,firstdeg)
	       else if mo === symbol GRevLex then MOgrlex (wts,firstdeg)
	       else if mo === symbol Lex then MOlex (wts,firstdeg)
	       else if mo === symbol GLex then MOglex (wts,firstdeg)
	       else if instance(mo, Eliminate) then MOelim(wts,firstdeg, mo#0)
	       else if instance(mo, ProductOrder) then MOproduct(wts, firstdeg, toList mo)
	       else error("invalid MonomialOrder option: ", toString mo)
	       )
	  );
     M.generatorSymbols = varlist;
     M.generatorExpressions = apply(varlist,
	  x -> if class x === Symbol then x else expression x
	  );
     scan(varlist, 
	  sym -> (
	       if Symbol =!= basictype sym 
	       and IndexedVariable =!= class sym
	       then error "expected variable or symbol"));
     ggPush M := x -> (
	  x = select(x, (k,v) -> k<n);
	  x = toSequence x;
	  (ggMONOMIAL, gg (# x), apply(x, (k,v) -> (gg k, gg v)))
	  );
     M.standardForm = ConvertApply(
	  v -> new HashTable from toList v,
	  ConvertRepeat( 
	       ConvertApply(
	       	    (i,e) -> i => e,
	       	    ConvertJoin(ConvertInteger, ConvertInteger))));
     iv := toList( n : 0 );
     toVec := p -> (
	  s := new MutableList from iv;
	  scan(p, (i,e) -> s#i = e);
	  toList s);
     M.listForm = ConvertApply(
	  toVec,
	  ConvertRepeat ConvertJoin(ConvertInteger, ConvertInteger));
     M.ConversionFormat = ConvertApply(
	  v -> if # v === 0 then M#1 else times v,
	  ConvertRepeat( 
	       ConvertApply(
		    (i,e) -> M_i^e,
	       	    ConvertJoin(ConvertInteger, ConvertInteger))));
     M.ConvertToExpression = ConvertApply(
	  args -> (
	       if #args === 1 then args#0
	       else if #args === 0 then expression 1
	       else new Product from toList args
	       ),
	  ConvertRepeat ConvertApply (
	       (v,i) -> if i != 1 then (expression v)^(expression i) else expression v,
	       ConvertJoin( 
		    ConvertApply(i -> M.generatorExpressions#i, ConvertInteger), 
		    ConvertInteger)));
     expression M := x -> (
	  x = select(x, (k,v) -> k < n);
	  x = new Product from apply(
	       toList x,
	       (k,v) -> Power{M.generatorExpressions#k, v}
	       );
	  if #x === 0 then expression 1 else x
	  );
     M.pop = () -> eePop(M.ConversionFormat);
     w := reverse applyTable(order, minus);
     w = if # w === 0 then apply(n,i -> {}) else transpose w;
     w = apply(w, x -> apply(makeSparse x, (k,v) -> (k + n, v)));
     if #w =!= #varlist then error "expected same number of degrees as variables";
     M.vars = M.generators = apply(# varlist, i -> new M from join( {(i,1)}, w#i));
     M.index = new MutableHashTable;
     scan(#varlist, i -> M.index#(varlist#i) = i);
     toString M := toExternalString M := x -> toString expression x;
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
     M * M := (x,y) -> mergePairs(x,y,plus);
     M#1 = new M from {};
     degree M := x -> (
	  if # x === 0
	  then apply(M.degreeLength,i->0)
	  else sum(select(toList x, (v,e)->v<n), (v,e) -> e * degs#v)
	  );
     baseName M := x -> (
	  if 1 === number(x, (v,e) -> v<n) and x#0#1 === 1
	  then (M.generatorSymbols) # (x#0#0)
	  else error "expected a generator"
	  );
     if options.Inverses then (
	  M / M := (x,y) -> select(x apply(y,(k,v)->(k,-v)), (k,v)->v!=0);
	  M ^ ZZ := (x,n) -> if n === 0 then M#1 else apply(x,(m,e)->(m,e*n));
	  )
     else (
	  M / M := (x,y) -> select(
	       x apply(y,(k,v)->(k,-v)), 
	       (k,v) -> v != 0);
	  M ^ ZZ := (x,n) -> (
	       if n < 0 then error "expected nonnegative exponent"
	       else if n === 0 then M#1 
	       else apply(x,(m,e)->(m,e*n)));
	  M | M := (x,y) -> try (y/x; true) else false;
	  M // M := (x,y) -> (
	       -- this doesnt get the right weights!!! XXXX
	       select(x apply(y,(k,v)->(k,-v)), (k,v)->v>0)
	       );
	  );
     betwNames := (m,v) -> concatenate between(m,
	  apply(v, x -> concatenate lines(toString x, " "))
	  );
     M.handle = newHandle (
	  ggPush M.MonomialOrder,
	  if M.?newEngine then ggPush variableOrder,
	  ggPush betwNames(" ",M.generators),		    -- or "" to omit them
	  if not M.?newEngine then (
	       if degreeLength M === 0 then ggzeromonoid else ggPush degreesMonoid degreeLength M,
	       ggPush flatten M.degrees,
	       ggPush {if options.Inverses then 1 else 0, 
		       options.MonomialSize,
		       if options.SkewCommutative then 1 else 0}
	       ),
	  ggmonoid) ;
     M.use = x -> scan(M.generatorSymbols,M.vars,assign);
     M)

makeit1 = memoize makeit1

makeMonoid := (options) -> (
     -- check the options for consistency, and set everything to the correct defaults
     options = new MutableHashTable from options;

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

     options = new OptionTable from options;
     makeit1 options)

monoid Array := Monoid => (
     monoidDefaults ==> options -> args -> (
     	  if options.Variables === null
     	  then options = merge(options, new OptionTable from {Variables => deepSplice sequence args}, last);
     	  makeMonoid options)
     ) @@ toSequence

monoid Ring := Monoid => R -> R.monoid

GeneralOrderedGroup = new Type of GeneralOrderedMonoid
GeneralOrderedGroup.synonym = "general ordered group"
GeneralOrderedGroup.Engine = true
toString GeneralOrderedGroup := M -> if M.?name then M.name else (
     if not M.?generatorExpressions
     then "--empty GeneralOrderedGroup--"
     else (
     	  v := M.generatorExpressions;
     	  if any(M.degrees, i -> i != {1}) 
	  then v = append(v, Degrees => M.degrees);
     	  concatenate("group [",between(",",toString\v),"]")
	  ))

group = method()
group Ring := R -> R.group

generators GeneralOrderedGroup := G -> G.generators
vars GeneralOrderedGroup := G -> G.generators

group Array := X -> monoid append(X,Inverses=>true)

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
--             Product=>
--             {{# M.Variables,M.MonomialOrder}, 
--             {# N.Variables,N.MonomialOrder}};
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
     RM _ M := (f,m) -> (
	  sendgg(ggPush f, ggPush m, gggetcoeff);
	  R.pop()
	  );
     f _ m)

stats GeneralOrderedMonoid := (M) -> sendgg(ggPush M, ggstats)

--		Copyright 1994 by Daniel R. Grayson

MonoidElement = new Type of BasicList
document { quote MonoidElement,
     TT "MonoidElement", " -- the class of all monoid elements.",
     PARA,
     SEEALSO "monoid"
     }

makeSparse := (v) -> select(apply(#v, i -> (i,v#i)), (k,v) -> v != 0)

document { quote Degrees,
     TT "Degrees", " -- an option which specifies the degrees of the generators.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.",
     PARA,
     "See ", TO "monoid", " for details."
     }
document { quote SkewCommutative,
     TT "SkewCommutative", " -- name for an optional argument for monoids
     that specifies that monoid rings created from them will be skewcommutative.",
     PARA,
     "The default value is false.",
     EXAMPLE "R = ZZ[x,y,SkewCommutative=>true]",
     EXAMPLE "x*y",
     EXAMPLE "y*x"
     }

document { quote MonomialSize,
     TT "MonomialSize => n", " -- an option which determines the maximum 
     exponent size.",
     PARA,
     "Used as an option to ", TO "monoid", ", or when a polynomial ring
     is created.  Setting 'MonomialSize=>n' specifies that monomial exponents 
     may be as large as 2^n - 1.  The default value is 8, allowing for exponents 
     up to 255.",
     PARA,
     "See ", TO "monoid", " for details."
     }

document { quote Inverses,
     TT "Inverses", " -- an option used in creating a monoid which tells
     whether negative exponents will be allowed, making the monoid into
     a group.",
     SEEALSO "monoid"
     }

GeneralOrderedMonoid = new Type of OrderedMonoid
GeneralOrderedMonoid.Engine = true
vars GeneralOrderedMonoid := M -> M.vars
options GeneralOrderedMonoid := M -> M.Options
name GeneralOrderedMonoid := M -> (
     if not M.?syms
     then "--empty GeneralOrderedMonoid--"
     else (
     	  v := M.syms;
     	  if any(M.degrees, i -> i != {1}) 
	  then v = append(v, Degrees => M.degrees);
          if M.Options.MonomialOrder =!= (options monoid).MonomialOrder
          then v = append(v, MonomialOrder => M.Options.MonomialOrder);
          if M.Options.MonomialSize =!= (options monoid).MonomialSize
          then v = append(v, MonomialSize => M.Options.MonomialSize);
	  if M.Options.SkewCommutative
	  then v = append(v, SkewCommutative => M.Options.SkewCommutative);
     	  concatenate("[",between(",",name\v),"]")
	  ))
net GeneralOrderedMonoid := M -> (
     if not M.?syms
     then "--empty GeneralOrderedMonoid--"
     else (
     	  v := M.syms;
     	  if any(M.degrees, i -> i != {1}) 
	  then v = append(v, Degrees => M.degrees);
          if (M.Options.MonomialOrder =!= (options monoid).MonomialOrder)
          then v = append(v, MonomialOrder => M.Options.MonomialOrder);
          if (M.Options.MonomialSize =!= (options monoid).MonomialSize)
          then v = append(v, MonomialSize => M.Options.MonomialSize);
     	  horizontalJoin flatten ("[",between(",",net\v),"]")
	  ))

document { quote GeneralOrderedMonoid,
     TT "GeneralOrderedMonoid", " -- the class of all ordered free 
     commutative monoids, as implemented by ", TO "monoid", ".",
     PARA,
     "This is the class of free monoids that can be handled by the ",
     TO "engine", ".",
     PARA,
     "Functions:",
     MENU {
	  TO "degree"
	  },
     PARA,
     "Keys:",
     MENU {
	  TO "degreesMonoid",
	  TO "index"
	  },
     PARA,
     SEEALSO ( "monoid", "Degrees", "MonoidElement")
     }     

-- this implementation is for sparse monomials, but it might
-- make sense to have a dense implementation
GeneralOrderedMonoid _ ZZ := (M,i) -> M.generators#i
GeneralOrderedMonoid _ List := (M,v) -> product(M.generators,v,(x,i)->x^i)
numgens GeneralOrderedMonoid := M -> # M.generators
degreeLength GeneralOrderedMonoid := M -> M.degreeLength

MOgrlex := (degs) -> (sendgg(ggPush degs, ggMOgrevlex); newHandle())
MOrlex := (degs) -> (sendgg(ggPush degs, ggMOrevlex); newHandle())
MOglex := (degs) -> (sendgg(ggPush degs, ggMOglex); newHandle())
MOlex := (degs) -> (sendgg(ggPush degs, ggMOlex); newHandle())
MOproduct := (m1, m2) -> (sendgg(ggPush m1, ggPush m2, ggMOproduct); newHandle())
MOelim := (degs, i) -> (sendgg(ggINTARRAY, gg degs, ggPush i, ggMOelim); newHandle())
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
	  t := local T;
	  Zn := group [if n === 1 then t else t_0 .. t_(n-1), 
			Degrees => {}]; -- MonomialOrder => RevLex];
	  Zn.name = "ZZ^" | name n;
	  Zn))

degreesMonoid ZZ := n -> use degreesMonoid2 n

document { quote degreesMonoid,
     TT "degreesMonoid n", " -- returns the monoid whose elements correspond
     to the multi-degrees of monomials in another monoid.",
     PARA,
     "Also used as a key under which to store the result."
     }

Eliminate = new SelfInitializingType of BasicList
new Eliminate from ZZ := (Eliminate,n) -> Eliminate {n}
ProductOrder = new SelfInitializingType of BasicList

document { quote RevLex,
     TT "RevLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the reverse lexicographic order."
     }
document { quote GRevLex,
     TT "GRevLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the graded reverse lexicographic order.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { quote GLex,
     TT "GLex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the graded lexicographic order.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { quote Lex,
     TT "Lex", " -- a symbol used as an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the (non-graded) lexicographic order."
     }
document { quote Eliminate,
     TT "Eliminate", " n -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the elimination order eliminating the
     first n variables, refined by the graded reverse lexicographic order.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, this
     is currently only graded using the first degree vector.  This will 
     eventually change."  -- MES
     }
document { quote ProductOrder,
     TT "ProductOrder", "{n1, ..., nr} -- an optional argument of
     ", TO "MonomialOrder", " in monoids handled by the ", TO "engine", " to
     indicate that the monomial order is the product of r graded reverse lex
     orders, each with n1, n2, ..., nr variables.",
     PARA,
     "Caveat: If the number of degree vectors is greater than one, the
     grading in each block only uses the first degree vector. This will 
     eventually change."  -- MES
     }

document { quote VariableBaseName,
     TT "VariableBaseName => x", " -- an optional argument used when creating
     monoids or rings to specify that the variables should be ",
     TT "x_0, ..., x_n", "."
     }

document { quote MonomialOrder,
     TT "MonomialOrder", " -- a key used with monoids to indicate a
     monomial order other than the default (graded reverse lexicographic)",
     PARA,
     "Values:",
     MENU {
	  {TO "GRevLex", " -- graded reverse lexicographic order (the default)"},
	  {TO "GLex", " -- graded lexicographic order"},
	  {TO "Lex", " -- lexicographic order"},
	  {TO "RevLex", " -- reverse lexicographic order"},
	  {TO "Eliminate", " n -- elimination order, eliminating first n variables"},
	  {TO "ProductOrder", "{n1, n2, ..., nv} -- product order"}
          },
     "Eventually, more general monomial orders will be allowed." -- MES
     }

document { quote Variables,
     TT "Variables", " -- a key used with monoids to indicate the list of 
     variable names, or the number of variables.",
     PARA,
     "This option is useful for those situations when one doesn't care about the
     names of the variables in a ring or monoid, or when one is creating a 
     tensor product ring, symmetric algebra, or other ring, and one wants control
     over the names of the ring variables. See also ", TO "tensor", "."
     }

monoidDefaults := new OptionTable from {
     VariableBaseName => null,
     Variables => null,
     Degrees => null,
     Inverses => false,
     MonomialOrder => null,
     MonomialSize => 8,
     SkewCommutative => false
     }
monoid = method()
document { quote monoid,
     TT "monoid R      ", " -- yields the underlying monoid of polynomial ring, 
                        group ring, or monoid ring.",
     PARA,
     "monoid [a,b,c] -- makes a free ordered commutative monoid on the
                        variables listed.",
     PARA,
     "Options available:",
     MENU {
	  TO "Degrees",
	  TO "Inverses",
	  TO "MonomialOrder",
	  TO "MonomialSize",
	  TO "SkewCommutative",
	  TO "Variables",
	  TO "VariableBaseName"
	  },
     PARA,
     "monoid [a,b,c,Degrees=>{2,3,4}] 
          -- makes a free ordered commutative monoid on the
	     variables listed, with degrees 2, 3, and 4, respectively.",
     PARA,
     "monoid [a,b,c,Degrees=>{{1,2},{3,-3},{0,4}}] 
          -- makes a free ordered commutative monoid on the
	     variables listed, with multi-degrees as listed.",
     PARA,
     "monoid [a,b,c,Degrees=>{{},{},{}}] 
          -- makes a free ordered commutative monoid on the
	     variables listed, ungraded.",
     PARA,
     "The variables listed may be symbols or indexed variables.
     The values assigned to these variables (with ", TO "assign", ") are
     the corresponding monoid generators.  The function ", TO "baseName", "
     may be used to recover the original symbol or indexed variable.",
     PARA,
     "The class of all monoids created this way is ",
     TO "GeneralOrderedMonoid", ".",
     PARA,
     SEEALSO ("OrderedMonoid","IndexedVariable","Symbol")
     }

generators GeneralOrderedMonoid := M -> M.generators
vars GeneralOrderedMonoid := M -> M.generators
degreesMonoid GeneralOrderedMonoid := M -> degreesMonoid degreeLength M

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
     mo := options.MonomialOrder;
     firstdeg := transpose degs;
     if # firstdeg === 0 then
	firstdeg = elements(n:1)
     else
        firstdeg = firstdeg#0;
     if mo === null then
	mo = MOgrlex firstdeg
     else if mo === quote RevLex then
	mo = MOrlex firstdeg
     else if mo === quote GRevLex then
	mo = MOgrlex firstdeg
     else if mo === quote Lex then
	mo = MOlex firstdeg
     else if mo === quote GLex then
	mo = MOglex firstdeg
     else if instance(mo, Eliminate) then
	mo = MOelim(firstdeg, mo#0)
     else if instance(mo, ProductOrder) then
	mo = MOproduct(firstdeg, elements mo)
--     else if mo === Weights wts then
--	mo = MOwtfcn(firstdeg, wts)
--     else if mo === General then
--	mo = MOgeneral(firstdeg, order, invorder, invdegs)
     else error "invalid MonomialOrder option";
     M.MonomialOrder = mo;

     M.syms = varlist;
     scan(varlist, 
	  sym -> (
	       if Symbol =!= basictype sym 
	       and IndexedVariable =!= class sym
	       then error "expected variable or symbol"));
     ggPush M := x -> (
	  x = select(x, (k,v) -> k<n);
	  x = unlist x;
	  (ggMONOMIAL, gg (# x), apply(x, (k,v) -> (gg k, gg v)))
	  );
     M.standardForm = ConvertApply(
	  v -> new HashTable from elements v,
	  ConvertRepeat( 
	       ConvertApply(
	       	    (i,e) -> i => e,
	       	    ConvertJoin(ConvertInteger, ConvertInteger))));
     iv := elements( n : 0 );
     toVec := p -> (
	  s := new MutableList from iv;
	  scan(p, (i,e) -> s#i = e);
	  elements s);
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
	       else new Product from elements args
	       ),
	  ConvertRepeat ConvertApply (
	       (v,i) -> if i != 1 then (expression v)^(expression i) else expression v,
	       ConvertJoin( 
		    ConvertApply(i -> M.syms#i, ConvertInteger), 
		    ConvertInteger)));
     expression M := x -> (
	  x = select(x, (k,v) -> k < n);
	  x = new Product from apply(
	       elements x,
	       (k,v) -> new Power from {expression M.syms#k, v}
	       );
	  if #x === 0 then expression 1 else x
	  );
     M.pop = () -> eePop(M.ConversionFormat);
     w := reverse applyTable(order, minus);
     w = if # w === 0 then apply(n,i -> {}) else transpose w;
     w = apply(w, x -> apply(makeSparse x, (k,v) -> (k + n, v)));
     M.vars = M.generators = apply(# varlist, i -> new M from join( {(i,1)}, w#i));
     M.index = new MutableHashTable;
     scan(#varlist, i -> M.index#(varlist#i) = i);
     name M := x -> name expression x;
     net M := x -> net expression x;
     M ? M := (x,y) -> (
	  -- comparison of two monomials
	  i := 1;
	  while i <= # x and i <= # y and x#-i === y#-i do i = i+1;
	  if i > # x
	  then (
	       if i > # y 
	       then quote ==
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
	       "expected a list of length ", name n
	       );
	  x = flatten elements x;
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
	  else sum(select(elements x, (v,e)->v<n), (v,e) -> e * degs#v)
	  );
     baseName M := x -> (
	  if 1 === number(x, (v,e) -> v<n) and x#0#1 === 1
	  then (M.syms) # (x#0#0)
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
	  apply(v, x -> concatenate lines(name x, " "))
	  );
     M.handle = newHandle (
	  ggPush M.MonomialOrder,
	  ggPush betwNames(" ",M.generators),-- or "" to omit them
	  if degreeLength M === 0 then ggzeromonoid
	  else ggPush degreesMonoid degreeLength M,
	  ggPush flatten M.degrees,
	  ggPush {if options.Inverses then 1 else 0, 
                  options.MonomialSize,
                  if options.SkewCommutative then 1 else 0},
	  ggmonoid) ;
     M.use = x -> scan(M.syms,M.vars,assign);
     M)

makeit1 = memoize makeit1

makeMonoid := (options) -> (
     -- check the options for consistency, and set everything to the correct defaults
     options = new MutableHashTable from options;

     -- First check the variable names

     if class options.Variables === ZZ 
     then (
	 x := quote x;
         options.Variables = elements (x_0 .. x_(options.Variables - 1)))
     else (
         options.Variables =
	    apply(flatten elements 
	       apply(options.Variables,
		    x->if class x === MutableList then elements x else x), 
	       i -> (
		    try baseName i
		    else error ("'", name i, "'", " can't be used as a variable" )
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

makeit2 := (args,options) -> (
     options = new MutableHashTable from options;
     if options.Variables === null then
          options.Variables = splice args;
     options = new OptionTable from options;
     makeMonoid options)

monoid Array := (args) -> use processArgs(unlist args,monoidDefaults,makeit2)
OptionsRegistry#monoid = monoidDefaults
monoid Ring := R -> R.monoid

GeneralOrderedGroup = new Type of GeneralOrderedMonoid
GeneralOrderedGroup.Engine = true
name GeneralOrderedGroup := M -> (
     if not M.?syms
     then "--empty GeneralOrderedGroup--"
     else (
     	  v := M.syms;
     	  if any(M.degrees, i -> i != {1}) 
	  then v = append(v, Degrees => M.degrees);
     	  concatenate("group [",between(",",name\v),"]")
	  ))

document { quote GeneralOrderedGroup,
     TT "GeneralOrderedGroup", " -- the class of all ordered free 
     commutative groups, as implemented by ", TO "group", ".",
     PARA,
     "This is the class of free commutative groups that can be 
     handled by the ", TO "engine", ".",
     PARA, "Functions:", MENU { 
	  TO "degree"
	  },
     PARA, "Keys:", MENU {
	  TO "index"
	  },
     PARA,
     SEEALSO ( "group", "Degrees" )
     }     

group = method()
group Ring := G -> G.group

document { quote group,
     TT "group R      ", " -- yields the underlying group of a group ring.",
     PARA,
     "group [a,b,c] -- makes a free ordered commutative group on the
     variables listed.",
     PARA,
     "group [a,b,c,Degrees=>{2,3,4}] 
     -- makes a free ordered commutative group on the
        variables listed, with degrees 2, 3, and 4, respectively.",
     PARA,
     "group [a,b,c,Degrees=>{{1,2},{3,-3},{0,4}}] 
     -- makes a free ordered commutative group on the
        variables listed, with multi-degrees as listed.",
     PARA,
     "group [a,b,c,Degrees=>{{},{},{}}] 
          -- makes a free ordered commutative group on the
	     variables listed, ungraded.",
     PARA,
     "The class of all groups created this way is ",
     TO "GeneralOrderedMonoid", ".",
     PARA,
     SEEALSO ( "Degrees", "OrderedMonoid", "monoid" )
     }

generators GeneralOrderedGroup := G -> G.generators
vars GeneralOrderedGroup := G -> G.generators

group Array := X -> monoid append(X,Inverses=>true)

tensor = method( Options => options monoid )

Monoid ** Monoid := (M,N) -> tensor(M,N)

tensor(Monoid, Monoid) := (M,N,options) -> (
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

document { quote tensor,
  TT "tensor(M,N)", " -- tensor product of rings or monoids.",
  PARA,
  "This method allows all of the options available for monoids, see
  ", TO "monoid", " for details.  This routine essentially combines the 
  variables of M and N into one monoid.",
  PARA,
  "For rings, the rings should be quotient rings of polynomial rings over the same
  base ring.",
  PARA,
  "Here is an example with monoids.",
  EXAMPLE "M = monoid[a..d, MonomialOrder => Eliminate 1]",
  EXAMPLE "N = monoid[e,f,g, Degrees => {1,2,3}]",
  EXAMPLE "P = tensor(M,N,MonomialOrder => GRevLex)",
  EXAMPLE "describe P",
  EXAMPLE "tensor(M,M,Variables => {x_0 .. x_7}, MonomialOrder => ProductOrder{4,4})",
  EXAMPLE "describe oo",
  "Here is a similar example with rings.",
  EXAMPLE "tensor(ZZ/101[x,y], ZZ/101[r,s], MonomialOrder => Eliminate 2)",
  SEEALSO "**"
  }

-- delayed installation of methods for monoid elements

promote(MonoidElement, Ring) := (m,R) -> promote(m,R#0)
promote(MonoidElement, RingElement) := (m,o) -> (
     R := class o;
     M := monoid R;
     k := coefficientRing R;
     if M =!= class m then error "expected monomial from same ring";
     one := 1_k;
     promote(M,R) := (m,o) -> (
	  sendgg(ggPush R, ggPush one, ggPush m, ggterm);
	  new R);
     promote(m,o))

RingElement _ MonoidElement := (f,m) -> (
     RM := ring f;
     R := coefficientRing RM;
     M := monoid RM;
     if M =!= class m then error "expected monomial from same ring";
     RM _ M := (f,m) -> (
	  sendgg(ggPush f, ggPush m, gggetcoeff);
	  R.pop()
	  );
     f _ m)

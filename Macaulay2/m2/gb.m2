--		Copyright 1995-2002 by Daniel R. Grayson

GroebnerBasis = new Type of MutableHashTable
GroebnerBasis.synonym = "Groebner basis"
toString GroebnerBasis := g -> toString new FunctionApplication from { gb, g.matrix }
net GroebnerBasis := g -> net gens g

summary GroebnerBasis := g -> (sendgg(ggPush g, ggstats);)

gbTrace = (n) -> (
     if class n === ZZ then (
	  stderr << "warning: engine tracing not re-implemented yet" << endl
	  )
     else error "expected an integer" )

bool := t -> if t then 1 else 0

gbOnly       := {false,  0}
gbWithChg    := {false, -1}
gbWithSyzygy := {true , -1}
--     	    	        ^^ --- number of rows to retain, or -1 for all
--     	         ^^^^ -------- whether to collect syzygies

-- tracingLevel := 0

makeGB := (f,type,strategy) -> (
     if f.cache#?type then f.cache#type
     else if (
	  type===gbOnly 
	  and f.cache#?gbWithChg 
	  and f.cache#gbWithChg.?returnCode 
	  and f.cache#gbWithChg.returnCode === 0
	  )
     then f.cache#gbWithChg
     else if (
	  ( type===gbOnly or type===gbWithChg ) 
	  and f.cache#?gbWithSyzygy
	  and f.cache#gbWithSyzygy.?returnCode 
	  and f.cache#gbWithSyzygy.returnCode === 0
	  )
     then f.cache#gbWithSyzygy
     else (
	  g := new GroebnerBasis;
	  withSyz := type#0;
	  rowsToKeep := type#1;
	  -- if tracingLevel >= 3 then (
	  --     << "keeping " << rowsToKeep << " rows of the syzygies" << endl;
	  --     );
	  g.GBtype = type;
	  g.matrix = f;
	  g.ring = ring f;
	  g.target = target f;
	  error "gb's not re-implemented yet";
	  g.handle = newHandle (
	       ggPush f,
	       ggPush bool withSyz,
	       ggPush rowsToKeep,
	       if not withSyz and f.?cache and f.cache.?cokernel and f.cache.cokernel.?poincare then (
		    -- if tracingLevel >= 3 then (
			-- << "using Poincare polynomial " << f.cokernel.poincare << " as hint" << endl;
			-- );
	            ggPush f.cache.cokernel.poincare		    -- the Poincare polynomial
	            )
	       else (
		    -- if tracingLevel >= 3 then << "no Poincare polynomial as hint" << endl;
     	       	    null
		    ),
	       ggPush strategy, 	  -- which strategy to use (0=default)
	       gggb);
	  f.cache#type = g;			  -- do this last (interrupts!)
	  g))

runGB := (G,ggcmds) -> (
     -- gbTrace ( tracingLevel = gbTrace 0 );
     -- if tracingLevel >= 4 then << "computing gb with type " << G.GBtype << endl;
     sendgg(ggPush G, ggcmds);
     sendgg ggcalc;
     G.returnCode = eePopInt();
     )

protect symbol StopBeforeComputation
protect symbol DegreeLimit
protect symbol BasisElementLimit
protect symbol SyzygyLimit
protect symbol PairLimit
protect symbol CodimensionLimit
protect symbol StopWithMinimalGenerators
protect symbol Syzygies
protect symbol ChangeMatrix
protect symbol SyzygyRows
protect symbol Strategy

inf := t -> if t === infinity then -1 else t

cl :=method()
cl ZZ := t -> {t}
cl List := t -> (
     scan(t, i -> if class i =!= ZZ then error "expected list of integers");
     t)

gb = method(
     TypicalValue => GroebnerBasis,
     Options => {
	  StopBeforeComputation => false,
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  SyzygyLimit => infinity,
	  PairLimit => infinity,
	  CodimensionLimit => infinity,
          SubringLimit => infinity,
	  StopWithMinimalGenerators => false,
	  Syzygies => false,
	  ChangeMatrix => false,
	  SyzygyRows => infinity,
	  Strategy => {}
	  }
     )

strategyCodes := new HashTable from {
     LongPolynomial => 8,
     Sort => 16,
     Primary => 3,
     Inhomogeneous => 2,
     Homogeneous => 1
     }

processStrategy := (v) -> (
     if class v =!= List then v = {v};
     sum(v,s->(
	       if not strategyCodes#?s
	       then error("unknown strategy ", toString s, " encountered");
	       strategyCodes#s)))     

gb Ideal := GroebnerBasis => options -> (I) -> gb ( module I, options )

gb Module := GroebnerBasis => options -> (M) -> (
     if M.?relations 
     then (
	  notImplemented();
	  -- provisional
	  m := generators M;
	  n := relations M;
	  gb (m|n, 
	       options,
	       -- ChangeMatrix => true,
	       -- Syzygies => true,
	       SyzygyRows => numgens source m))
     else gb(generators M, options))

gb Matrix := GroebnerBasis => options -> (f) -> (
     R := ring target f;
     if ring source f =!= R
     then error "expected module map with source and target over the same ring";
     if not isFreeModule target f
     then error "Groebner bases of subquotient modules not yet implemented";
     if not isFreeModule source f
     then f = ambient f * generators source f;   -- sigh
     if isPolynomialRing R and not (isField coefficientRing R or coefficientRing R === ZZ)
     then error "expected coefficient ring to be ZZ or a field";
     if isPolynomialRing R and coefficientRing R === ZZ and not isHomogeneous f
     then (
	  -- do it by homogenization
	  error "inhomogeneous Groebner bases over ZZ not implemented yet";
	  )
     else (
	  type := {
	       options.Syzygies,
	       if options.Syzygies or options.ChangeMatrix
	       then inf options.SyzygyRows else 0
	       };
	  strat := processStrategy options.Strategy;
	  G := makeGB(f, type, strat);
	  if not options.StopBeforeComputation 
	  then runGB(G, (
		    ggPush cl options.DegreeLimit,
		    ggPush {
			 inf options.BasisElementLimit,
			 inf options.SyzygyLimit,
			 inf options.PairLimit,
			 inf options.CodimensionLimit,
			 bool options.StopWithMinimalGenerators,
			 inf options.SubringLimit,
			 strat
			 }));
	  G))

mingens GroebnerBasis := Matrix => options -> (g) -> (
     sendgg(ggPush g, gggetmingens);
     getMatrix ring g			  -- we're losing information here! MES
     )

syz = method(Options => options gb)

syz GroebnerBasis := Matrix => options -> (g) -> (
     sendgg(ggPush g, gggetsyz);
     getMatrix ring g )

generators GroebnerBasis := Matrix => (g) -> (
     sendgg(ggPush g, gggetgb);
     getMatrix ring g)

getChangeMatrix GroebnerBasis := Matrix => (g) -> (
     sendgg(ggPush g, gggetchange);
     getMatrix ring g)

forceGB = method(
     TypicalValue => GroebnerBasis,
     Options => {
          MinimalMatrix => null,
	  SyzygyMatrix => null,
	  ChangeMatrix => null
	  }
     )

forceGB Matrix := GroebnerBasis => options -> (f) -> (
     if not isFreeModule source f then error "expected a free module";
     minmat := if options.MinimalMatrix === null
               then f
               else options.MinimalMatrix;
     changemat := if options.ChangeMatrix === null
               then id_(source f)
               else options.ChangeMatrix;
     syzmat := if options.SyzygyMatrix === null
               then map(target changemat, 0)
               else options.SyzygyMatrix;
     nsyz := numgens target changemat;
     if nsyz >= numgens source minmat then nsyz = -1;
     type := {
	  options.SyzygyMatrix =!= null,
          nsyz
	  };
     g := new GroebnerBasis;
     g.GBtype = type;
     g.matrix = f;
     g.ring = ring f;
     g.target = target f;
     g.returnCode = 0;
     g.handle = newHandle(
          ggPush minmat,
          ggPush f, 
          ggPush changemat, 
          ggPush syzmat, 
          gggb);
     f.cache#type = g;
     g)

Matrix // GroebnerBasis := Matrix => (n,g) -> (
     -- this gb might not be one with change of basis matrix attached...
     -- so it is best for the user not to use it
     R := ring g;
     if R =!= ring n then error "expected matrix over the same ring";
     sendgg(ggPush g, ggPush n, ggreduce, ggPush 1, ggpick, ggpop);
     getMatrix R)
RingElement // GroebnerBasis := Matrix => (r,g) -> (r * id_(target g)) // g

Matrix % GroebnerBasis := Matrix => (n,g) -> (
     R := ring n;
     if R =!= ring g then error "expected matrix over the same ring";
     sendgg(ggPush g, ggPush n, ggreduce, ggpop);
     getMatrix R)

RingElement % GroebnerBasis := RingElement =>
ZZ % GroebnerBasis := (r,g) -> ((r * id_(target g)) % g)_(0,0)

GroebnerBasis == GroebnerBasis := (g,h) -> (
     ring g === ring h
     and (
     	  sendgg(ggPush g, ggPush h, ggisequal);
     	  eePopBool()))

-- Auto-reduction
autoReduce = method()
autoReduce Matrix := (m) -> (
     sendgg(ggPush m, ggautoreduce);
     getMatrix ring m)

///
TEST
R = ZZ/101[a..f]
m = matrix{{a^2-b-c,b^2-c-d,c^4-b^3-2*d}}
autoReduce m
///     

Index: e/x_mat.cpp
===================================================================
--- e/x_mat.cpp	(revision 4020)
+++ e/x_mat.cpp	(working copy)
@@ -536,7 +536,7 @@
   Matrix::iterator i(f);
   for (int c=0; c<f->n_cols(); c++)
     for (i.set(c); i.valid(); i.next())
-      if (R->promote(S,i.entry(),a))
+      if (S->promote(S,i.entry(),a))
 	mat.set_entry(i.row(), c, a);
       else
 	{
Index: e/QQ.cpp
===================================================================
--- e/QQ.cpp	(revision 4020)
+++ e/QQ.cpp	(working copy)
@@ -138,8 +138,8 @@
 
 bool QQ::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
 {
-  // Rf = ZZ ---> QQ
-  if (Rf->is_ZZ())
+  // ZZ ---> QQ = Rf
+  if (Rf->is_QQ())
     {
       result = QQ::from_int(MPZ_VAL(f));
       return true;
Index: m2/rationals.m2
===================================================================
--- m2/rationals.m2	(revision 4020)
+++ m2/rationals.m2	(working copy)
@@ -38,23 +38,17 @@
 
 QQ.Engine = true
 assert (hash ZZ < hash QQ)
+
 promote(ZZ,QQ) := (n,QQ) -> n/1
-lift(QQ,ZZ) := (r,o) -> (
-     if denominator r === 1 then numerator r 
-     else error "rational number is not an integer"
-     )
-promote(QQ,QQ) := (r,QQ) -> r
-lift(QQ,QQ) := (r,QQ) -> r
+lift(QQ,ZZ) := (r,o) -> if denominator r === 1 then numerator r else error "rational number is not an integer"
+liftable(QQ,ZZ) := (r,o) -> denominator r === 1
+lift(QQ,QQ) := promote(QQ,QQ) := (r,QQ) -> r
+liftable(QQ,QQ) := (QQ,QQ) -> true
+
 QQ.degreeLength = 0
 isUnit QQ := x -> x != 0
-
 isConstant QQ := i -> true
 
-promote(QQ,QQ) := (i,o) -> i
-promote(ZZ,QQ) := (i,o) -> i/1
-promote(QQ,Ring) := (r,S) -> promote(r,S#0)
-
-
 -- Local Variables:
 -- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
 -- End:
Index: m2/complex.m2
===================================================================
--- m2/complex.m2	(revision 4020)
+++ m2/complex.m2	(working copy)
@@ -92,7 +92,6 @@
 promote(RR,CC) := 
 promote(QQ,CC) := 
 promote(ZZ,CC) := (i,o) -> i + zeroCC
-promote(CC,Ring) := (r,S) -> promote(r,S#0)
 promote(CC,CC) := (i,o) -> i
 
 -- Local Variables:
Index: m2/integers.m2
===================================================================
--- m2/integers.m2	(revision 4020)
+++ m2/integers.m2	(working copy)
@@ -21,11 +21,9 @@
 ZZ.mathML = "<mi>&Zopf;</mi>"
 ZZ.frac = QQ
 
-lift = method()
-liftable = method()
-promote = method()
-promote(ZZ,ZZ) := (i,ZZ) -> i
-lift(ZZ,ZZ) := (i,ZZ) -> i
+promote(ZZ,ZZ) := lift(ZZ,ZZ) := (i,ZZ) -> i
+liftable(ZZ,ZZ) := x -> true
+
 ZZ.random = () -> random 21 - 10
 
 oldgcd := gcd
@@ -93,8 +91,6 @@
 
 isConstant ZZ := i -> true
 
-promote(ZZ,ZZ) := (i,o) -> i
-
 -- Local Variables:
 -- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
 -- End:
Index: m2/matrix2.m2
===================================================================
--- m2/matrix2.m2	(revision 4020)
+++ m2/matrix2.m2	(working copy)
@@ -383,17 +383,6 @@
   { map(S,R,vars S * substitute(n, S)), map(R,S,vars R * n^(-1))}
   )
 
-lift(Matrix,Ring) := Matrix => (f,S) -> (
-     -- this will be pretty slow and stupid
-     if ring target f === S then f
-     else if isQuotientOf(ring f,S) and
-	     isFreeModule source f and
-	     isFreeModule target f then
-	 map(S^(-degrees target f), S^(-degrees source f), 
-	     applyTable(entries f, r -> lift(r,S)))
-     else matrix(S, applyTable(entries f, r -> lift(r,S)))
-     )
-
 lift(Ideal,Ring) := Ideal => (I,S) -> (
      -- provisional, just for quotient rings
      T := ring I;
@@ -414,11 +403,11 @@
      F := map(ring M, R2,flatten entries M);
      F D1)
 
--- promote(Matrix,Ring) := (f,S) -> (
+-- promote(Matrix,RingElement) := (f,S) -> (
 --      error "this use of 'promote' has been replaced by '**'";
 --      );
 -- 
--- promote(Ideal,Ring) := (I,S) -> (
+-- promote(Ideal,RingElement) := (I,S) -> (
 --      error "this use of 'promote' has been replaced by '*'";
 --      );
 
Index: m2/reals.m2
===================================================================
--- m2/reals.m2	(revision 4020)
+++ m2/reals.m2	(working copy)
@@ -24,6 +24,17 @@
 
 isConstant RR := i -> true
 
+round = x -> floor(x + 0.5)
+
+promote(RR,RR) := (i,RR) -> i
+promote(QQ,RR) := 
+promote(ZZ,RR) := (i,RR) -> i + 0.
+
+lift(RR,ZZ) := (r,ZZ) -> if r == floor r then floor r else error("can't lift ",toString r, " to ZZ")
+liftable(RR,ZZ) := (r,ZZ) -> r == floor r
+
+lift(RR,QQ) := (r,QQ) -> notImplemented()
+
 RRR.isBasic = true
 RRR#0 = toRRR 0						    -- deceptive, since the precision gets fixed!
 RRR#1 = toRRR 1
@@ -39,14 +50,17 @@
 RRR.Engine = true
 isConstant RRR := i -> true
 
-round = x -> floor(x + 0.5)
+promote(RRR,RRR) := (i,RRR) -> i
+promote(RR,RRR) := 
+promote(QQ,RRR) := 
+promote(ZZ,RRR) := (i,RRR) -> toRRR i
 
-promote(RR,RR) := (i,o) -> i
-promote(ZZ,RR) := (i,o) -> i + 0.
-promote(QQ,RR) := 
-promote(ZZ,RR) := (i,o) -> i + 0.
-promote(RR,Ring) := (r,S) -> promote(r,S#0)
+lift(RRR,ZZ) := (r,ZZ) -> if r == floor r then floor r else error("can't lift ",toString r, " to ZZ")
+liftable(RRR,ZZ) := (r,ZZ) -> r == floor r
 
+lift(RRR,RR) := (r,RR) -> notImplemented()
+lift(RRR,QQ) := (r,QQ) -> notImplemented()
+lift(RRR,ZZ) := (r,ZZ) -> notImplemented()
 
 -- Local Variables:
 -- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
Index: m2/matrix1.m2
===================================================================
--- m2/matrix1.m2	(revision 4024)
+++ m2/matrix1.m2	(working copy)
@@ -655,13 +655,6 @@
      if N.?generators then d = d + getshift N.generators;
      d)
 
-promote(Matrix,ZZ) := (f,ZZ) -> (
-     if ring f === ZZ then f
-     else error "can't promote");
-promote(Matrix,QQ) := (f,QQ) -> (
-     if ring f === QQ then f
-     else matrix applyTable(entries f, r -> promote(r,QQ)));
-
 super(Matrix) := Matrix => (f) -> (
      M := target f;
      if M.?generators then map(super M, M, M.generators) * f
@@ -671,24 +664,22 @@
 isInjective Matrix := (f) -> kernel f == 0
 isSurjective Matrix := (f) -> cokernel f == 0
 
-scan({ZZ}, S -> (
-	  lift(Matrix,S) := (f,S) -> (
-	       -- this will be pretty slow
-	       if ring target f === S then f
-	       else if isQuotientOf(ring f,S) and
-		       isFreeModule source f and
-		       isFreeModule target f then
-		   map(S^(-degrees target f), S^(-degrees source f), 
-		       applyTable(entries f, r -> lift(r,S)))
-	       else matrix(S, applyTable(entries f, r -> lift(r,S)))
-	       );
-	  lift(Ideal,S) := (I,S) -> (
-	       -- this will be pretty slow
-	       if ring I === S then I
-	       else
-		   (ideal lift(generators I,S)) +
-		   ideal (presentation ring I ** S));
-	  ));
+lift(Matrix,Number) := (f,F) -> (
+     -- this will be pretty slow
+     if ring target f === F then f
+     else if isQuotientOf(ring f,F) and
+	     isFreeModule source f and
+	     isFreeModule target f then
+	 map(F^(-degrees target f), F^(-degrees source f), 
+	     applyTable(entries f, r -> lift(r,F)))
+     else matrix(F, applyTable(entries f, r -> lift(r,F)))
+     );
+lift(Ideal,Number) := (I,F) -> (
+     -- this will be pretty slow
+     if ring I === F then I
+     else
+	 (ideal lift(generators I,F)) +
+	 ideal (presentation ring I ** F));
 
 content(RingElement) := Ideal => (f) -> ideal \\ last \ listForm f
 
Index: m2/enginering.m2
===================================================================
--- m2/enginering.m2	(revision 4021)
+++ m2/enginering.m2	(working copy)
@@ -26,9 +26,8 @@
 
 toString EngineRing := R -> if ReverseDictionary#?R then toString ReverseDictionary#R else toString R.RawRing
 
-ZZ _ EngineRing := 
-RR _ EngineRing := 
-promote(ZZ,EngineRing) := RingElement => (i,R) -> new R from i_(R.RawRing)
+Number _ EngineRing := 
+promote(Number,RingElement) := RingElement => (i,R) -> new R from rawFromNumber(raw R, i)
 
 new RingElement from RawRingElement := (R, f) -> new R from { symbol RawRingElement => f };
 
@@ -39,8 +38,6 @@
      S#0 = 0_S;
      S)
 
-promote(RR,EngineRing) := RingElement => (i,R) -> i_R
-
 -----------------------------------------------------------------------------
                 FractionField = new Type of EngineRing
 		FractionField.synonym = "fraction field"
@@ -406,108 +403,24 @@
 
 -----------------------------------------------------------------------------
 
--- new lift and promote, version 2
+basicLift = (r,B) -> new B from rawLift(raw B, raw r)
+multipleBasicLift = (r,v) -> ( r = raw r; scan(v, B -> r = rawLift(raw B, raw r)); new last v from r )
 
-liftChain := (R,A) -> (
-     -- how to lift from R to A, assuming A is a precursor of R
-     if R === A then ()
-     else (
-	  S := R;
-	  while S =!= A and class S === QuotientRing do S = ambient S;
-	  if S === A then 1 : S
-	  else (
-	       if S.?baseRings then S = last S.baseRings;
-	       if S === A then 1 : S
-	       else if R === S then error "no lifting possible for these rings"
-	       else prepend(S, liftChain(S, A)))))
+basicLiftMatrix = (m,F) -> map(F,, rawLift(raw F, raw m))
+multipleBasicLiftMatrix = (m,v) -> (m = raw m; scan(v, F -> m = rawLift(raw F, m)) map(last v,,m) )
 
-promoteChain := (A,R) -> (
-     -- how to promote from A to R, assuming A is a precursor of R
-     if R === A then ()
-     else append((
-	       S := R;
-	       while S =!= A and class S === QuotientRing do S = ambient S;
-	       if S === A then ()
-	       else (
-	       	    if class S === PolynomialRing 
-	       	    or class S === GaloisField
-	       	    or class S === FractionField
-		    then S = last S.baseRings;
-		    if S === A then ()
-		    else if R === S then error "no promotion possible for these rings"
-		    else promoteChain(A, S))),
-	  R))
+basicPromote = (r,B) -> new B from rawPromote(raw B, raw r)
+multipleBasicPromote = (r,v) -> ( r = raw r; scan(v, B -> r = rawPromote(raw B, raw r)); new last v from r )
 
-eeLift := (B,r) -> new B from rawLift(raw B, raw r)
+basicPromoteMatrix = (m,F) -> map(F,, rawPromote(raw F, raw m))
+multipleBasicPromoteMatrix = (m,v) -> (m = raw m; scan(v, F -> m = rawPromote(raw F, m)) map(last v,,m) )
 
-lift(RingElement, RingElement) := RingElement =>
-lift(RingElement, ZZ) :=
-lift(RingElement, QQ) := (r,o) -> (
-     R := class r;
-     A := class o;
-     if R === A then (
-	  lift(R,A) := (r,o) -> r
-	  )
-     else (
-	  c := liftChain(R,A);
-	  lift(R,A) := (r,o) -> (
-	       scan(c, B -> r = eeLift(B,r));
-	       r)
-	  );
-     lift(r,o))
+-- now done in the engine
+-- promote(QQ, RingElement) := RingElement => (r,S) -> (
+--      a := promote(numerator r,S);
+--      b := promote(denominator r,S);
+--      if isField S then a/b else if a % b == 0 then a // b else error ("conversion from QQ to ", toString S, " not possible"))
 
-lift(RingElement,Ring) := RingElement => 
-lift(ZZ,Ring) :=
-lift(QQ,Ring) := (r,A) -> lift(r,A#0)
-
-promote(QQ, RingElement) := RingElement => (r,o) -> (
-     S := class o;
-     if member(QQ,S.baseRings) then (
-	  c := promoteChain(QQ,S);
-	  promote(QQ,S) := (f,o) -> (
-	       f = raw f;
-	       scan(c, S -> f = rawPromote(S.RawRing,f));
-	       new S from f)
-	  )
-     else (
-	  promote(QQ,S) := (r,S) -> (
-	       a := promote(numerator r,S);
-	       b := promote(denominator r,S);
-	       if isField class S then (
-		    a/b
-		    )
-	       else (
-	       	    if a % b == 0 then a // b
-	       	    else error "division not possible"
-		    )
-	       );
-	  );
-     promote(r,S))
-
-promote(RingElement, RingElement) := RingElement => (r,o) -> (
-     R := class r;
-     S := class o;
-     if R === S then (
-	  promote(R,S) := (r,o) -> r
-	  )
-     else (
-	  c := promoteChain(R,S);
-	  promote(R,S) := (f,o) -> (
-	       f = raw f;
-	       scan(c, S -> f = rawPromote(S.RawRing,f));
-	       new S from f)
-	  );
-     promote(r,S))
-
-promote(ZZ,RingElement) := (i,o) -> promote(i, ring o)
-
-promote(RingElement,Ring) := RingElement => (r,S) -> promote(r,S#0)
-promote(ZZ,Ring) := (r,S) -> promote(r,S#0)
-
-liftable(RingElement,Ring) := Boolean => 
-liftable(ZZ,Ring) := 
-liftable(QQ,Ring) := (f,R) -> try (lift(f,R);true) else false
-
 isUnit(RingElement) := (f) -> 1 % ideal f == 0
 
 Ring _ String := RingElement => (x,s) -> x.indexStrings#s
@@ -518,12 +431,6 @@
      else error "no method found for item of class Ring"
      )
 
-ZZ _ Ring := RingElement => (i,R) -> (
-     if i === 1 then R#1
-     else if i === 0 then R#0
-     else i * R#1
-     )
-
 isConstant RingElement := r -> r == 0 or all(degree r, i -> i === 0)
 
 -- Local Variables:
Index: m2/engine.m2
===================================================================
--- m2/engine.m2	(revision 4024)
+++ m2/engine.m2	(working copy)
@@ -190,10 +190,9 @@
 RawRingElement == RawRingElement := (x,y) -> x === y
 
 RawRing _ ZZ := (R,n) -> rawRingVar(R,n)
-ZZ _ RawRing := (n,R) -> rawFromNumber(R,n)
 raw RR := x -> x _ (RR.RawRing)
-RR _ RawRing := (n,R) -> rawFromNumber(R,n)
-RRR _ RawRing := (n,R) -> rawFromNumber(R,n)
+Number _ RawRing := (n,R) -> rawFromNumber(R,n)
+
 RawRingElement _ RawRing := (x,R) -> rawPromote(R,x)
 
 RawRingElement == RawRingElement := (x,y) -> x === y
Index: m2/modules.m2
===================================================================
--- m2/modules.m2	(revision 4020)
+++ m2/modules.m2	(working copy)
@@ -183,6 +183,17 @@
 source Matrix := f -> f.source
 target Matrix := f -> f.target
 
+lift(Matrix,RingElement) := lift(Matrix,Number) := Matrix => (f,S) -> lift(f, ring f, S)
+liftable(Matrix,RingElement) := liftable(Matrix,Number) := Boolean => (f,S) -> liftable(f, ring f, S)
+promote(Matrix,RingElement) := promote(Matrix,Number) := Matrix => (f,S) -> promote(f, ring f, S)
+
+lift(Matrix,QQ,ZZ) := (f,ZZ,QQ) -> basicLiftMatrix(f,QQ^(dim target f))
+
+promote(Matrix,Number,Number) := promote(Matrix,Number,RingElement) := (f,A,R) -> basicPromoteMatrix(f,R^(rank target f))
+lift(Matrix,Number,Number) := lift(Matrix,RingElement,Number) := (f,R,A) -> (
+     if not isFreeModule source f or not isFreeModule target f then error "expected source and target to be free";
+     basicLiftMatrix(f,R^(rank target f)))
+
 Vector = new Type of BasicList				    -- an instance v will have one entry, an n by 1 matrix m, with class v === target m
 Vector.synonym = "vector"
 Vector _ ZZ := (v,i) -> (ambient v#0)_(i,0)
Index: m2/rings.m2
===================================================================
--- m2/rings.m2	(revision 4020)
+++ m2/rings.m2	(working copy)
@@ -56,6 +56,12 @@
      degreeLength R == 0 
      )
 
+promote = method(Dispatch=>{Input,Output,Output})
+lift = method(Dispatch=>{Input,Output,Output})
+liftable = method(Dispatch=>{Input,Output,Output}, TypicalValue => Boolean)
+
+Number _ Ring := (n,R) -> promote(n,R)
+
 -- Local Variables:
 -- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
 -- End:
Index: m2/ofcm.m2
===================================================================
--- m2/ofcm.m2	(revision 4024)
+++ m2/ofcm.m2	(working copy)
@@ -369,17 +369,11 @@
 
 -- delayed installation of methods for monoid elements
 
-promote(MonoidElement, Ring) := RingElement => (m,R) -> promote(m,R#0)
-promote(MonoidElement, RingElement) := RingElement => (m,o) -> (
-     R := class o;
+promote(MonoidElement, RingElement) := RingElement => (m,R) -> (
      M := monoid R;
      k := coefficientRing R;
      if not instance(m,M) then error "expected monomial from same ring";
-     one := 1_k;
-     promote(M,R) := (m,o) -> new R from rawTerm(R.RawRing, 
-	                                         raw one,
-						 m.RawMonomial);
-     promote(m,o))
+     new R from rawTerm(R.RawRing, raw 1_k, m.RawMonomial))
 
 -- Local Variables:
 -- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
Index: packages/Macaulay2/doc2.m2
===================================================================
--- packages/Macaulay2/doc2.m2	(revision 4024)
+++ packages/Macaulay2/doc2.m2	(working copy)
@@ -978,7 +978,7 @@
 	  TO (generators, Ring),
 	  TO (numgens, Ring),
 	  TO (symbol _, Ring, ZZ),
-	  TO (symbol _, ZZ, Ring)
+	  TO (symbol _, Number, Ring)
 	  },
      "Common ways to get information about a ring:",
      UL {
Index: packages/Macaulay2/functions/lift-doc.m2
===================================================================
--- packages/Macaulay2/functions/lift-doc.m2	(revision 4020)
+++ packages/Macaulay2/functions/lift-doc.m2	(working copy)
@@ -3,15 +3,14 @@
 --- notes: BUG to fix
 
 undocumented {
-	  (lift,Ideal,ZZ),
-	  (lift,ZZ,Ring),
-	  (lift,QQ,Ring),
-	  (lift,ZZ,ZZ),
-	  (lift,QQ,ZZ),
-	  (lift,QQ,QQ),
-	  (lift,Matrix,ZZ),
-	  (lift,RingElement,ZZ),
-	  (lift,RingElement,QQ)}
+     (lift, Matrix, Number, Number),
+     (lift, Matrix, QQ, ZZ),
+     (lift, ZZ, ZZ),
+     (lift, QQ, ZZ),
+     (lift, RRR, ZZ), (lift, QQ, QQ), (lift, RRR, QQ),
+     (lift, Matrix, RingElement, Number),
+     (lift, RR, ZZ), (lift, RR, QQ), (lift, RRR, RR)
+     }
 
 document { 
      Key => {lift,
Index: packages/Macaulay2/functions/symbol-underscore-doc.m2
===================================================================
--- packages/Macaulay2/functions/symbol-underscore-doc.m2	(revision 4020)
+++ packages/Macaulay2/functions/symbol-underscore-doc.m2	(working copy)
@@ -1,7 +1,6 @@
 undocumented {
-     (symbol _, ZZ, EngineRing),
+     (symbol _, Number, EngineRing),
      (symbol _, EngineRing, ZZ),
-     (symbol _, RR, EngineRing),
      (symbol _, ZZ, Monoid),
      (symbol _, Monoid, ZZ),
      (symbol _, Symbol, GeneralOrderedMonoid),

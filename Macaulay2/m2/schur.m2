--		Copyright 1996 by Daniel R. Grayson

SchurRing = new Type of EngineRing

name SchurRing := S -> "Schur(" | string (# (monoid S).generatorSymbols) | ")"

newSchur := (R,M) -> (
     if not (M.?Engine and M.Engine) 
     then error "expected ordered monoid handled by the engine";
     if not (R.?Engine and R.Engine) 
     then error "expected coefficient ring handled by the engine";
     SR := new SchurRing from (ggPush R, ggPush M, ggschur);
     SR.baseRings = append(R.baseRings,R);
     if R.?newEngine or M.?newEngine then SR.newEngine = true;
     ONE := SR#1;
     if degreeLength M != 0 then (
	  -- there must be something smarter to do, but if we
	  -- do not do this, then we get into an infinite loop
	  -- because each monoid ring ZZ[a,b,c] needs its degrees ring
	  -- ZZ[t], which in turn needs to make its degrees ring 
	  -- ZZ[], which in turn needs one.
	  SR.degreesRing = degreesRing degreeLength M;
	  )
     else (
	  SR.degreesRing = ZZ;
	  );
     if R.?char then SR.char = R.char;
     SR.monoid = M;
     SR ? SR := (f,g) -> (
	  if f == g then quote ==
	  else leadMonomial f ? leadMonomial g
	  );
     R * M := (r,m) -> (
	  sendgg(ggPush SR, ggPush r, ggPush m, ggterm);
	  new SR);
     M * R := (m,r) -> (
	  sendgg(ggPush SR, ggPush r, ggPush m, ggterm);
	  new SR);
     SR * M := (p,m) -> p * (R#1 * m);
     M * SR := (m,p) -> (R#1 * m) * p;
     R + M := (r,m) -> r * M#1 + R#1 * m;
     M + R := (m,r) -> r * M#1 + R#1 * m;
     SR + M := (p,m) -> p + R#1 * m;
     M + SR := (m,p) -> p + R#1 * m;
     R - M := (r,m) -> r * M#1 - R#1 * m;
     M - R := (m,r) -> R#1 * m - r * M#1;
     SR - M := (p,m) -> p - R#1 * m;
     M - SR := (m,p) -> R#1 * m - p;
     SR.ConvertToExpression = ConvertApply(
	  args -> (
	       if # args === 1 
	       then args#0
	       else Sum toList args
	       ),
	  ConvertRepeat ConvertApply ( 
	       (m,r) -> r * m,
	       ConvertJoin(M.ConvertToExpression, R.ConvertToExpression)));
     expression SR := f -> convert(
	  SR.ConvertToExpression,
	  sendgg(ggPush f, ggtonet)
	  );
     SR.generators = apply(M.generators, m -> SR#(name m) = SR#0 + m);
     scan(keys R,k -> if class k === String then SR#k = promote(R#k,SR));
     SR.use = x -> (
	  M + M := (m,n) -> R#1 * m + R#1 * n;
	  M - M := (m,n) -> R#1 * m - R#1 * n;
	  - M := (m,n) -> - R#1 * n;
	  scan(SR.baseRings, A -> (
	       if A =!= R then (
		    A * M := (i,m) -> (i * R#1) * m;
		    M * A := (m,i) -> m * (i * R#1);
		    );
	       A + M := (i,m) -> i * R#1 + m;
	       M + A := (m,i) -> m + i * R#1;
	       A - M := (i,m) -> i * R#1 - m;
	       M - A := (m,i) -> m - i * R#1;
	       M / A := (m,r) -> (m * ONE) / (r * ONE);
	       M % A := (m,r) -> (m * ONE) % (r * ONE);
	       ));
	  SR);
     SR
     )

ck := i -> if i < 0 then error "expected decreasing row lengths" else i

SchurRing _ List := (S,a) -> (
     M := (monoid S).generators;
     m := (
	  if # a === 0 
	  then 1_M
	  else product(# a, i -> (M#i) ^ (
		    ck
	       	    if i+1 < # a 
	       	    then a#i - a#(i+1)
	       	    else a#i)));
     sendgg(ggPush S, ggPush 1, ggPush m, ggterm);
     new S)

Schur = method ( Options => { } )

Schur(ZZ) := options -> n -> (
     R := ZZ;
     x := quote x;
     prune := v -> drop(v, - # select(v,i -> i === 0));
     M := monoid[x_1 .. x_n];
     vec := apply(n, i -> apply(n, j -> if j<=i then 1 else 0));
     name M := net M := x -> first lines sendgg(ggPush x, ggsee, ggpop);
     M.ConvertToExpression = ConvertApply(
	  args -> new Holder from {name (
	       if args === ()
	       then {}
	       else prune sum(args,(v,n) -> n * vec#v)
	       )},
	  ConvertRepeat ConvertJoin(
	       ConvertInteger,	  -- index of variable
	       ConvertInteger	  -- exponent on variable
	       ));
     S := newSchur(R,M);
     dim S := s -> (
	  sendgg(ggPush s, ggdim);
	  R.pop());
     S
     )

document { quote Schur,
     TT "Schur n", " -- creates a Schur ring of degree n.",
     PARA,
     "This is the representation ring for the general linear group of n by n
     matrices.",
     PARA,
     SEEALSO {"SchurRing"}
     }

document { quote SchurRing,
     TT "SchurRing", " -- the class of all Schur rings.",
     PARA,
     "A Schur ring is the representation ring for the general linear group of 
     n by n matrices, and one can be constructed with ", TO "Schur", ".",
     EXAMPLE "R = Schur 4",
     "The element corresponding to the Young diagram ", TT "{3,2,1}", " is
     obtained as follows.",
     EXAMPLE "R_{3,2,1}",
     "The dimension of the underlying virtual representation can be obtained
     with ", TO "dim", ".",
     EXAMPLE "dim R_{3,2,1}",
     "Multiplication in the ring comes from tensor product of representations.",
     EXAMPLE "R_{3,2,1} * R_{1,1}",
     SEEALSO {quote _, SchurRing, List}
     }

document { (quote _, SchurRing, List),
     TT "S_v", " -- produce the element of the Schur ring ", TT "S", " corresponding
     to the Young diagram whose rows have lengths as in the list ", TT "v", ".",
     PARA,
     "The row lengths should be in decreasing order.",
     SEEALSO "SchurRing"
     }

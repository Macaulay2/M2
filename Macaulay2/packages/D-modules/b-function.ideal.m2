-----------------------------------------------------------------------
-- bFunction (I, w) -> bf
-- I = holonomic ideal in a Weyl algebra with no parameters
-- w = weight
-- bf = b-function of I with respect to w (polynomial 
-- 	     in K[$s], where K is the coefficient ring)
--
-- (method: algorithms 5.1.5 and 5.1.6 in Saito-Strumfels-Takayama)
-----------------------------------------------------------------------

 
bFunction = method(Options => {Strategy => TryGeneric})

-- makes a polinomial monic (internal) 
makeMonic := f -> ( (1 / (leadCoefficient f)) * f );

-- lifts the coeffs of a polynomial to QQ,
-- returns error if QQ is not a subring of a coefficient ring
-- (internal)
makeQQ := f -> (
     s := symbol s;
     R := QQ[s];
     if not member(QQ, R.baseRings) then
     error "QQ is not a base ring of R";
     if f == 0 then 0_R 
     else sum(listForm f, u -> lift(u#1, QQ) * s^(sum u#0))
     );

-- trivial intersection strategy (internal)
bfTrivialIntersection := method()
bfTrivialIntersection(Ideal, List) :=  (I, w) -> (
     local tInfo;
     -- prep work
     if ((ring I).?IntRing == false) then
     createIntRing (ring I);
     if ((ring I).?dpairVars == false) then
     createDpairs (ring I);

     W := ring I;
     IR := W.IntRing;
     dpV := W.dpairVars;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(dpI#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if (#w != ((numgens W) // 2)) then
     error "expected weight vector of length " | ((numgens W) // 2);
     
     w = apply(numgens W, i -> (
	       p := position(dpI#1, u -> u == i); 
	       if p =!= null  then w#p
	       else (
		    p = position(dpI#0, u -> u == i);
		    -w#p
		    )  
	       ));
     
     -- compute in_(-w,w) (I)   
     inI := inw(I, w);
               
     n := #(dpI#0);
     eulerOp := sum(n, i -> w_(dpI#1#i)*(dpV#0#i)*(dpV#1#i));
     elimIdeal := W.RtoIR inI + ideal (W.RtoIR eulerOp - IR_(numgens(IR) - 1));
     pInfo(2, "computing elimIdealGB... ");  
     tInfo = toString first timing(
	  elimIdealGB := gens gb elimIdeal;
	  );
     pInfo(3, " elimIdealGB = " | toString elimIdealGB); 
     pInfo(2, " time = " | tInfo);
     
     -- take the generator of J and cook up the b-function 
     bGen := selectInSubring(1,elimIdealGB);
     bfcn := (
	  if numgens source bGen == 0 then 0_T 
     	  else makeMonic (mingens ideal bGen)_(0,0)
	  );
     makeQQ bfcn     	
     );

-- TryGeneric or NonGeneric strategy (internal)
bfGenericOrNonGeneric := method(Options => {Strategy => TryGeneric})
bfGenericOrNonGeneric(Ideal, List) := o -> (I, w) -> (
     local tInfo;
     -- prep work
     if ((ring I).?ThetaRing == false) then
     createThetaRing (ring I);
     
     if ((ring I).ThetaRing.?IntRing == false) then
     createIntRing (ring I).ThetaRing;
     
     if ((ring I).?dpairVars == false) then
     createDpairs (ring I);

     W := ring I;
     T := W.ThetaRing;
     TI := T.IntRing;
     dpV := W.dpairVars;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(dpI#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if (#w != ((numgens W) // 2)) then
     error ("expected weight vector of length " | ((numgens W) // 2));
     
     w = apply(numgens W, i -> (
	       p := position(dpI#1, u -> u == i); 
	       if p =!= null  then w#p
	       else (
		    p = position(dpI#0, u -> u == i);
		    -w#p
		    )  
	       ));
     
     -- compute in_(-w,w) (I)   
     inI := inw(I, w);	    
     
     n := #(dpI#0);
     eulerOp := sum(n, i -> w_(dpI#1#i)*(dpV#0#i)*(dpV#1#i));
     	  
     -- two different strategies can be used 
     local intIdeal;
     if (o.Strategy == TryGeneric) and 
     (-- "isGeneric" check
	  pInfo(2, "'isGeneric' check... ");  
     	  tInfo = toString first timing(
	       isIt := all(flatten entries gens inI, W.isGeneric)
	       );
	  pInfo(2, " time = " | tInfo);
	  isIt
     	  ) 
     then(
	  -- GENERIC
	  pInfo(1, "b-function: Using GENERIC strategy... ");  
	  intIdeal = inI;
	  ) 
     else(     
	  -- NON-GENERIC
	  pInfo(1, "b-function: Using NON-GENERIC strategy... ");  
	  pInfo(2, "Calculating intIdeal... ");  
	  tInfo = toString first timing(
	  dpI' := {select(dpI#0, i -> w#i != 0), select(dpI#1, i -> w#i != 0)};
	  dpI'' := {select(dpI#0, i -> w#i == 0), select(dpI#1, i -> w#i == 0)};
	  -- want: eliminate all u_i, v_i as well as x_i, dx_i of weight 0
	  u := symbol u;
	  v := symbol v;	  
	  UV := (coefficientRing W)[ (dpI'#0) / (i -> u_i), (dpI'#1) / (i -> v_i), 
	       (dpI''#0) / (i -> W_i), (dpI''#1) / (i -> W_i),
	       (dpI'#0) / (i -> W_i), (dpI'#1) / (i -> W_i),
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       MonomialOrder => Eliminate (2 * #dpI#0) ];
	  WtoUV := map(UV, W, matrix { apply(numgens W, i -> (
			      if member(i, dpI'#0) then 
			      (u_i * substitute(W_i, UV))
			      else if member(i, dpI'#1) then (substitute(W_i, UV) * v_i)
			      else substitute(W_i, UV)
			      )) 
		    });
	  intGB := gens gb ((WtoUV inI) 
	       + ideal apply(#dpI'#0, i -> (u_(dpI'#0#i) * v_(dpI'#1#i) - 1)
		    )); 
	  intIdeal = ideal substitute(selectInSubring(1, intGB), W);
	  );
     	  pInfo(2, " time = " | tInfo);
	  );
     pInfo(3, " intIdeal = " | toString intIdeal); 
     
     -- compute J = intIdeal \cap K[\theta]
     pInfo(2, "computing elimIdealGB... ");  
     use T;
     tInfo = toString first timing(
	  elimIdeal := (T.RtoIR  ideal (first entries gens intIdeal / W.WtoT ))
     	  + ideal(TI_(numgens TI - 1) - T.RtoIR W.WtoT eulerOp);
     	  elimIdealGB := gens gb elimIdeal;
	  );
     pInfo(3, " elimIdealGB = " | toString elimIdealGB); 
     pInfo(2, " time = " | tInfo);
     -- take the generator of J and cook up the b-function 
     bGen := selectInSubring(1,elimIdealGB);
     bfcn := (
	  if numgens source bGen == 0 then 0_T 
     	  else makeMonic (mingens ideal bGen)_(0,0)
	  );
     makeQQ bfcn     	
     );

bFunction(Ideal, List) := o -> (I, w) -> (
     result := (
	  if o.Strategy == TrivialIntersection then bfTrivialIntersection(I, w)
     	  else if o.Strategy == TryGeneric or o.Strategy == NonGeneric 
     	  then bfGenericOrNonGeneric(I, w, o)
     	  else error "wrong Strategy option"
	  );
     use ring I;
     result
     );

-----------------------------------------------------------------------
-- globalBFunction (f) -> bf
-- f = polynomial (assumed to be an element of 
-- 	     a Weyl algebra with no parameters)
-- bf = global b-function (polynomial in K[s], where K is 
-- 	     the coefficient field)
--
-- (method: definition 5.3.10 in Saito-Strumfels-Takayama)
-----------------------------------------------------------------------
globalBFunction = method(Options => {Strategy => TryGeneric});
globalBFunction(RingElement) := o -> f -> (
     W := ring f;
     if (W.?dpairVars == false) then
     createDpairs W;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(W.dpairInds#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if any(listForm f, m -> any(dpI#1, i -> m#0#i != 0)) then
     error "expected no differentials in the polynomial";
     
     t := symbol t;
     dt := symbol dt;     
     WT := (coefficientRing W)[ t, dt, (entries vars W)#0, 
	       WeylAlgebra => W.monoid.Options.WeylAlgebra | {t => dt}];
     w := {1} | toList (((numgens W) // 2):0);
     f' := substitute(f,WT);
     If := ideal ({t - f'} 
     	  | (dpI#1 / (i->(
	       	    	 DX := WT_(i+2);
	       	    	 (DX * f' - f' * DX) * dt + DX
	       	    	 )))
	  );
     pInfo(666, toString If);
     bfunc := bFunction(If, w, Strategy => o.Strategy);
     s := (ring bfunc)_0;
     result := makeMonic substitute(bfunc, { s => -s - 1 });
     use W;
     result
     );--end of globalBFunction

-- factors a b-function
factorBFunction = method()
factorBFunction(RingElement) := f -> (
     R := ring f;
     
     -- sanity check
     if numgens R != 1 then
     error "polynomial ring of one variable expected";
     if coefficientRing R != QQ then
     error "expected polynomial over QQ";
     
     l := listForm f;
     d := product(l, u -> denominator(u#1));
     l = l / (u -> (u#0, lift(u#1*d, ZZ)));
     R' := ZZ[R_0];
     f = sum (l, u -> u#1*R'_(u#0));
     f = factor f;
     
     result := apply(drop(f,-1), u->(
	       if first degree u#0 != 1 then error "incorrect b-function";
	       coeff := listForm u#0 / (v->v#1);
	       Power(R_0 + (if #coeff> 1 then (coeff#1/coeff#0) else 0), u#1)
	       ));
     use R;
     result
     );-- end factorBFunction

-- named after what it does
getIntRoots = method()
getIntRoots(RingElement) := f -> (
     p := factorBFunction f;
     roots := apply(toList p, 
	  u -> -substitute(u#0, {(ring u#0)_0 => 0_(ring u#0)}) ); -- all roots
     roots = select(roots, u -> denominator leadCoefficient u == 1);
     apply(roots, u -> numerator leadCoefficient u)    
     );-- end getIntRoots

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes a Bernstein operator for a polynomial f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
globalB = method()

globalB(Module, Matrix, RingElement) := (M, u, f) -> (
     W := ring M;
     F := map(M, W^1, u);
     tempI := ideal kernel F;
     globalB(tempI,f)
     )

globalB(Ideal, RingElement) := (I, f) -> (
     W := ring I;
     AnnI := AnnIFs (I,f);
     Ws := ring AnnI;
     ns := numgens Ws;
     
     elimWs := (coefficientRing Ws)[(entries vars Ws)#0,
	  WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate (ns-1)];
     ff := substitute(f,elimWs);
     elimAnnI := substitute(AnnI, elimWs);
     H := gens elimAnnI | matrix{{ff}};
     
     gbH := gb(H, ChangeMatrix => true);

     bpolys := selectInSubring(1, gens gbH);
     if (bpolys == 0) then error "module not specializable";
     if (rank source bpolys > 1) then error "ideal principal but not
     realized as such.  Need better implementation";
     bpoly := bpolys_(0,0);

     ind := position((entries gens gbH)#0, i -> (i == bpoly));
     C := getChangeMatrix gbH;
     Bop := C_(numgens source H - 1, ind);
     
     Ks = (coefficientRing W)[Ws_(ns-1)];
     bpoly = substitute(bpoly, Ks);
     use W;
     hashTable {Bpolynomial => bpoly, Boperator => Bop}
     )

globalBoperator = method()
globalBoperator(RingElement) := (f) -> (
     W := ring f;
     if (W.?dpairVars == false) then
     createDpairs W;
     I := ideal W.dpairVars#1;
     (globalB(I,f))#Boperator
     )
   













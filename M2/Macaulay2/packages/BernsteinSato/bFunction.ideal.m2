-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

-----------------------------------------------------------------------
-- bFunction (I, w) -> bf
-- I = holonomic ideal in a Weyl algebra with no parameters
-- w = weight
-- bf = b-function of I with respect to w (polynomial 
-- 	     in K[$s], where K is the coefficient ring)
--
-- (method: algorithms 5.1.5 and 5.1.6 in Saito-Strumfels-Takayama)
-----------------------------------------------------------------------

bFunction = method(Options => {Strategy => IntRing})

-- makes polynomial f monic (internal) 
makeMonic := f -> ( if coefficientRing ring f === QQ 
     then (1 / (leadCoefficient f)) * f 
     else (1 // (leadCoefficient f)) * f
     );

-- lifts the coeffs of a polynomial to QQ,
-- returns error if QQ is not a subring of a coefficient ring
-- (internal)
makeQQ := f -> (
     local R;
     s := symbol s;
     if ring f === QQ or f === 0 then (
	  R = QQ[s];
	  if not member(QQ, R.baseRings) then
	  error "QQ is not a base ring of R";
	  if f == 0 then 0_R 
	  else sum(listForm f, u -> lift(u#1, QQ) * s^(sum u#0))
	  )
     else (
	  R = (coefficientRing ring f)[s];
	  if f == 0 then 0_R 
	  else sum(listForm f, u -> u#1 * s^(sum u#0))
	  )
     );

-- trivial intersection strategy (internal)
bfIntRing := method()
bfIntRing(Ideal, List) :=  (I, w) -> (
     local tInfo;
     -- prep work
     if not (ring I).?IntRing then
     createIntRing (ring I);
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
	  if numgens source bGen == 0 then 0 
     	  else makeMonic (mingens ideal bGen)_(0,0)
	  );
     makeQQ bfcn     	
     );

-- TryGeneric or NonGeneric strategy (internal)
bfGenericOrNonGeneric := method(Options => {Strategy => TryGeneric})
bfGenericOrNonGeneric(Ideal, List) := o -> (I, w) -> (
     local tInfo;
     -- prep work
     if not (ring I).?ThetaRing then
     createThetaRing (ring I);
     
     if not (ring I).ThetaRing.?IntRing then
     createIntRing (ring I).ThetaRing;
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
	  UV := (coefficientRing W)(monoid [ (dpI'#0) / (i -> u_i), (dpI'#1) / (i -> v_i), 
	       (dpI''#0) / (i -> W_i), (dpI''#1) / (i -> W_i),
	       (dpI'#0) / (i -> W_i), (dpI'#1) / (i -> W_i),
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       MonomialOrder => Eliminate (2 * #dpI#0) ]);
	  WtoUV := map(UV, W, matrix { apply(numgens W, i -> (
			      if member(i, dpI'#0) then 
			      (UV_(u_i) * substitute(W_i, UV))
			      else if member(i, dpI'#1) then (substitute(W_i, UV) * UV_(v_i))
			      else substitute(W_i, UV)
			      )) 
		    });
	  intGB := gens gb ((WtoUV inI) 
	       + ideal apply(#dpI'#0, i -> (UV_(u_(dpI'#0#i)) * UV_(v_(dpI'#1#i)) - 1)
		    )); 
	  intIdeal = ideal substitute(selectInSubring(1, intGB), W);
	  );
     	  pInfo(2, " time = " | tInfo);
	  );
     pInfo(3, " intIdeal = " | toString (intIdeal)); 
     
     -- compute J = intIdeal \cap K[\theta]
     pInfo(2, "computing elimIdealGB... ");  
     intIdealGens := first entries gens intIdeal;
     tInfo = toString first timing(
	  elimIdeal := (T.RtoIR  (if #intIdealGens==0 then ideal T 
		    else ideal ( intIdealGens/ W.WtoT )))
     	  + ideal(TI_(numgens TI - 1) - T.RtoIR W.WtoT eulerOp);
     	  elimIdealGB := gens gb elimIdeal;
	  );
     pInfo(3, " elimIdealGB = " | toString elimIdealGB); 
     pInfo(2, " time = " | tInfo);
     -- take the generator of J and cook up the b-function 
     bGen := selectInSubring(1,elimIdealGB);
     bfcn := (
	  if numgens source bGen == 0 then 0_TI
     	  else makeMonic (mingens ideal bGen)_(0,0)
	  );
     makeQQ bfcn     	
     );

bFunction(Ideal, List) := RingElement => o -> (I, w) -> (
     result := (
	  if o.Strategy == IntRing then bfIntRing(I, w)
     	  else if o.Strategy == TryGeneric or o.Strategy == NonGeneric 
     	  then bfGenericOrNonGeneric(I, w, o)
     	  else error "wrong Strategy option"
	  );
     result
     );

-- factors a b-function
factorBFunction = method()
factorBFunction(RingElement) := Product => f -> (
     R := ring f;
     
     -- sanity check
     if numgens R != 1 then
     error "polynomial ring of one variable expected";
     if coefficientRing R =!= QQ then
     error "expected polynomial over QQ";
     
     l := listForm f;
     d := product(l, u -> denominator(u#1));
     l = l / (u -> (u#0, lift(u#1*d, ZZ)));
     R' := ZZ(monoid [R_0]);
     f = sum (l, u -> u#1*R'_(u#0));
     f = factor f;
     f = select(f, u-> first degree u#0 > 0);
     
     result := apply(f, u->(
	       if first degree u#0 != 1 then error "internal error: incorrect b-function";
	       coeff := listForm u#0 / (v->v#1);
	       Power(R_0 + (if #coeff> 1 then (coeff#1/coeff#0) else 0), u#1)
	       ));
     if #result==0 then 1_R' else result
     );-- end factorBFunction

bFunctionRoots = method()
bFunctionRoots RingElement := List => f -> (
     if f==1 then {} else apply(toList factorBFunction f, 
	 u -> - leadCoefficient substitute(u#0, {(ring u#0)_0 => 0_(ring u#0)}) )
     );
getIntRoots = method()
getIntRoots RingElement := List => f -> (
     roots := bFunctionRoots f;
     roots = select(roots, u -> denominator u == 1);
     apply(roots, u -> numerator u)    
     );-- end getIntRoots

 
TEST ///
Dtrace 1
pInfo(1, "testing globalBFunction...")

for str in {IntRing, TryGeneric, NonGeneric, GeneralBernsteinSato} do (
     	  print str;
	  x = symbol x; dx = symbol dx; 
	  R = QQ[x, dx, WeylAlgebra => {x=>dx}];
	  n = 10;
	  f = x^n;     	    	 
	  b = globalBFunction(f, Strategy => str);
	  assert ((n^n * b) == ( 
		    use ring b;
		    s := (ring b)_0;
		    product(n, i -> n * (s + 1) - i)       
		    ))
	  );
	  
clearAll()
pInfo(1, "testing generalB...")
for str in {InitialIdeal, StarIdeal} do (
     	  pInfo(1, "Strategy=>" | toString str);
	  R = QQ[x_1..x_4];
	  F = {x_3*x_1^2 + x_4*x_2^3};
	  b = {1_R,x_1,x_2} / (g->toString factorBFunction generalB (F,g,Strategy=>str));
	  assert(toString b == "{(s+1)*(s+2)*(s+3/2)*(s+4/3)*(s+5/3)*(s+5/6)*(s+7/6), (s+1)*(s+2)*(s+5/2)*(s+4/3)*(s+5/3)*(s+11/6)*(s+13/6), (s+1)*(s+2)*(s+3/2)*(s+5/3)*(s+7/3)*(s+7/6)*(s+11/6)}")
	  );

assert(toString factorBFunction generalB (F,1_R,Exponent=>2) == "(s+1)*(s+2)^2*(s+3)*(s+3/2)*(s+5/2)*(s+4/3)*(s+5/3)*(s+7/3)*(s+8/3)*(s+5/6)*(s+7/6)*(s+11/6)*(s+13/6)")
///

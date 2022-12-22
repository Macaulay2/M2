trackProjectiveCertified = method()
trackProjectiveCertified (List,List,List) := List => (S,T,solsS) -> (
-- tracks solutions from start system to target system (robust certified)
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- OUT: solsT = list of target solutions corresponding to solsS
     HISTORY := DBG>1;
     if #T > 0 then R := commonRing T else error "expected nonempty target system";
     if #S != #T then 
     error "expected same number of polynomials in start and target systems";
     if any(S, f->ring f =!= R) or any(T, f->ring f =!= R)
     then error "expected all polynomials in the same ring";
     n := numgens R; 
     if not(n == #T+1 
	  -- and all(T, isHomogeneous) and all(S, isHomogeneous) -- bug in isHomogeneous!!!
	  ) 
     then error "expected n equations in n+1 variables";
     deg := T/first@@degree;
     if S/first@@degree != deg then error "degrees of start and target systems do not match";
     
     -- M2 (main code)  --------------------------------------------------------     
     setupStartTime := currentTime();
     -- thresholds and other tuning parameters (should include most of them as options)
     theSmallestNumber := 1e-12;
     
     K := coefficientRing R;
     if class K === InexactFieldFamily then error "QQ or Gaussian rationals expected"; 
     if K === QQ then (
     	  I := symbol I;
     	  K = makeQI(); -- THE coefficient ring
     	  R = K[gens R]; 
     	  S = apply(S,f->sub(f,R));
     	  T = apply(T,f->sub(f,R));
	  );
     solsS = solsS / (s->sub(transpose matrix {toList s}, K)); -- convert to vectors
     	  -- affine patch functions ???
     	  pointToPatch := (x0,p)-> (1/(p*x0)_(0,0))*x0; -- representative for point x0 in patch p
	  patchEquation := p -> p * transpose vars R - 1;
	  --dPatch := true; -- not null ???       
     	  dPatch := null; -- for now !!!
     -- create homotopy
     t := local t; 
     Rt := K(monoid[gens R, t]); 
     t = last gens Rt;
     RtoRt := map(Rt,R,matrix{drop(gens Rt,-1)});
     H := matrix {apply(#S, i->(1-t)*(RtoRt S#i)+t*(RtoRt T#i))};
     JH := transpose jacobian H; 
     Hx := JH_(toList(0..n-1));
     Ht := JH_{n};

     -- here come things we have to compute only once
     norm2T := BombieriWeylNormSquaredQI T; -- n1
     norm2S := BombieriWeylNormSquaredQI S; -- n2
     max'deg := max deg;      
     W0 := 17/(50000*max'deg^3);
     u0 := 17586/100000;
     epsilon0 := u0^2/((4*max'deg)^3*(1+9*u0/8)^2);    

     -- evaluation times
     etH := 0;
     etHx := 0; 
     etHt := 0;
     specH := t0 -> flatten entries sub(H, vars R | matrix {{t0}});

     -- evaluation functions	
     evalH := (x0,t0)-> (
	  tr := timing (
     	       r := lift(sub(transpose H, transpose x0 | matrix {{t0}}), K);
	       if dPatch === null then r
	       else r || matrix{{0}} -- zero is appended
	       );
	  etH = etH + tr#0;
	  tr#1
	  );
     evalHx := (x0,t0)->( 
	  tr := timing (
     	       r := lift(sub(Hx, transpose x0 | matrix {{t0}}), K);
	       if dPatch === null then r
	       else r || matrix { flatten entries dPatch }
	       );
	  etHx = etHx + tr#0;
	  tr#1
	  );  
     evalHt := (x0,t0)->(
	  tr := timing (
	       r := lift(sub(Ht, transpose x0 | matrix {{t0}}), K);
	       if dPatch === null then r
	       else r || matrix {{0_K}}
	       );
	  etHt = etHt + tr#0;
	  tr#1
	  );
     local log'denominator't0; -- stores log_2 denominator t0
     compute'dt'epsilon := ( -- local subroutine (relies on evaluation functions)
	  x0,--z_i
	  t0 --s_i
	  ) -> 
     (
	  H't0 := specH t0; --g_i	
	  H0 := evalH(x0,t0); --v2
	  norm2H := BombieriWeylNormSquaredQI H't0; --n4	
	  invM := inverse evalHx(x0,t0); --M
	  cols'invM := entries transpose invM; 					   
	  norm2x0 := normSquareQI x0; 
	  chiTildeSquare1 := sum(#deg, 
	       i->(normSquareQI cols'invM#i)*(deg#i)*norm2H*norm2x0^(deg#i-1)
	       ) + (normSquareQI last cols'invM)*norm2x0; --a
	  ReScalarProductTandH't0 := Re BombieriWeylScalarProductQI(T,H't0); --Re <f,g_i>
	  v3 := norm2H*evalH(x0,1) - ReScalarProductTandH't0*H0; --v3
	  chiTildeSquare2 := --b
	    1 + 
	    normSquareQI(
		 invM
		 *
		 v3 --v4
		 )
	    / (norm2x0*( --||z_i||^2
		      norm2T*norm2H --n1*n4 
		      - 
		      ReScalarProductTandH't0^2 --n5
		      )); 
	  chiTildeSquare := chiTildeSquare1*chiTildeSquare2; --ab
	  if DBG>1 then << "chiTildeSquare1 = " << toRR chiTildeSquare1 
	  << ", chiTildeSquare2 = " << toRR chiTildeSquare2 << endl;
	  W := W0/chiTildeSquare;
	  L := 1 - W + W^2/6;
	  U := 1 - W/2; 
	  if DBG>1 then << "U - L = " << toRR (U-L) << "; ||x0|| = " << sqrt norm2x0 << 
	  "; res1 = " << abs QItoCC H0_(0,0)/(sqrt norm2x0)^max'deg << endl;
	  
          -- computation of epsilon
	  epsilon := epsilon0 / chiTildeSquare1;
	  if DBG>1 then << "epsilon = " << toRR epsilon << endl;

	  (-- return pair (dt,epsilon) 
	       pick'dt((norm2H,
		    Re BombieriWeylScalarProductQI(H't0,T-S),--Re <H't0,T-S>,
		    BombieriWeylNormSquaredQI(T-S) --||T-S||^2
		    ),L,U,log'denominator't0), 
	       epsilon
	       )
	  ); -- END compute'dt'epsilon
     
     compStartTime := currentTime();      

     rawSols := apply(#solsS, sN->(
	       s := solsS#sN;
	       s'status := Processing;
	       if DBG > 2 then << "tracking solution " << toString s << endl;
	       x0 := s; 
	       t0 := 0; 
	       log'denominator't0 = 0;
	       count := 1; -- number of computed points
	       if HISTORY then history := new MutableHashTable from{ count => new MutableHashTable from {
			 "t"=>t0,"x"=>x0
			 } };
	       while s'status === Processing do (
		    if DBG > 4 then << "--- current t = " << toRR t0 << endl;
     		    
		    dPatch = matrix{ flatten entries x0 / conjugateQI}; -- x0* used in evaluation
	     			
		    (dt,epsilon) := compute'dt'epsilon(x0,t0); -- main work is done here

		    --debugging code: multiply the representative by 10
		    --dPatch = matrix{ flatten entries (10*x0) / conjugateQI}; -- x0* used in evaluation
		    --compute'dt'epsilon(10*x0,t0); -- check independence on a representative
		    --dPatch = matrix{ flatten entries x0 / conjugateQI}; -- x0* used in evaluation

                    dx := 0; -- 0-th order predictor

		    if dt < 0.000001 then error "dt is very small";
		    if HISTORY then history#count#"dx" = dx;

    	 	    t1 := min(t0 + dt, 1_QQ);
		    x1 := x0 + dx;
		    
		    dx = -inverse(evalHx(x1,t1))*evalH(x1,t1);
		    x1 = x1 + dx;
		    x1 = shorterZero(x1,epsilon);
		    --x1 = roundQI(5,x1); -- round to a few digits!!!
		    
		    x0 = x1;
		    --x0 = normalizeQI x1; -- approximate normalization!!! 
		    t0 = t1;
		    log'denominator't0 = round log_2 denominator t0;
		    if DBG>1 then (
			 << "*** step " << count << " ***: t0 = " << toRR t0 << ", x0 = " << toString x0 << endl;
		    	 << "number of digits in the denominator of t0: " << log'denominator't0 << endl;
			 );
		    count = count + 1;
		    if HISTORY then history#count = new MutableHashTable from {"t"=>t0,"x"=>x0};
		    
		    if t0 == 1 then s'status = Regular;
		    );        	    
	       if DBG > 0 then << (if s'status == Regular then "."
		    else if s'status == Singular then "S"
		    else if s'status == MinStepFailure then "M"
		    else if s'status == Infinity then "I"
		    else error "unknown solution status"
		    ) << if (sN+1)%100 == 0 then endl else flush;
	       -- create a solution record 
	       (x0,
		    NumberOfSteps => count-1, -- number of points - 1 
		    SolutionStatus => s'status, 
		    --ConditionNumber => conditionNumber evalHx(x0,t0),
		    LastT => t0 
		    ) | ( if HISTORY
		    then sequence new HashTable from history 
		    else sequence ())
	       ));
     if DBG>3 then print rawSols;
     ret := rawSols/(s->{flatten entries first s} | drop(toList s,1));
     if DBG>0 then (
	  << "Number of solutions = " << #rawSols << endl;
	  << "Average number of steps per path = " << toRR sum(ret,s->s#1#1)/#ret << endl;
	  if DBG>1 then 
	  << "Evaluation time (M2 measured): Hx = " << etHx << " , 
	  Ht = " << etHt << " , H = " << etH << endl;
	  << "Setup time: " << compStartTime - setupStartTime << endl;
	  << "Computing time:" << currentTime() - compStartTime << endl; 
	  );
     apply(ret, s->
	       if HISTORY then drop(toList s, -1)
	       else toList s
	       )
     )

-- Gaussian rationals: "QI" = QQ[I]/(I^2+1)
THE'QI := QQ[I]/(I^2+1) 
makeQI = method()
makeQI = ()->THE'QI

conjugateQI = method() 
conjugateQI RingElement := RingElement => x -> sub(sub(x, matrix{{ -I }}),ring x)

Re = method() 
Re ZZ := identity
Re QQ := identity
Re RingElement := RingElement => x -> sub((x + conjugateQI x)/2,QQ)
Im = method() 
Im RingElement := RingElement => x -> sub((x - conjugateQI x)/(2*THE'QI_0),QQ)
     
normSquareQI = method(TypicalValue=>RingElement) -- 2-norm of a vector                           
normSquareQI List := v -> sub(sum(v, x->x*conjugateQI x),QQ);
normSquareQI Matrix := v -> normSquareQI flatten entries v -- this is Frobenius norm for a matrix

productQI = method()
productQI (List, List) := (a,b) -> sum(#a, i->a#i*conjugateQI b#i)
productQI (Matrix, Matrix) := (a,b) -> productQI(flatten entries a, flatten entries b)

distanceSquareQI = method()
distanceSquareQI (Matrix, Matrix) := (a,b) -> (Re productQI(a,b))^2 / (normSquareQI a * normSquareQI b)  

normalizeQI = method(TypicalValue => Matrix) -- normalizes a column vector
normalizeQI Matrix := v -> (1/approxSqrt(normSquareQI v,1/100000))*v

roundQI = method() 
roundQI (ZZ, RingElement) := RingElement => (n,x) -> 10^(-n)*(round(10^n*Re x) + round(10^n*Im x)*(ring x)_0)
roundQI (ZZ, Matrix) := Matrix => (n,M) -> matrix apply(entries M, r->apply(r, e->roundQI(n,e)))

QItoQQ = x -> sub(x,QQ)

newCCRing := memoize (R->CC(monoid R))

QItoCC = method()
QItoCC RingElement := RingElement => x -> (
     R := ring x;
     if R === THE'QI then toRR Re x + ii*(toRR Im x)
     else if instance(R, PolynomialRing) then (
     	  CR := coefficientRing R;
	  newR := newCCRing R;
	  sum(listForm x, (m,c)->QItoCC c * newR_m)
	  )
     else "error can not convert to complex numbers"
     )
QItoCC List := List => L -> L/QItoCC
QItoCC Matrix := Matrix => M -> matrix(M/QItoCC)
	     
floorQQ = method()
floorQQ QQ := ZZ => x -> (numerator x // denominator x) - (if x<0 then 1 else 0)

approxSqrt = method()
approxSqrt(QQ,QQ) := (t,e) -> (
     --t' := toQQ(sqrt t);
     --if t'^2>=t and t'^2<t*(1+e)^2 then return t'; -- hack!!!
     t' := approxSqrtGeqOne(numerator t,e) / approxSqrtGeqOne(denominator t, e/(1-e));
     --print (toRR t' - sqrt t);
     t'
     )

approxSqrtGeqOne = method()
approxSqrtGeqOne(ZZ,QQ) := (t,e) -> (
     if t == 0 then return 0;
     t0 := t;
     t' = (t0 + 1)/2;
     while t'^2>t*(1+e)^2 do (
	  if (floorQQ t')^2 >= t then t' = floorQQ t';
	  t0 = t';
	  t' = t0/2 + t/(2*t0);
	  );
     t'
     )

-- out: z s.t. ||z-x0|| <= sqrt e and coords of z ar integers of length at most 1 + 3 sqrt((n+1)/e)
shorterZero = method()
shorterZero (Matrix,QQ) := Matrix => (x0,e) -> (
      n := numRows x0 - 1;
      R := ring x0; 
      x0'rat := apply(flatten entries x0, c->{Re c, Im c});     
      m := lcm(flatten flatten x0'rat/denominator@@QItoQQ);
      x := m*x0;
      x'rat := apply(flatten entries x, c->{Re c, Im c});     
      norm2x := normSquareQI x;
      --1/0;
      r := (21/20)^2;
      a := 4; 
      k := 0;
      while 2*(n+1)*r*a <= e*norm2x do (
	   k = k+1;
	   a = 4*a;
	   );
      z'rat := apply(x'rat,c->apply(c,t->round(2^(-k)*t)));
      transpose matrix{apply(z'rat, c->c#0+c#1*R_0)}
      )

BombieriWeylScalarProductQI = method()
BombieriWeylScalarProductQI (RingElement,RingElement) := RingElement => (f,g) -> sum(listForm f, a->(
	  imc := product(a#0, d->d!) / (sum(a#0))!; -- inverse of multinomial coeff
	  bc := coefficient((ring f)_(first a),g); -- coeff of corresponding monomial in g
	  imc*a#1*conjugateQI bc
	  ))
BombieriWeylScalarProductQI (List,List) := QQ => (F,G) -> sum(#F, i->BombieriWeylScalarProductQI(F#i,G#i)) 

BombieriWeylNormSquaredQI = method()
BombieriWeylNormSquaredQI RingElement := QQ => f -> Re sum(listForm f, a->(
	  imc := product(a#0, d->d!) / (sum(a#0))!; -- inverse of multinomial coeff
	  imc*a#1*conjugateQI a#1 
	  ))
BombieriWeylNormSquaredQI List := QQ => F -> sum(F, f->BombieriWeylNormSquaredQI f) 

pick'dt = (abc,L,U,log'denominator't0) -> (
     (a,b,c) := abc;
     -- solve: (b^2-acU^2)t^2+2ab(1-U^2)+a^2(1-U^2)=0 and the same for L
     A := toRR(b^2-a*c*U^2);
     B := toRR(2*a*b*(1-U^2));
     C := toRR(a^2*(1-U^2));
     assert(A<0 and B^2-4*A*C>=0);
     --return toQQ( (-B-sqrt(B^2-4*A*C))/(2*A) ); -- hack
     if DBG>5 then <<"pick'dt: dt = " << t << endl;
     local r;
     isPositive := t -> (
	  tb := t*b;
	  r = (a+tb)^2 / (a*(a+2*tb+t^2*c));
  	  a+tb > 0 and 4*r > (L+U)^2
	  );
     t1 := 1;
     L2 := L^2;
     isPositive t1;
     if a+b>0 and r>=L2 then t1
     else (
	  U2 := U^2;
	  t0 := 0;
	  t2 := (t0+t1)/2;
     	  count := 0; -- the number of bisections
	  s2 := isPositive t2;
	  while (L2>r or U2<r) or count<log'denominator't0-1 do (
	       if s2 then t0 = t2 else t1 = t2;
	       t2 = (t0+t1)/2;
	       count = count + 1;
	       s2 = isPositive t2;
	       ); 
	  if DBG>1 then << "number of bisections: " << count << endl; 
	  t2
     	  )
     ) 

toQQ = method()
toQQ (ZZ,RR) := QQ => (n,t) -> ( -- n=number of binary digits
     s := 2^(-round log_2 t + n);
     round(s*t) / s  
     )
toQQ RR := QQ => t -> round (t<<(maxExponent//2))/2^(maxExponent//2)

compute'u0RPc = delta -> (
     u0 := 0.17586;
     R := sqrt 2; 
     P := sqrt 2 + sqrt(2+5/8);
     a := (2*delta-1)*u0/(sqrt(2)+2*delta*u0);
     c':= 1-(1-a)^(P/sqrt 2); 
     c := (1-(sqrt 2)*u0/2)^(sqrt 2)*c'/(1+(sqrt 2)*u0/2);
     (u0,R,P,c)
     ) 
end
-----------------------------------------------------------------

load "certifiedNAG.m2"
(u0,R,P,c) = compute'u0RPc(3/4)
C = R*P/log_10(1+c)

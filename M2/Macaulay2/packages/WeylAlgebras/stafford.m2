-- stafford.m2
-- 2002-2006 
-- author: Anton Leykin

---------------------------------------------------------------------
---------- Implementation of "effective" Stafford's procedure -------
---------------------------------------------------------------------

--------------------------------------------------------------------
-- Given: M\subset S^(m), where S=D<x,dx> 
--     	  \de_1...\de_m \in S D-lin.independent, 
--        \alpha\in S 
-- Find: f\in D(x)<\p> s.t. \sum(\alpha\de_i f \e_i) is not in M
--------------------------------------------------------------------
-- INPUT 
-- R : ring Q<x1...xn,d1...dn>, 
-- k : comes from {\cal R}_k = Q(x1...xn,d1...dk)<d(k+1)..,dn>
-- Mmat : matrix defining submodule M of S^(n)
-- alpha: element of R
-- de : list of de_1...de_m \in Q<x(k+1),\p(k+1)>
-- OUTPUT
-- f : \in Q<x(k+1),\p(k+1)>  
--------------------------------------------------------------------
--findF (Ring, ZZ, Matrix, RingElement, List) := (R, k, Mmat, alpha, de) -> (
makeSigma = (alpha,de,f) -> (
     transpose matrix {apply(de, de->alpha*de*f)}
     )
findF = (p) -> (
     R := p#0; k := p#1; M := p#2; alpha := p#3; de := p#4;
     n := numgens R // 2; 
     m := numgens target M;
     pInfo(99, ">>>findF:\tR = "|toString R|"\n\tk = "|toString k|"\n\tM = "|toString M|
	  "\n\talpha = "|toString alpha|"\n\tde = "|toString de);
     f := 1_R;
     s := makeSigma(alpha,de,f);
     if isAll(k,M) then error "M equals the whole ambient space";
     if reduceNK(s, gens gb M, n+k) != 0 then return f;
     if m != #de then error "rank target Mmat != #de";
     -- get rid of DX
     maxDeg := max(flatten entries s/(d->varDegree(n+k,d)));
     while maxDeg > 0 do (
	  maxDeg = maxDeg - 1; 
	  X := R_k;
	  f = f*X;
	  s = X*s - transpose matrix{flatten entries s/(u->u*X)};
     	  if reduceNK(s, gens gb M, n+k) != 0 then return f;
	  );
     -- get rid of X
     maxDeg = max(flatten entries s/(d->varDegree(k,d)));
     while maxDeg > 0 do (
	  maxDeg = maxDeg - 1; 
	  DX := R_(n+k);
	  f = f*DX;
	  s = DX*s - transpose matrix{flatten entries s/(u->u*DX)};
     	  if reduceNK(s, gens gb M, n+k) != 0 then return f;
	  );
     -- check if s has only one nonzero component
     ind := select(toList(0..m-1),i->s_(i,0)!=0);
     if #ind>1 then error "index is not unique"
     else ind = first ind;
     pInfo(99, "ind = "|toString ind|" s = "|toString s|" M = "|toString M);
     -- e_ind is in M
     pInfo(99, "findF: cutting the matrix");
     smallerM := M^((toList(0..ind-1))|(toList(ind+1..(m-1)))); 
     findF {R,k,gens gb smallerM,alpha,drop(de,{ind,ind})}             
     )

--findF (Ring, ZZ, Matrix, RingElement, List) := (R, k, Mmat, alpha, de) -> (
findF2 = (p) -> (
     R := p#0; k := p#1; Mmat := p#2; alpha := p#3; de := p#4;
     n := numgens R // 2; 
     m := numgens target Mmat;
     if m != #de then error "rank target Mmat != #de";
     maxDeg := max(de/(d->varDegree(n+k,d)));
     maxDe := first select(1, de, d->varDegree(n+k,d)==maxDeg); 
     noD := cancelOutDX(alpha*maxDe, k, n+k);
     iHaveNoD := apply(de, d->valuePres(noD,alpha*d));

     maxDeg = max(iHaveNoD/(d->varDegree(k,d)));          
     -- here maxDe is an index of the "biggest" guy
     maxDe = first select(toList(0..#iHaveNoD-1), 
	  i->varDegree(k,iHaveNoD#i)==maxDeg);
     noX := cancelOutX(iHaveNoD#maxDe,k,n+k);
     noDX := composePres(noD,noX);

     -- try to get f
     f := select(noDX, 
	  u->(
	       alphaDeU1 := apply(de, d->alpha*d*u#1); 
	       reduceInCalR(k, transpose matrix{alphaDeU1}, Mmat)!=0
	       )
	  );
     if #f>0 then (
	  deg := min apply(f,u->first degree u#1);
	  return (first select(1,f,u->first degree u#1 == deg))#1
	  );
     
     -- otherwise e_maxDe is in M
     << "findF: cutting the matrix" << endl;  
     smallerMmat := Mmat^((toList(0..maxDe-1))|(toList(maxDe+1..(m-1)))); 
     findF {R,k,gens gb smallerMmat,alpha,drop(de,{maxDe,maxDe})}  
     );     

--------------------------------------------------------------------
-- Given M\subset S^m, \de_1...\de_m and \alpha\in S^m 
-- find f\in D(x)<\p> s.t. M + S sum(\alpha\de_i f \e_i) = S^m 
--------------------------------------------------------------------
--findGoodF (Ring, ZZ, Matrix, RingElement, List):=(R, k, Mmat, alpha, de) -> (
findGoodF = (p) -> (
     R := p#0; k := p#1; Mmat := p#2; alpha := p#3; de := p#4;
     n := numgens R // 2; 
     m := numgens target Mmat;
     if m != #de then error "rank target Mmat != #de";
     pInfo(3,">>>findGoodF: finding f");
     f := findF p;
     alphaDeF := makeSigma(alpha,de,f);
     Mmat' := gens gb (Mmat| alphaDeF);
     if isAll(k, Mmat') then (
	  pInfo(3,"<<<findGoodF: return "|toString f);
	  return f
	  );
     pInfo(3,"findGoodF: finding g");
     t := (lclm(Mmat, alphaDeF))#1; -- t*sum(alpha de_i f) \in M
     g := findG {R,k,Mmat,Mmat',t,alpha,de,f};
     return f+g;     
     )

protect alphaDeF
--findG (Ring, ZZ, Matrix, Matrix, RingElement, RingElement, List, RingElement):=
--      (R,    k,  Mmat,   Mmat',  t,           alpha,       de,   f) -> (
findG = (p) -> (
     R := p#0; k := p#1; Mmat := p#2; Mmat' := p#3; 
     t := p#4; alpha := p#5; de := p#6; f := p#7;
     n := numgens R // 2; 
     m := numgens target Mmat;
     if m != #de then error "rank target Mmat != #de";
     pInfo(99,"findG: t*alpha="|toString(t*alpha)|" Mmat'="|toString Mmat'); 
     g := findGoodF {R,k,Mmat',t*alpha,de};
     --alphaDeG := apply(de, d->alpha*d*g);
     --P1mat := transpose matrix{alphaDeG};
     --N1mat := gens gb (Mmat| P1mat);
     P3mat := makeSigma(alpha,de,f+g);
     pInfo(99, "findG: computing N3mat = gb "|toString(Mmat| P3mat));
     N3mat := gens gb (Mmat| P3mat);
     pInfo(99, "findG: checking (f+g)... N3mat="|toString N3mat);
     if isAll(k, N3mat) then (
	  pInfo(3,"<<<findG: return "|toString(g));
	  return g;
	  )
     else (
	  -- intersect M+P(\alpha,f) and M+P(\alpha,g)
	  alphaDeG := makeSigma(alpha,de,g);
	  Smat := alphaDeF | Mmat | alphaDeG;
	  S := (syz(Smat))^{0}; -- syzygies are columns, take the first row  
	  Mmat'' := Mmat | matrix{apply(flatten entries S, s->s*alphaDeF)};
	  return findG {R,k,Mmat,Mmat'',t,alpha,de,f};
	  ); 
     )
--------------------------------------------------------------------
-- Given \rho, \de_1...\de_m \in S^m 
-- find f\in D(x)<\p> s.t. 
-- 	S^(m+1) rho + S (\e_0 + sum(\rho \de_i f \e_i)) = S^(m+1)
--------------------------------------------------------------------
--findFforRho (Ring, ZZ, RingElement, List):=(R, k, rho, de) -> (
findFforRho = (p) -> (
     R := p#0; k := p#1; rho := p#2; de := p#3;
     n := numgens R // 2; 
     m := #de;
     Mmat := rho*id_(R^m);
     findGoodF {R,k,Mmat,rho,de}
     ) 
findFforRhoWhenUis0 = (p) -> (
     R := p#0; k := p#1; rho := p#2; de := p#3;
     n := numgens R // 2; 
     m := #de;
     Mmat := rho*id_(R^m);
     findGoodF {R,k,Mmat,1_R,de}
     ) 
-------------------------------------------------
-- decomposes v = \de_1*G_1 + ... + \de_n * G_n
-------------------------------------------------
deltaG = (k,v) -> (
     R := ring v;
     n := numgens R // 2;
     l := listForm v;
     de := {};
     listG := {};
     while #l != 0 do (
	  f := first l;
	  G := 0;
	  de = de | {(R_k)^(f#0#k)*(R_(n+k))^(f#0#(n-k))};
	  scan(l, u-> if u#0#k==f#0#k and u#0#(n-k)==f#0#(n-k) 
	       then G = G + u#1*R_(
		    take(u#0,k)|{0}|
		    take(u#0,{k+1,n+k-1})|{0}|
		    take(u#0,{n+k+1,2*n-1})
		    )
	       );
	  l = select(l,  u-> u#0#k!=f#0#k or u#0#(n-k)!=f#0#(n-k));
	  listG = listG|{G}; 
	  );  
     (de,listG)
     )
---------------------------------------------------
-- returns maximal D-part(non-invertible D's) of g 
---------------------------------------------------
maxDpart = (k,g)->(
     n := numgens ring g // 2;
     dk := k+n; 
     l:= listForm g / (m->(toList(dk:0)|drop(m#0,dk),1));
     drop((listForm leadTerm toPoly(l,ring g))#0#0,dk)
     )
-----------------------------------------------------
-- finds \rho s.t. \rho Bu_i \in ideal q for all i
-----------------------------------------------------
findRho = (k,Bu,q)-> (
     R := ring q;
     n := numgens R // 2;
     Rk1 := makeRk(R,n+k+1);
     m := #Bu;
     M := (transpose matrix{Bu}) | (q*id_(R^m));
     M = substitute(M,Rk1);
     S := gens gb syz M;
     rhoCandidates := select(toList(0..numgens source S-1),
	  i->S_(0,i)!=0 and lastVarN(S_(0,i))<=n+k);
     deg := min(apply(rhoCandidates, i->first degree(S_(0,i))));
     rhoI := first select(1,rhoCandidates,i->first degree(S_(0,i))==deg);
     return substitute(S_(0,rhoI),R)     
     ) 

-----------------------------------------------------------------
-- given q, u and v produces (q',a,b,f) s.t.
-- q' = aq+b(u+vf) and Ddeg(q')<Ddeg(q)
-----------------------------------------------------------------
quvf = (q,u,v)-> (
     R := ring q;
     n := numgens R // 2; 
     k := lastVarN q - n; -- number of D's inverted  
     Rk := makeRk(R,n+k); -- something like script R
     q = substitute(q,Rk); 
     u' := u = substitute(u,Rk);          
     v = substitute(v,Rk);          
     deG := deltaG(k,v);
     de := deG#0; G := deG#1;
     
     maxD := maxDpart(k+1,last G);
     scan(#G-1, i->(
	 newMaxD := maxDpart(k+1,G#i);
	 if product(newMaxD,j->j+1)<product(maxD,j->j+1) 
	 then maxD = newMaxD;
	 ));
     H := apply(toList((toList((n-k-1):0))..maxD), u->
	 toPoly({(toList((k+1):0) | u | toList(n:0),1)},Rk)
	 );
     f := 0; -- f to output
     I := ideal q + ideal u; 
     pInfo(99, "quvf: H = "|toString H);
     scan (H, h->(
	 if isAll(k,I) then break; 
	 B := apply(G, g->g*h);
	 rho := findRho(k,B|{u'},q);
	 f1 := if u'==0 then findFforRhoWhenUis0 {Rk,k,rho,de} 
	 else findFforRho {Rk,k,rho,de};
	 f = f+f1*h;
	 u' = u'+v*f1*h;
	 --I = I + ideal B;
	 I = ideal q + ideal u';      
	 ));     
     RkGb := gb(matrix{{q,u+v*f}}, ChangeMatrix=>true);     
     pInfo(999, "quvf: RkGb = "|toString RkGb);
     RkGbList := first entries gens RkGb;
     ChMat := getChangeMatrix RkGb;
     ind := first select(1, toList(0..#RkGbList-1), 
	  i->lastVarN(RkGbList#i)<n+k); 
     q' := substitute(RkGbList#ind,R);
     a := substitute(ChMat_(0,ind),R);
     b := substitute(ChMat_(1,ind),R);
     f = substitute(f,R);
     (q',a,b,f) -- aq+b(u+vf) = q'
     )

------------------------------------------
-- finds a'=a+dc, b'=b+ec s.t. a',b' generate 
-- the same ideal as a,b,c 
------------------------------------------
mainStep = (a,b,c) -> (
     R := ring a;
     n := numgens R // 2; 
     a' := a;
     b' := b;
     l := lclm(a,c);
     h1 := l#0;
     h2 := 0;
     q := l#1;
     while lastVarN q >=n do (
	  pInfo(99, "mainStep: q = "|toString q);
	  pInfo(99, "mainStep: qc = "|toString (q*c));
	  if h1 == 0 then (
	       temp := a';
	       a' = b';
	       b' = temp;
	       h1 = h2;
	       h2 = 0;
	       );
	  local g1; local g2;
	  if h2 == 0 then ( g1 = 0; g2 = 1; )
	  else (
	       l = lcrm(h1,h2);
	       g1 = l#0;
	       g2 = l#1;
	       );
	  l = lclm(q*c,b');
	  s := l#0; t := l#1;
	  v := t*g2;
	  pInfo(99, "mainStep: v = "|toString v);
	  rs := quvf(q,0_R,v);
	  q = rs#0; -- new q = p1*q + p2*v*f
	  p1 := rs#1; p2 := rs#2; f:= rs#3;
	  pInfo(99, "mainStep: g1 = "|toString g1|" g2 = "|toString g2|" f = "|toString f);
	  a' = a' + g1*f*c;
	  b' = b' + g2*f*c;
	  h1 = (p1-p2*s)*h1; 
	  h2 = (p1-p2*s)*h2+p2*t;   
	  );     
     {a',b'}
     )

stafford = method();
stafford Ideal := I -> (
     g := first entries gens I;
     while #g>2 do g = mainStep(g#0,g#1,g#2) | drop(g,3);
     ideal g
     )
---------------------------------------------------------------
-- ABOVE is what was mystaff.m2 
--
-- BELOW is what was ore.m2 
---------------------------------------------------------------
toPoly = (l,R) -> (
     f := 0_R;
     scan(l, m->f=f+m#1*R_(m#0));
     f 
     )

makeRk = method();
makeRk (Ring,ZZ) := (R,k)->( -- R = k[x_1,...,x_n,D_1,...,D_n]  
     n := numgens R;
     v := first entries vars R;
     w := toList(k:0)|toList((n-k):1);
     (coefficientRing R)(monoid [v, 
	  WeylAlgebra=>R.monoid.Options.WeylAlgebra,
	  Weights=>w
	  ]) 
     )
makeSk = method();
makeSk (Ring,ZZ) := (R,k)->( -- R = k[x_1,...,x_n,D_1,...,D_n]  
     n := numgens R;
     v := first entries vars R;
     w := toList(k:0)|{1}|toList((n-k-1):0);
     (coefficientRing R)(monoid [v, 
	  WeylAlgebra=>R.monoid.Options.WeylAlgebra,
	  Weights=>w
	  ]) 
     )

lastVarN = method()
lastVarN (RingElement) := q -> (
     n := numgens ring q;
     k := n-1;
     while k>=0 do (
	  if any(listForm q, m->m#0#k>0) 
	  then break
	  else k=k-1;
	  );
     k
     )
satellite = method()
satellite (Ring,ZZ) := (R,k) -> (
     n := numgens R;
     n2 := n//2;
     if k<n2 then n2+k
     else k-n2
     ) 
kDegree = method()
kDegree (RingElement,ZZ) := (f,k) -> (
     max apply(listForm f, u->u#0#k)
     )  

-------------------------------------------------------------
-- cancels out the n-th (differential) variable
-- (where m-th (nondifferential) variable corresponds to it) 
-------------------------------------------------------------
cancelOutDX = (h, m, n) -> (
     W := ring h;
     X := W_m;
     l := listForm h;
     s := max(l/(u->u#0#n)); 
     apply(s+1, i->
	  ((-1)^i*binomial(s,i)*X^i, X^(s-i))
	  )     
     )
-------------------------------------------------------------
-- differentiate once w.r.t. n-th (differential) variable
-- (where m-th (nondifferential) variable corresponds to it) 
-------------------------------------------------------------
diffDX = (h, m, n) -> (
     W := ring h;
     X := W_m;
     X*h-h*X
     )
---------------------------------------------------
-- cancels out the m-th (non-differential) variable 
---------------------------------------------------
cancelOutX = (h, m, n) ->(
     W := ring h;
     DX := W_n;
     l := listForm h;
     s := max(l/(u->u#0#m));
     apply(s+1, i->
	  ((-1)^(s-i)*binomial(s,i)*DX^i, DX^(s-i))
	  )     
     )
---------------------------------------------------
-- given presentations of  g \in DfD and h \in DgD 
-- computes the presentation of h \in DfD
---------------------------------------------------
composePres = (g, h) ->(
     flatten apply(g, u->( apply(h, v->(
	       		 (v#0 * u#0, u#1 * v#1)
	       		 ))))
     ) 
---------------------------------------------------
-- evaluates the presentation
---------------------------------------------------
valuePres = (p, h) -> (
     sum( p, u -> u#0 * h * u#1 )
     )

--------------------------------------------------------------------------
-- obtains the presentation of 1 in the two-sided DhD, where D = A_n(k) 
--------------------------------------------------------------------------
getOne = h -> (
     -- prep work
     W := ring h;
     createDpairs W;

     dpV := W.dpairVars;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(dpI#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if not isField coefficientRing W then 
     error "expected field as the coefficient ring";
     
     t := h;
     pres := {(1_W,1_W)};
     scan(#(dpV#0), i->(
	       pres1 := cancelOutX(t, dpI#0#i, dpI#1#i);
	       t = valuePres(pres1, t);
	       pres2 := cancelOutDX(t, dpI#0#i, dpI#1#i);
	       t = valuePres(pres2, t);
	       pres = composePres(composePres(pres, pres1),pres2);
	       ));            
     l := leadCoefficient t;
     apply(pres, u->((1/l)*u#0, u#1))
     )

---------------------------------------------------
-- degree of k-th var in r
---------------------------------------------------
varDegree = method()
varDegree (ZZ, RingElement) := (k,r) -> (
     max(listForm r/(u->u#0#k))
     )

---------------------------------------------------
-- lclm = "lowest common left multiple
-- lclm (a,b) = (c,d) s.t. ca=db
---------------------------------------------------
lclm = method()
lclm (RingElement, RingElement) := (a,b) -> (
     S := syz(matrix{{a,b}}--, SyzygyLimit=>1
	  );
     inds := toList(0..numgens source S - 1);
     minDeg := min apply(inds, i->first degree S_(0,i));
     minI := first select(1, inds, i->first degree S_(0,i)==minDeg);
     return (S_(0,minI),S_(1,minI));
     )
lclm (Matrix, Matrix) := (a,b) -> (
     s := syz (b|a);
     c := first select(1,toList(0..(numgens source s - 1)),
	  i->s_(0,i)!=0);
     s = first entries transpose s_{c};
     (drop(s,1), -s#0)  
     )
lcrm = (a,b) -> (
     s := Fourier Fourier Fourier syz(Fourier matrix{{a,b}}
	  --,SyzygyLimit=>1
	  );
     (s_(0,0), s_(1,0))
     )

-----------------------------------------------------------------
-- check is vector v is "divisible" by w
-----------------------------------------------------------------
leadMonomial Vector := v -> (
     l:=leadComponent v;
     leadMonomial (components v)_l
     )
Ddivisible = (v,w,k) -> (     
     R := ring v;	 
     n := numgens R // 2;      
     ltV := (leadTerm v)_0;
     ltW := (leadTerm w)_0;
     if (l:=leadComponent ltV) != leadComponent ltW then (return false);
     all(drop(listForm leadMonomial (components ltV)_l, n+k-1) 
	  -drop(listForm leadMonomial (components ltW)_l, n+k-1), i->i>=0)
     )

-----------------------------------------------------------------
-- reduce vector v w.r.t GB M (k the last noninvertible variable)
-----------------------------------------------------------------
reduceInCalR = (k,v,M) -> (
     --<< "reduceInCalR: v=" << v << " M=" << M << endl; 
     R := ring v;	 
     n := numgens R // 2;      
     r := v;
     while  r!=0 do (
     	  l := select(1, toList(0..(numgens source M - 1)), 
	       i->Ddivisible(r,M_{i},k));
     	  if #l == 0 then (
	       --<< "reduceInCalR: return " << r << endl;
	       return r
	       );
	  g := M_(first l);
	  noninvert := drop(listForm leadMonomial g, n+k);
	  mult := sum(listForm (components g)#(leadComponent g), m->
	       if drop(m#0, n+k)==noninvert 
	       then m#1*R_(take(m#0, n+k))
	       else 0
	       );
	  r = (mult*r)%M;
     );
     r
     )
--------------------------------------------------------------------
-- new implementation
---------------------------------------------------------------------
isDivisibleNK = method()
isDivisibleNK (Matrix,Matrix,ZZ) := (f,g,nk) -> (
     if f==0 or g==0 then error "nonzero vectors expected";
     i := first select(1, reverse toList(0..numgens target f-1), i->f_(i,0)!=0);
     j := first select(1, reverse toList(0..numgens target g-1), i->g_(i,0)!=0);
     if i!=j then return false;
     -- lf and lg are the leading monomials as lists
     lf := first first listForm leadMonomial f_(i,0); -- !!! added "first first" 
     lg := first first listForm leadMonomial g_(j,0); --
     -- first nk vars are nor looked at
     return all(toList(nk..numgens ring f-1), i->lf#i>=lg#i) 
     )
reduceNK = method()
reduceNK (Matrix,Matrix,ZZ) := (f, M, nk) -> (
     while f!=0 do (
       	  divI := select(1, toList(0..numgens source M - 1), i->isDivisibleNK(leadTerm f, leadTerm M_{i}, nk));
       	  if #divI > 0 then g := M_{divI#0}
	  else return f;  
	  pInfo(999, "f = "|toString f|" g = "|toString g); 	    
     	  S := syz( matrix{{leadTerm f, leadTerm g}}--, SyzygyLimit=>1
	       );
	  inds := toList(0..numgens source S - 1);
	  minDeg := min apply(inds, i->first degree S_(0,i));
	  minI := first select(1, inds, i->first degree S_(0,i)==minDeg);
	  f = S_(0,minI)*f+S_(1,minI)*g;
	  -- reduce the coefficients in x's 
	  R := ring f;
	  n := numgens R // 2;
	  C := null;
	  scan(numgens target f, i->(
		    if f_(i,0)!= 0 then 
		    scan(listForm f_(i,0), u->(
			      if C === null then C = take(u#0,n)
			      else C = apply(n, j->min(u#0#j,C#j));
			      ))
		    ));
	  pInfo(999, "C = "|toString C);	   
	  if C =!= null and sum C != 0 then 
	  f = transpose matrix {
	       apply(numgens target f, i->(
		    toPoly(apply(listForm f_(i,0), u->(u#0-(C|toList(n:0)),u#1)),R)
		    ))};
	  pInfo(999, "new f = "|toString f);	   
     	  );
     return f;
     )
--------------------------------------------------------------------
-- determines whether image M = target M 
--------------------------------------------------------------------
isAll = method()
isAll (ZZ,Matrix) := (k,M) -> (
     R := ring M;
     m := numgens target M;
     return all(toList(0..m-1), i->(
	       e := transpose matrix{toList(i:0)|{1_R}|toList(m-i-1:0)};
	       reduceNK(e, gens gb M, (numgens R//2) + k) == 0
	       ));
     t := target M;
     all(toList(0..numgens t - 1), i->reduceInCalR(k,t_{i},M)==0)
     )
isAll (ZZ, Ideal) := (k,I) -> (
     return isAll(k, gens gb I);
     R := ring I;
     n := numgens R // 2;
     GB := first entries gens gb(I,SubringLimit=>1);
     any(GB, g->lastVarN g < n+k)
     )


TEST ///
--EXAMPLE 1-------------------------------
n = 3
R = QQ[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1; b = D_3; c = D_2;
a = D_1+x_2; b = D_3^2+x_2*D_3+x_3; c = D_2^2;
stafford ideal (a,b,c)
assert(ideal(a,b,c)==oo)
--EXAMPLE 2-------------------------------
n = 3
R = ZZ/101[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1; b = D_3; c = D_2;
a = D_1+x_2; b = D_3^2+x_2*D_3+x_3; c = D_2^2;
stafford ideal (a,b,c)
assert(ideal(a,b,c)==oo)
--EXAMPLE 3-------------------------------
n = 3
R = QQ[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1+x_3; b = D_2+x_2; c = D_3+x_1;
d = x_2+D_2
a = d*(D_1+x_3); b = (D_1+x_3)*(D_3+x_1); c = d*(D_3+x_1);
stafford ideal (a,b,c)
assert(ideal(a,b,c)==oo)
--EXAMPLE 4-------------------------------
n = 4
R = QQ[x_1..x_n,D_1..D_n, WeylAlgebra=>(apply(n,i->x_(i+1)=>D_(i+1)))] 
a = D_1; b = D_2; c = D_3; d = D_4;
stafford ideal (a,b,c,d)
assert(ideal(a,b,c,d)==oo)
///
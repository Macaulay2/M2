newPackage(
    "QthPower",
    Version => "1.02", 
    Date => "January 17, 2014",
    Authors => {{Name => "Douglas A. Leonard",
                 Email => "leonada@auburn.edu",
                 HomePage => "http://www.dms.auburn.edu/~leonada"}},
    Headline => 
    "An implementation of the Qth-Power algorithm for computing integral closures",
    Keywords => {"Commutative Algebra"},
    PackageImports => {"IntegralClosure"}
)
------------------------------------------------------------------------
export{"qthConductor", 
       "qthIntegralClosure", 
       "rationalIntegralClosure", 
       "minimization",
       "weightGrevlex",
       "grevlexWeight",
       "testWeightMatrix"
}
-- PURPOSE : To implement Leonard's Qth-power algorithm 
--           to compute the integral closure C(A,Q(A)) of a domain A=R/I
--           an integral extension of a polynomial ring P
--	     over ZZ/q or QQ as qthIntegralClosure and rationalIntegralClosure respectively.
--          [rationalIntegralClosure calls qthIntegralClosure for several small q.]
--   INPUT : weight matrix wtR used to generate an appropriate weight-over-grevlex monomial ordering, 
--           multivariate polynomial ring R over ZZ/q or QQ
--          (with Noether normalization, P the subring of the last numRows(wtR) (independent) variables), 
--	     and generating set for the ideal defining an affine domain 
--          A=R/ideal(I) that is an integral extension of P 
--  OUTPUT : list a polynomials, the zeroth of which is a common denominator, hence a conductor, in P,
--           with all being numerators for the fractions from Q(A) forming a P-module generating set,
--           list of minimal,reduced Gr\"obner basis elements for the ideal of induced relations,
--	     multivariate polynomial ring of that ideal,
--           induced weight function for that ring.
-- COMMENTS :(1) The package contains short unexported functions 
--              for doing basic number theory, 
--              in particular, the (complete) extended Euclidean algorithm
--	        and the Chinese Remainder Theorem algorithm, 
--              as well as standard modular exponentiation, 
--              and functions for dealing 
--              with certain weighted monomial orderings,
--              Versions of some of these functions are now part of the core of {\sc Macaulay2},
--              though they were not in 2009/2010 when this code was originally written. 
--          (2) The algorithm was originally meant to do only type I domains
--              when the weight matrix actually defines a weight function
--      	but probably works on integral extensions if wt(LT(f))=wt(f-LT(f)). 
--	    (3) Methods are local, in that the conductor is factored,
--	        and separate closures for each local factor 
--              are reconciled to get the global result.  
--          (4) While there is a minimization function below based on the induced weights,
--              it is currently probably better to use the weights
--              to manually define a new ring with appropriate names,
--              and appropriately map the output to the new ring.
--              An example is provided below. 
--          (5) In addition to the test examples below, there are several annotated examples
--              on the author's website above, 
--              comparing this to other implementations of integral closure/normalization algorithms.             
--          (6) The code doesn't currently check the input to see 
--              (a) if the weight matrix is appropriate for the quotient ring,
--              (b) whether R/ideal(I) is an integral extension, let alone a domain,
--      	(c) whether q>0 for qthIntegralClosure and q=0 for rationalIntegralClosure,
--              and (d) probably other things that may produce erroneous output.		
------------------------------------------------------------------------
-----------------------  Code  -----------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
-------- non-exported helper functions --------------------------------- 
------------------------------------------------------------------------

-- dependent and independent "logs" and leading monomials of a polynomial
logpoly = (v,depvars,indvars) -> (
        lv := leadMonomial v;
    indlog := apply(#indvars, i->degree(indvars#i, lv));
   indprod := product(#indvars, i->(indvars#i)^(indlog#i));
    deplog := apply(#depvars, i->degree(depvars#i, lv));
   depprod := product(#depvars, i->(depvars#i)^(deplog#i));
             {deplog, depprod, indlog, indprod}
);
-------------------------------------------------------------------------
-- Compare "logs" entrywise
geqlog = (v, w) -> (
    for i from 0 to #v-1 do if v#i < w#i then return false; true
);
-------------------------------------------------------------------------
--equal dependent monomials , comparable independent monomials
pgeq = (a,b,depvars,indvars)->(
  if (logpoly(a,depvars,indvars))#0 == (logpoly(b,depvars,indvars))#0
  and geqlog((logpoly(a,depvars,indvars))#2,(logpoly(b,depvars,indvars))#2)
  then return true;false
);       
-------------------------------------------------------------------------
-- Reduction modulo \Delta^(q-1) times the previous module
red = (ee, I, dq, foot) -> (
   eold := ee;
   enew := 0;
   i := #foot-1;
   while i >= 0 and eold != 0 do (	
      enew = (eold % (dq*foot#i)) % I;
      if eold > enew then (
         eold = enew;
         i = #foot;
      );
      i = i-1;
   );
   enew % I
);
------------------------------------------------------------------------
-- repeated squaring to compute qth powers of a RingElement modulo I
fastq = (g, I) -> (
   q := char ring I;
   result := 1;
    -- Starting from the LSBit, multiply by appropriate powers of g.
    -- See any standard cryptography text for this.
   while q != 0 do (
      if q%2==1 then result = (result*g)%I;
        q = q//2;
        g = (g^2)%I;
    );
    result
);
-------------------------------------------------------------------------
--non-exported basic number-theoretic methods
-------------------------------------------------------------------------
-- the extended Euclidean algorithm
eea = (a,b)->(
     r:={a,b};
     u:={0,1};
     v:={1,0};
     i:=1;
     q:=0;
     while r#i!=0 do(
	  q=r#(i-1)//r#i;
	  r=append(r,r#(i-1)%r#i);
	  u=append(u,q*u#i+u#(i-1));
	  v=append(v,q*v#i+v#(i-1));
	  i=i+1;
     );
     (r,u,v)
);
----------------------------------------------------------------------
--the Chinese remainder theorem for two integers
crt = (c,d,p,n)->(
     (r,u,v):=eea(p,n);
     l:=((-c*u#-2*n+d*v#-2*p)*(-1)^(#r)) % (p*n);
     if 2*l > p*n then(
	   return l-p*n
     )
     else(
	  if 2*l <= -p*n then(
	       return l+p*n
	  )
          else return l
	  );   	   
     );
----------------------------------------------------------------------
--the Chinese remainder theorem for two polynomials 
polycrt = (poly1,poly2,mod1,mod2)->(
   temp1:=poly1;
   temp2:=poly2;
   poly3:=0;
   lm1:=0;lm2:=0;lm:=0;co3:=0;
   while temp1 !=0 do(
      lm1=leadMonomial(temp1);
      if temp2 !=0 then(
         lm2=leadMonomial(temp2);
         lm=max({lm1,lm2});
         co3=crt(coefficient(lm,temp1),coefficient(lm,temp2),mod1,mod2);
         poly3=poly3+co3*lm;
--	 print("1");print(poly3);
         temp1=temp1-coefficient(lm,temp1)*lm;
         temp2=temp2-coefficient(lm,temp2)*lm;
      )
      else(
         co3=crt(coefficient(lm1,temp1),0,mod1,mod2);
         poly3=poly3+co3*lm1;
--	 print("2");print(poly3);
         temp1=temp1-leadTerm(temp1);
      );
   );
   while temp2!=0 do(
      lm2=leadMonomial(temp2);
      co3=crt(0,coefficient(lm2,temp2),mod1,mod2);
         poly3=poly3+co3*leadMonomial(temp2);
--	 print("3");print(poly3);
         temp2=temp2-leadTerm(temp2);
   );
   poly3
);
--------------------------------------------------------------------------------
-- returns a pair of integers for the fraction a/b \equiv c mod N with a^2+b^2 minimal 
-- using intermediate results from the extended Euclidean algorithm
recon = (c,N)->(
   rold:=N;
   uold:=0;
   rnew:=c;
   unew:=1;
   rem:=0;
   u:=0;
   while ((unew)^2+(rnew)^2<(uold)^2+(rold)^2) do(
      rem=rold%rnew;
      u=-(rold//rnew)*unew+uold;
      uold=unew;
      unew=u;
      rold=rnew;
      rnew=rem;
   );
   {rold,uold}
);   
-------------------------------------------------------------------------------
--- reconstructing polynomials over Q from polynomials over Z------------------
--using the above function on each coefficient---------------------------------
polyrecon = (pol,md,rng)->(
   co:=(coefficients(pol))#1;
   -- list of coefficients as integers mod mo
   ent:=flatten entries(co);     
   -- pairs (numerator,denominator)          
   ap:=apply(ent,v->recon(sub(v,ZZ),sub(md,ZZ)));
   -- lift these to QQ
   app:=apply(ap, v->1/v#1*sub(v#0,rng));
   -- list of monomials
   appp:=apply( flatten (entries (coefficients(pol))#0),v->sub(v,rng));
   -- mons:=(coefficients(pol))#0;
   newpoly:=0;
   for i to #appp-1 do(
      newpoly=newpoly+appp#i*app#i;
   );
   newpoly
);
--------------------------------------------------------------------------------
--methods for defining monomial orderings from matrices
--------------------------------------------------------------------------------
--defining the grevlex-over-weight monomial ordering matrix
grevlexWeight = method();
grevlexWeight(Matrix) :=(matr)->(
grev := for i to numColumns(matr)-numRows(matr)-1 list (
           for j to numColumns(matr)-1 list(
	      if i+j<numColumns(matr)-numRows(matr) then 1 else 0
              )
           );
           matrix(grev)||matr
       );
--------------------------------------------------------------------------------
--defining the weight-over-grevlex monomial ordering matrix
--exported because it is used in the documentation examples
weightGrevlex = method();
weightGrevlex(Matrix) := (matr)-> (
   grev := for i to numColumns(matr)-numRows(matr)-1 list (
              for j to numColumns(matr)-1 list(
	         if i+j < numColumns(matr)-numRows(matr) then 1 else 0
                 )
              );
              matr||matrix(grev)
          );
--------------------------------------------------------------------------------
--defining a ring with a monomial ordering given by a matrix
--not just from the two types of matrices defined above
matrixOrder := (field,matr)-> (
     field[Variables=>numColumns matr, Weights=>entries matr]
);   
--------------------------------------------------------------------------------
--weights from a matrix
wt := (F,M) -> (
     for i to numRows(M)-1 list 
        (weightRange(flatten entries M^{i},F))#1
     );

--------------------------------------------------------------------------------
-- Exported methods -----------------------------------------------------------
--------------------------------------------------------------------------------
--Test the weight matrix for compatibility with the Gr\'obner basis generators
--All outputs should be true

testWeightMatrix = method(TypicalValue => List);
testWeightMatrix(Matrix,Ring,List) := (wtr,r,GB) ->(
   for i to #GB-1 list(
      pol:=GB#i;
      lp:=leadTerm(pol);
      lq:=leadTerm(pol-lp);
      mp:=transpose matrix{apply(#gens r,i->degree((gens r)#i,lp))};
      mq:=transpose matrix{apply(#gens r,i->degree((gens r)#i,lq))};
      wtr*mp==wtr*mq
      )	  
);
---------------------------------------------------------------
-- Find a conductor element D for ic(R) over R
-- lying in the given Noether normalization P of R 
-- by column-reducing the transpose of the Jacobian matrix

qthConductor = method(TypicalValue => RingElement);
qthConductor(Ideal,ZZ) := (I,depno) -> (
     R := ring I;
    RP := (coefficientRing R)[gens R,MonomialOrder=>{Position=>Up,{depno,#gens R-depno}}];
    IP := sub(I,RP);
    GP := gens gb (transpose jacobian IP|matrix{{gens IP}}**identity(RP^(numColumns(jacobian IP))));
    depvars:=take(gens RP,depno);
    indvars:=take(gens RP,depno-#gens RP);
    qthconductor := 1;
    rowconductor := 0;
    j := numColumns(GP)-1;
    i := numRows(GP)-1;
    while i >= 0 and j >= 0 do(
       while i>=0 and j>=0 and (GP_(i,j) == 0 or (logpoly(GP_(i,j),depvars,indvars))#1 != 1) do(
          j = j-1;
       );
       rowconductor = 0;
       while i >= 0 and j >= 0 and GP_(i,j) != 0 and (logpoly(GP_(i,j),depvars,indvars))#1 == 1 do(
	  rowconductor=gcd(rowconductor,GP_(i,j));
	  j = j-1;
       );
       if rowconductor != 0 then qthconductor = qthconductor*rowconductor;
       i = i-1; 
    );
    use R;
    sub(qthconductor,R)
);     
--------------------------------------------------------------------------------
-- Find the numerators of the fractions defining the P-module presentation 
-- of the integral closure of a quotient ring 
-- using the Qth-power algorithm,
-- the zeroth numerator also being the common denominator, 
-- a conductor element for the integral closure lying in the given Noether normalization, P.
icRfractions = method();
icRfractions (Ideal,ZZ,List) := (I,depno,foot) ->(
   -- Initialization
              R := ring I;
          indno := #gens R -depno;
        depvars := take(gens R, depno);
        indvars := take(gens R, -indno);
              q := char R;
              J := ideal(1_R);
         Deltaq := qthConductor(I,depno);
             fa := factor Deltaq;
   for a to #fa-1 do(
          delta := ((fa#a)#0)^((fa#a)#1); 
             dq := delta^(q-1);   
              g := foot;
              h := apply(g, v->fastq(v,I));
              e := apply(h, v->red(v,I,dq,foot));
   modulenumber := -1;
     numerators := foot;
      -------------------------------------------------------------------------    
      -------------------------------------------------------------------------    
      -- Compute the next module generating set; 
      -- continue to loop as long as different modules are generated.
      skip := true;
      loop := true;
      while loop == true do(       	
         loop = false;
         i1 := 0;
         while i1 < #g do(
	    skip = false;
	    if i1 != 0 
	    and leadMonomial g#i1 == leadMonomial g#(i1-1) then(  	   	
               skip = true;
	    );
            if i1 !=0 then(
	       for j1 to i1-1 do(
	          if e#(i1-1-j1) == 0 
		  and pgeq(g#i1,g#(i1-1-j1),depvars,indvars) then(
		     skip = true;
		     break;
	          );
	       );
            );    
	    if skip == true then(   
	       g = drop(g,{i1,i1});
	       h = drop(h,{i1,i1});
	       e = drop(e,{i1,i1});
	       i1 = i1-1;
	    );
            if skip == false then(
               if e#i1 != 0 then(
                  j1 := #g-1;
                  while j1 > i1 and e#j1 != 0 do(
                     if pgeq(g#j1,g#i1,depvars,indvars) 
		     and pgeq(e#j1,e#i1,depvars,indvars) then(  
                        g = drop(g,{j1,j1});
		        h = drop(h,{j1,j1}); 
                        e = drop(e,{j1,j1});
                     );
                     j1 = j1-1;
                  );
               );      	      
	       --removal-----------------------
	       j2 := #g-1;
	       while j2 > i1 do(
	          if pgeq(g#j2,g#i1,depvars,indvars) then(
		     g = drop(g,{j2,j2});
		     h = drop(h,{j2,j2});
		     e = drop(e,{j2,j2});
	          );
	          j2 = j2-1;
	       );
               -- "row-reduction", that is, taking F-linear combinations
               j3 := 0;
               while j3 < i1 and e#i1 != 0 do(
                  if e#j3 != 0 then(
                     log1 := logpoly(e#i1,depvars,indvars);
                     log2 := logpoly(e#j3,depvars,indvars);	
                     if pgeq(e#i1,e#j3,depvars,indvars) 
		     and apply((log1#2-log2#2), v->v%q)==apply(indno, v->0) 
		     then(
		        prod1 := 1;
		        for k1 to indno-1 do( 
			   prod1 = prod1*(indvars#k1)^((log1#2-log2#2)#k1//q)
		        );
		        loop = true;
		        if g#i1 > (g#j3)*prod1 then(
		           lc := leadCoefficient(e#i1)//leadCoefficient(e#j3);
                           g1 := g#i1-lc*prod1*g#j3;
			   h1 := h#i1-lc*prod1^q*h#j3;
			   e1 := e#i1-lc*prod1^q*e#j3;
			    g  = replace(i1,g1,g);
			    h  = replace(i1,h1,h);
                           m1 := #numerators-1;
			   while m1 >= 0 and e1 != 0 do(
			      if pgeq(e1,dq*numerators#m1,depvars,indvars) then(
			         e1 = e1-leadTerm(e1)//leadTerm(dq*numerators#m1)*dq*numerators#m1; 
			         m1 = #numerators;
			      );
		              m1 = m1-1;
			   );
		           e = replace(i1,e1,e);
                           j3 = -1; 
	    	        )
		        else(
		           if g#i1 < (g#j3)*prod1 then(
                              g2 := (g#j3)*prod1;
                               g  = sort(append(g,g2));
		            pos1 := position(g, a->a==g2);
                              m2 := #numerators-1;
			      h2 := prod1^q*h#j3;
			       h  = insert(pos1,h2,h);
			      e2 := prod1^q*e#j3;
			      while m2 >= 0 and e2 != 0 do(
			         if pgeq(e2,dq*numerators#m2,depvars,indvars) then(
			            e2 = e2-leadTerm(e2)//leadTerm(dq*numerators#m2)*dq*numerators#m2; 
			             m2 = #numerators;
			         );
		                 m2 = m2-1;
			      );
		              e = insert(pos1,e2,e);
                           );
		        );   
                     );
                  );
                  j3 = j3+1;
	       );
               if e#i1 == 0 then( 
	          j4 := #g-1;
	          while j4 > i1 do(
		     if pgeq(g#j4,g#i1,depvars,indvars) then( 
		        g=drop(g,{j4,j4});
		        h=drop(h,{j4,j4});
		        e=drop(e,{j4,j4});
		     );         
	             j4 = j4-1;
	          );
	       )
               else( 
	       );   
	       -- shifting, that is multiplication by a free variable	       
               if e#i1 != 0 then(
                  loop = true;   
                  log3 := logpoly(e#i1,depvars,indvars);
	          for j5 to #numerators-1 do(
	             log4 := logpoly(dq*numerators#j5,depvars,indvars);
	             if log4#1 == log3#1 then(
	                prod2 := 1;
			mx := 0;
	                for k2 to indno-1 do(
		           mx = -((-(((log4)#2)#k2-((log3)#2)#k2))//q);
		           if mx > 0 then( prod2 = prod2*(indvars#k2)^mx);
		        );
	                if all(apply(#g,l->g#l != 0 and leadMonomial(g#l) == leadMonomial(g#i1*prod2)), l->l==false) 
		        then(
                           g3 := (g#i1)*prod2;
                         pos2 := position(sort(append(g,g3)),a->a==g3);
                            g  = insert(pos2,g3,g);
			   h3 := (h#i1)*prod2^q;
			    h  = insert(pos2,h3,h);
		           m3 := #numerators-1;
			   e3 := (e#i1)*prod2^q;
			   while m3 >= 0 and e3 != 0 do(
			      if pgeq(e3,dq*numerators#m3,depvars,indvars) then(
	                         e3 = e3-leadTerm(e3)//leadTerm(dq*numerators#m3)*dq*numerators#m3;
			          m3 = #numerators;
			      );
		              m3 = m3-1;
			   );
		           e = insert(pos2,e3,e);
                           loop = true;
                        );
	             );          
	          );
	          k3 := 0;
                  while k3 < #g do(
	             if g#k3 != 0 and e#k3 != 0 then(
	                log5 := logpoly(e#k3,depvars,indvars);
	                if k3 < i1 and log5#0 == log1#0 then(
	                   if all(apply((log1#2-log5#2), v->v%q), v->v==0) then(
	                      ww := apply(indno, v->degree(indvars#v,lcm(log1#3,log5#3)//log1#3));
                           prod3 := product(#ww, l->(indvars#l)^((ww#l)//q));
                              g4 := (g#i1)*prod3;
                               g  = sort(append(g,g4));
                            pos3 := position(g,a->a == g4);
                              m4 := #numerators-1;
                              h4 := (h#i1)*prod3^q;
			       h  = insert(pos3,h4,h);
			      e4 := (e#i1)*prod3^q;
			      while m4 >= 0 and e4 != 0 do(
			         if pgeq(e4,dq*numerators#m4,depvars,indvars) then(
	                            e4 = e4-leadTerm(e4)//leadTerm(dq*numerators#m4)*dq*numerators#m4; 
			            m4 = #numerators;
			         );
		                 m4 = m4-1;
			      );
		              e = insert(pos3,e4,e);
                              loop = true;
                           );
	                );
	             );
	             k3 = k3+1;
                  );
	       );
            );
            i1 = i1+1;
         );
         --initialize next P-module generator
         modulenumber = modulenumber+1;
         --remove any elements that are zeroed out  
         poslist := positions(g, v->v!=0);
         g = g_poslist;
         h = h_poslist;
         e = e_poslist;
         poslist = positions(e, v->v==0);
         --save only elements describing the next module 
         g = g_poslist;
         h = h_poslist;
         e = e_poslist;
         numerators = g;
         i2 := #numerators-1;
         while i2 > 0 do(
            for j6 to i2-1 do(
               if pgeq(numerators#i2,numerators#j6,depvars,indvars) then(
                  numerators = delete(numerators#i2,numerators);
	          break;
               );
            );
            i2 = i2-1;
         );
         numerators=for i3 to #numerators-1 list(  
            head1 := leadTerm(numerators#i3);
            tail1 := numerators#i3-head1;
	    j7 := i3-1;
	    while j7 >= 0 and tail1 != 0 do(
	       w3 := tail1%numerators#j7;    
	       if tail1 > w3  then(
	          tail1 = w3;
	          j7 = i3;
	       );
               j7 = j7-1;
	    );   	
            (head1+tail1) % I
         );
         numerators=for i4 to #numerators-1 list if numerators#i4 !=0 then numerators#i4;
         numerators=delete(,numerators);
         e= apply(h,v->red(v,I,dq,numerators));
collectGarbage;	 
      );
      J=intersect(J,ideal(numerators));
   );
   J1 := first entries gens gb J;
   J2 := sort flatten for i5 to #foot-1 list for j8 to #J1-1 list (foot#i5)*(J1#j8) % I;
   J3 := for i6 to #J2-1 list if J2#i6 != 0 and member((logpoly(J2#i6,depvars,indvars))#1,foot) then J2#i6 else continue;
   J4 := unique J3;
   i7 := #J4-1;
   while i7 > 0 do(
      j9 := i7-1;
      while j9 >= 0 do(
         if pgeq(J4#i7,J4#j9,depvars,indvars) then(
	    J4 = drop(J4,{i7,i7});
	    break;
         );
         j9 = j9-1;
      );
      i7 = i7-1;
   );   	 	   	
   J5 := sort(for i8 to #J4-1 list (
	        head2 := leadTerm(J4#i8); 
		tail2 := J4#i8-head2; 
		for j0 to i8-1 do 
		   tail2 = tail2 % J4#(i8-1-j0); 
		   head2+tail2
	     )
	);
   GCD := gcd(J5);
   apply(J5,v->v//GCD)
);

-------------------------------------------------------------------------------
--weight matrix from fractions
-------------------------------------------------------------------------------
icRweightmatrix = method(TypicalValue => Matrix);
icRweightmatrix (List,Matrix) := (fractions,matr) ->(
   revfrac := rsort(fractions);
   weightlist := for i to #revfrac-2 list( 
      wt(revfrac#i,matr)-wt(revfrac#-1,matr)
   );
   transpose matrix(weightlist)|matr_{numColumns(matr)-numRows(matr)..numColumns(matr)-1}   
);
-------------------------------------------------------------------------------
-- polynomial ring for the integral closure presentation
-------------------------------------------------------------------------------

icRring = method(TypicalValue => Ring);
icRring(List,Matrix,Ring) := (fractions,wtR,Rq) -> 
   matrixOrder(coefficientRing Rq,grevlexWeight(icRweightmatrix(fractions,wtR))
);

-------------------------------------------------------------------------------
--induced relations for the integral closure presentation
-------------------------------------------------------------------------------
icRrelations = method();
icRrelations(List,Ideal,Ring,ZZ) := (fractions,I,icR,depno)-> (
          Rq := ring I;
       indno := #gens Rq - depno;  
     depvars := take(gens Rq,depno);
     indvars := take(gens Rq,-indno);
       noeth := for i to #gens Rq-1 list if i < depno then 0 else icR_(#fractions+i-depno-1);
         psi := map(icR,Rq,noeth);
      revold := rsort(fractions);
   -- local variables----------------------------------------------------------
   relij := tailij := tailji := relji := GCD := l := k := 0;
   -- quadratic relations defining the multiplication over P
   quads := flatten( 
      for i to #fractions-2 list(
         for j from i to #fractions-2 list(
            relij = icR_i*icR_j;
            tailij = ((revold#i)*(revold#j))%I//(revold#-1);
            k = #fractions-1;
            while tailij != 0 and k >= 0 do(
	       if pgeq(tailij,revold#k,depvars,indvars) then(
	          l = leadTerm(tailij)//leadTerm(revold#k);
	          tailij = tailij-l*revold#k;
	          relij = if k == #fractions-1 then relij-psi(l) else relij-psi(l)*(icR_k);
	          k = #fractions-1;
	       )
               else(
	          k = k-1;
	       );
	    );
            relij
         )
      )	
   );
   -- linear relations defining any P syzygies
   lins := flatten( 
      for i to #fractions-2 list(
         for j from i+1 to #fractions-2 list(
	    if (logpoly(revold#i,depvars,indvars))#0 == (logpoly(revold#j,depvars,indvars))#0 then(
	       GCD = gcd(leadMonomial(revold#i),leadMonomial(revold#j));	
	       tailji = (revold#i*leadTerm(revold#j))//GCD-(revold#j*leadTerm(revold#i))//GCD;
	       relji = icR_i*psi(leadTerm(revold#j)//GCD)-icR_j*psi(leadTerm(revold#i)//GCD);
               k = #fractions-2; 
               while tailji != 0 and k >= 0 do(
	          if pgeq(tailji,revold#k,depvars,indvars) then(
	             l = leadTerm(tailji)//leadTerm(revold#k);
	             tailji = tailji-l*revold#k;
	             relji = relji-psi(l)*(icR_k);
	    	     k = #fractions-2;
	          )
                  else(
	             k = k-1;
	          );
	       );
               relji
            )
         )	
      )
   );
   delete(0_icR,delete(null,quads|lins))
);
-------------------------------------------------------------------------------
-- ZZ/q version
-------------------------------------------------------------------------------
qthIntegralClosure = method();
qthIntegralClosure(Matrix,Ring,List) := (wtR,Rq,Iq) -> (
         q := char Rq;
	 if not q > 0 then error "q not positive"; 
     indno := numRows wtR;
     depno := (numColumns wtR) -indno;
   indvars := take(gens Rq, -indno);
   depvars := take(gens Rq, depno);
         I := ideal(Iq);
      degs := for dv in depvars list 
                max(apply(Iq,v->degree(dv,v)));
      foot := {1};
              for i to depno-1 do
	         if degs#i > 0 then(  
	            foot = flatten for j to degs#i-1 list 
		           for f in foot list 
			      f*(depvars#i)^j;
		           );		     
   LMideal := ideal(apply(Iq, v->leadMonomial v));
      foot  = sort(delete(0_Rq,for f in foot list f % LMideal));
      degr := #foot;
 fractions := icRfractions(I,depno,foot);
     wticR := icRweightmatrix(fractions,wtR);
      iccR := matrixOrder(coefficientRing Rq,grevlexWeight(wticR));
    relicR := icRrelations(fractions,I,iccR,depno);
       icR := ring(relicR#0);
   ---------------------------------------------------------------------------	
             (fractions,relicR,icR,wticR)
);
------------------------------------------------------------------------------
--QQ version
------------------------------------------------------------------------------
rationalIntegralClosure = method();
rationalIntegralClosure(Matrix,Ring,List) := (wtR,R0,I0) -> (
         if not coefficientRing(R0)===QQ then error "R0 not a ring over QQ";
           I := ideal(I0);
       indno := numRows(wtR);
       depno := numColumns(wtR)-indno;
    indvars0 := take(gens R0,-indno);
     indvars := indvars0;
    depvars0 := take(gens R0,depno);
     depvars := depvars0;
        degs := for dv in depvars0 list max(apply(I0,v->degree(dv,v)));
       foot0 := {1};
                for i to depno-1 do
	           if degs#i >0 then( 
	              foot0 = flatten for j to degs#i-1 list 
		      for f in foot0 list 
		         f*(depvars0#i)^j;
		      );
     LMideal := ideal(apply(I0, v->leadMonomial v));
       foot0  = sort(delete(0_R0,for f in foot0 list f % LMideal));
	foot := foot0;
        degr := #foot0;
          RZ := ZZ(monoid R0);
         toQ := map(R0,RZ);
   ---------------------------------------------------------------------------
   --uninteresting local variables--------------------------------------------
   newrelsZ:=newvarsZ:=oldrelsZ:=oldvarsZ:=icrZ:=icRZ:=0;
   newrelsQ:=newvarsQ:=oldrelsQ:=oldvarsQ:=icrQ:=icRQ:=0;
   oldmod:=0;
   ---------------------------------------------------------------------------
         D0 := qthConductor(I,depno);
	 D0  = D0/leadCoefficient(D0);
	 print(D0);
   ---------------------------------------------------------------------------
   --find primes that definitely can't be used--------------------------------
   badprimes := 2*wtR_(0,0);
   for f in I0 do(
      badprimes = badprimes*lcm(apply(apply(flatten entries ((coefficients(f))#1),v->substitute(v,QQ)),v->denominator(v))); );
    ---------------------------------------------------------------------------
   -- look for good primes, 
   -- and run the qth power algorithm using ZZ/q instead of QQ
        q := 2;
   firstPrime := true;
   stable := false;
   while stable == false do(
      stable = true;
      while isPrime(q) == false or badprimes % q == 0 do(
         q = q+1; 
      );
	   Rq := (ZZ/q)(monoid R0);
          toq := map(Rq,R0);
	  toZ := map(RZ,Rq);
           Iq := I0/toq;
--         foot00 := foot0/toq;
--      depvars00 := depvars0/toq;
--      indvars00 := indvars0/toq;
           Jq := ideal(Iq);
   ----------------------------------------------------------------------------
           Dq := qthConductor(Jq,depno);
	   Dq=Dq/leadCoefficient(Dq);
           print(q,Dq);
   --q is good if D%q matches Dq-----------------------------------------------   
      if Dq == sub(D0,Rq) then(
	    print(q);
            d := Dq^(q-1);
         --this next part will be replaced by a call to the qth power method
         --with Iq, footq, etc..      
         (Numerators,relationsicRq,icRq,wticR) := 
               qthIntegralClosure(wtR,Rq,Iq);
	  print toString(relationsicRq);     	 
         ----------------------------------------------------------------------
         --if more than one good prime has been used, 
         --it is then necessary to use CRT and EEA----------------------------------
         if firstPrime == true then(
	      oldvarsZ = for i to #Numerators-1 list sub(Numerators#i,RZ);
                  icRZ = ZZ(monoid icRq);
	      oldrelsZ = for i to #relationsicRq-1 list sub(relationsicRq#i,icRZ);
              oldvarsQ = for i to #oldvarsZ-1 list polyrecon(oldvarsZ#i,q,R0);
	          icRQ = QQ(monoid icRZ);		 
	      oldrelsQ = for i to #oldrelsZ-1 list polyrecon(oldrelsZ#i,q,icRQ);
	        oldmod = q;
	       stable  = false;
	   firstPrime  = false;
         )
         else(
            --find variables and relations mod the product of all primes so far
	    newvarsZ = for i to #oldvarsZ-1 list 
	                  polycrt(oldvarsZ#i,sub(Numerators#i,RZ),oldmod,q);
	    newrelsZ = for i to #oldrelsZ-1 list 
	                  polycrt(sub(oldrelsZ#i,icRZ),sub(relationsicRq#i,icRZ),oldmod,q);
	      oldmod = oldmod*q;
	    newvarsQ = for i to #newvarsZ-1 list 
	                  polyrecon(newvarsZ#i,oldmod,R0);
	    newrelsQ = for i to #newrelsZ-1 list 
	                  polyrecon(newrelsZ#i,oldmod,icRQ);
            --Has this process stabilized yet or not?
	    if newvarsQ != oldvarsQ or newrelsQ != oldrelsQ  then(
	       oldvarsQ = newvarsQ;
	       oldrelsQ = newrelsQ;
	       oldvarsZ = newvarsZ;
	       oldrelsZ = newrelsZ;
	       stable = false;
	    )
            else(
	       Vars:=reverse(oldvarsQ);
	       Vars1:=for i to #Vars-2 list Vars#i/Vars#(#Vars-1);
	       m:=matrix{join(Vars1,take(gens(R0),-indno))};
	       phi:=map(frac(R0),icRQ,m);
	       j1:=apply(oldrelsQ,v->phi(v));
               II:=sub(I,frac(R0));
	       j2:=apply(j1,v->v%II);
	       for i to #j2-1 do(
		  if j2#i!=0 then(
		     oldvarsQ = newvarsQ;
	             oldrelsQ = newrelsQ;
	             oldvarsZ = newvarsZ;
	             oldrelsZ = newrelsZ;
	             stable = false;
		     break;
		  );
	       );
	    );	     
         );
         q = q+1;
      )
      else(
         q = q+1;
         stable = false;  
      );
   );
--   icR := matrixOrder(QQ,grevlexWeight(wticR));
   oldicR := ring(oldrelsQ#0);
   (oldvarsQ,oldrelsQ,oldicR,wticR)   
);
-------------------------------------------------------------------------------
-- possible change of Noether normalization 
-- to minimize the number of dependent variables needed
minimization = method();
minimization(List,List,Ring,Matrix) := (fractions,relicR,icR,wticR) -> (
     depno := #fractions -1;
         T := reverse(entries transpose wticR);
     indno := #gens icR -depno;
  minimize := false;
   ------------------------------------------------------
   I1:=I2:=I3:=I4:=a:=b:=0;
   ------------------------------------------------------                 
   for i to indno -1 when minimize == false do(
      for j from indno to #T -1 do(
	 if (T#j < T#i) and (apply(T#i,v->v*((T#j)#0)) == apply(T#j,v->v*((T#i)#0))) then(
	    a = i;
	    b = j;
	    minimize = true;
	    break;
	 );
      );
   );
   if minimize == true then(
      rels := apply(relicR, v -> sub(v,icR));
      wtR1 := transpose matrix(reverse(
	         for i to #T -1 list( 
		    if i == a then( 
		       T#b
		    )
		    else( 
		       if i == b then(
			  T#a
		       )
		       else(
			  T#i
		       )
		    )
                 )
	      )
	   );
      R1 := (coefficientRing icR)[Variables => #gens icR, Weights=>entries weightGrevlex(wtR1)]; 
      f := map(R1,icR,
	      for i to #T -1 list
		 if i == #gens(R1)-a-1 then R1_(#gens(R1)-b-1)
		 else if i == #gens(R1)-b-1 then R1_(#gens(R1)-a-1)
		 else R1_(i)
	   );
      I1 = first entries gens gb ideal apply(rels, v->f(v));
      I2 = for i to #I1-1 list if first degree(leadMonomial(I1#i)) != 1 then I1#i; 
      I3 = delete(,I2);
      I4 = apply(I3,v->sub(v,R1));
      return {wtR1,R1,I4}
   )
   else(
      return {wticR,icR,relicR}
   );   	   
);   


-------------------------------------------------------------------------------
----------------------  Documentation -----------------------------------------
-------------------------------------------------------------------------------
beginDocumentation()

document {
     Key => QthPower,
     ---------------------------------------------------------------------------
     -- PURPOSE : Compute the integral closure of a type I integral domain
     --          (and probably for even more general integral extensions)
     --           See Leonard 2001, Leonard and Pellikaan 2003, Leonard 2009.
     -- PROGRAMS : qthConductor
     --            qthIntegralClosure
     --            rationalIntegralClosure
     --            minimization
     -- wtR is a weight matrix for the input R/ideal(I) 
     -- with the last indno variables defining a Noether normalization P of R 
     -- qthConductor computes a conductor element in P
     -- icRfractions computes a list of numerators
     -- defining numerators for a P-module generating set of fractions for icR,  
     -- with the zero-th numerator also the common denominator, a conductor element in P 
     -- qthIntegralClosure and rationalIntegralClosure also return the ring icR, 
     -- and the induced relations for the ideal presenting the integral closure
     -- PROGRAMMERs : This implementation was written by and is maintained by
     --               Douglas A. Leonard.
     -- THANKS: Mark van Hoeij, David Cook II, and those who wrote IntegralClosure.m2
     --         and/or helped me understand some of Macaulay 2's philosophy  
     -- UPDATE HISTORY : 11 February 2010, 8 September 2010, 24 August  2011, 10 January 2104
     ---------------------------------------------------------------------------
     PARA {
	  "This package computes the integral closure of type I affine domains
	  and some slightly more general integral extensions of polynomial rings P 
	  using the qth-power algorithm from 
	  D.A.Leonard, 
	  Finding the missing functions for one-point AG codes, 
	  IEEE Trans. Inform.Theory, 47(6), 2001, pp. 2566-3573,
	  D.A.Leonard and R.Pellikaan,
	  Integral closures and weight functions over finite fields,
          Finite Fields and Their Applications 9(4), 2003, pp. 479-504,
	  D.A.Leonard,
	  A weighted module view of integral closures of affine domains of type I, 
	  Advances in Mathematics of Communication 3(1), 2009, pp. 1-11. 
	  ({\tt icFracP} in the {\tt IntegralClosure} package of {\tt Macaulay2} and {\tt normalP}
          in {\tt Singular}'s {\tt normal} package are attempts to generalize this to generic input
	  by ignoring all of the structure that is required by this package.) 
          Also this package contains the extension to examples over the rationals; 
	  which, in turn, allows for quicker answers over ZZ/q for most large q,
	  which can be produced if desired merely by changing the coefficient ring from QQ to ZZ/q."
	  }
     }
--------------------------------------------------------------------------------------------------------
document {
     Key => {
	  weightGrevlex, (weightGrevlex, Matrix)
	  },
     Headline => "transform a weight matrix into a monomial ordering matrix",
     Usage => "wtgrev=weightGrevlex(wtR)",
     Inputs => {
	  "wtR" => "a weight matrix"
	  },
     Outputs => { 
	  "wtgrev" => Matrix => "weight-over-grevlex matrix" 
	  },
     EXAMPLE lines ///
          wtR = matrix{{5,6,6},{3,6,0}};
          weightGrevlex(wtR) 
          ///,
     PARA {
	  "It is standard in other algebra systems 
	  to have a weighted monomial ordering based on 
	  one row of weights such as matrix{{5,6,6}} 
	  being extended to matrix{{5,6,6},{1,1,0},{1,0,0}},
	  whereas M2 would extend it to matrix{{5,6,6},{1,1,1},{1,1,0}}.
	  The method here allows for more than one independent row of weights
	  matrix{{5,6,6},{3,6,0}} to be extended to
	  matrix{{5,6,6},{3,6,0},{1,0,0}}.
	  Note that the number of rows necessairly matches the number of (free) variables,
	  those of P, since the rightmost square submatrix defines a monomial ordering on P."
	  }
     }	
----------------------------------------------------------------------
document {
     Key => {
	  grevlexWeight, (grevlexWeight, Matrix)
	  },
     Headline => "transform a weight matrix into a monomial ordering matrix",
     Usage => "wtgrev=grevlexWeight(wtR)",
     Inputs => {
	  "wtR" => "a weight matrix"
	  },
     Outputs => { 
	  "wtgrev" => Matrix => "grevlex-over-weight matrix" 
	  },
     EXAMPLE lines ///
          wtR = matrix{{5,6,6},{3,6,0}};
          grevlexWeight(wtR) 
          ///,
     PARA {
	  "It is not standard in algebra systems 
	  to have a weighted monomial ordering based on 
	  one row of weights such as matrix{{5,6,6}} 
	  extended to matrix{{1,1,0},{1,0,0},{5,6,6}},
	  While M2 could extend it to matrix{{1,1,1},{1,1,0},{1,0,0},{5,6,6}}
	  fairly easily, the method here allows for more than 
	  one independent row of a matrix
	  matrix{{5,6,6},{3,6,0}} to be extended to
	  matrix{{1,0,0},{5,6,6},{3,6,0}}."
	  }
     }	
----------------------------------------------------------------------------
document {
     Key => {
	  testWeightMatrix, (testWeightMatrix, Matrix, Ring, List)
	  },
     Headline => "test compatibility of weight matrix with Groebner basis elements",
     Usage => "L=testWeightMatrix(wtr,r,GB)",
     Inputs => {
	  "wtr" => "weight matrix",
	  "r" => "polynomial ring with weight-over-grevlex order",
	  "GB" => "list of generators for the ideal of relations"
	  },
    Outputs => {
	  "L" => "list of boolean checks"
          },
     EXAMPLE lines ///
          wtr=matrix{{5,6,6},{2,6,0}};
	  r=ZZ/2[x,y,z,Weights=> entries weightGrevlex(wtr)];
	  GB={x^6+x^3*z-y^3*z^2};
	  L=testWeightMatrix(wtr,r,GB)
	  ///,
	PARA {
	     "each test should be true if the weight matrix is compatible"
        }   
     }	           
----------------------------------------------------------------------------
document {
     Key => {
	  qthIntegralClosure, (qthIntegralClosure, Matrix, Ring, List)
	  },
     Headline => "computes integral closures in positive characteristic",
     Usage => "(fractions,relicR,icR,wticR) = qthIntegralClosure(wtR,Rq,GB)",
     Inputs => {
	  "wtR" => "weight matrix",
	  "Rq" => "polynomial ring over ZZ/q with weight-over-grevlex order",
	  "GB" => "set of generators for the ideal of relations"
	  },
     Outputs => {
	  "fractions" => List => "numerators of P-module generating set, 
	        the first doubling as a common denominator, hence a conductor element as well", 
	  "relicR" => List => "a Gr\"obner basis for the ideal of induced relations",
          "icR" => Ring => "grevlex-over-weight polynomial ring over ZZ/q for the presentation",
	  "wticR" => Matrix => "weight matrix for the integral closure"
       	  },
     EXAMPLE lines ///
           wtR = matrix{{5,6,6},{3,6,0}}; 
           Rq = ZZ/23[x,y,z,Weights=>entries weightGrevlex(wtR)];
           GB = {x^6+x^3*z-y^3*z^2};
      	   (fractions,relicR,icR,wticR) = qthIntegralClosure(wtR,Rq,GB)
          ///,
     SeeAlso => {icFractions,integralClosure,icFracP},  
     PARA {
     	  "The presentation is therefore a quotient ring, icR (
	  with grevlex-over-weight monomial ordering implicit from wticR)
          modulo the ideal, relicR, of induced relations that define the
	  P-algebra multiplication and possible P-linear dependencies.
	  The fractions returned could be used to define a map from
	  (fractions#0)icR to Rq.
	  Note that if 
	  wtR*matrix{{6},{0},{0}}\neq max{ wtR*matrix{{3},{0},{1}},wtR*matrix{{0},{3},{2}}
	  in the example above, the algorithm will undoubtedly fail at some step."
     	  }
     }	   
-----------------------------------------------------------------------------------
 document {
     Key => {
	  rationalIntegralClosure, (rationalIntegralClosure, Matrix, Ring, List)
	  },
     Headline => "computes integral closures over the rationals",
     Usage => "(fractions,relicR,icR,wticR) = rationalIntegralClosure(wtR,Rq,GB)",
     Inputs => {
	  "wtR" => {"weight matrix"},
	  "R0" => {"polynomial ring over QQ with weight-over-grevlex order"},
	  "GB" => {"set of generators for the ideal of relations"}
	  },
     Outputs => {
	  "fractions" => List => "a list of numerators for a P-module generating set,
	         the first doubling as a common denominator, hence a conductor element as well", 
	  "relicR" => List => "a Gr\"obner basis for the ideal of induced relations",
          "icR" => Ring => "a grevlex-over-weight polynomial ring over QQ for the presentation", 
	  "wticR" => Matrix => " weight matrix for the integral closure" 
	  },
     EXAMPLE lines ///
           wtR = matrix{{11,6}};
           R0 = QQ[y,x,Weights=> entries weightGrevlex(wtR)];
           GB = {(y^2-3/4*y-15/17*x)^3-9*y*x^4*(y^2-3/4*y-15/17*x)-27*x^11};
           (fractions,relicR,icR,wticR) = rationalIntegralClosure(wtR,R0,GB)
          ///,
     SeeAlso =>{ icFractions, integralClosure},
     PARA {
     	  "This is just like qthIntegralClosure except over QQ instead of ZZ/q. 
	   It calls qthIntegralClosure for several small primes q and reconciles the results
	   using the Chinese Remainder Theorem and the extended Euclidean algorithm."
     	  }
     }	   
 -----------------------------------------------------------------------------------
 document {
     Key => {
	  qthConductor, (qthConductor, Ideal, ZZ)
	  },
     Headline => "computes a conductor element which also lives in the given Noether normalization, P",
     Usage => "qthConductor(I,depno)",
     Inputs => {
	  "I" => {"ideal of relations"},
	  "depno" => {"number of dependent variables"}
	  },
     Outputs => {
	  "delta"=> RingElement => {"a conductor element"}
	  },
     EXAMPLE lines ///
           wtR = matrix{{12,5}};
           Rq = ZZ/2[y,x,Weights=> entries weightGrevlex(wtR)];
           Iq = {y^5+y^2*(x^4+x)+y*x^2+x^12};
           I  = ideal(Iq);
           depno = (numColumns wtR) -(numRows wtR);
           delta = qthConductor(I,depno)
          ///,
     PARA {
     	  "This gives a canonical conductor element {\tt delta} 
	  living in the given Noether normalization, P,
	  the subring of the last numRows(wtR) (free) variables."
     	  }
     }	   

-----------------------------------------------------------------------------------
 document {
     Key => {
	  minimization, (minimization, List, List, Ring, Matrix)
	  },
     Headline => "change to a better Noether normalization suggested by the induced weights",
     Usage => "(newwticR,newicR,newrelicR) = minimization(fractions, relicR, icR, wticR)",
     Inputs => {
	  "fractions" => {"ring elements defining a P-module generating set for icR"},
          "relicR" => {"a Gr\"obner basis for the relation ideal"},
	  "icR" => {"a ring"},
	  "wticR" => {"a weight matrix"}
	  },
     Outputs => { 
	  "newwticR" => Matrix => "weight matrix", 
	  "newicR" => Ring => "ring", 
	  "newrelicR" => List => "Gr\"obner basis of relations"
	  },
     EXAMPLE lines ///
          wtR = matrix{{31,12}};
           Rq = ZZ/2[y,x,Weights=>entries weightGrevlex(wtR)];
    	   Iq = {y^12+y^11+y^10*x^2+y^8*x^9+x^31};
          ic1 = qthIntegralClosure(wtR,Rq,Iq)
          ic2 = minimization(ic1); toString ic2
    	  ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)
	  R1=ZZ/2[f38,f31,f12,f5,Weights=> entries weightGrevlex(matrix{{38,31,12,5}})];
	  phi1=map(R1,ic2#1,matrix{{0,0,f38,0,f31,0,0,0,0,0,f12,f5}});
	  I1=ic2#2/(v->phi1(v))
          G1=transpose gens gb ideal I1
          R2=ZZ/2[f38,f31,f24,f12,f5,MonomialOrder=>{Weights=>{1,1,1,1,0},Weights=>{1,1,1,0,0},Weights=>{1,1,0,0,0},Weights=>{1,0,0,0,0},Weights=>{38,31,24,12,5}}];
	  phi2=map(R2,R1,matrix{{f38,f31,f12,f5}});
	  I2=ideal((flatten entries G1)/(v->phi2(v)))+ideal(f12^2-f24)
          G2=transpose gens gb I2
	  ///,
     PARA {
     	  "Minimization changes the order of the variables
	  to impicitly change to a new Noether normalization, but doesn't
	  actually recompute the integral closure presentation
	  as a strict affine algebra over the new Noether normalization.
	  Therefore a second call to an integral closure method is
	  required if the output of normalization doesn't suffice.
	  It does produce the three outputs needed as input to 
	  either qthIntegralClosure or rationalIntegralClosure.
	  I don't necessarily like this second call.
	  At the current time it is easier to use the output of minimization
	  to produce manually (as above) a good minimization."
     	  }
     }	
------------------------------------------------------------------------------
------------------------------------------------------------------------------
TEST ///
------------------------------------------------------------------------------
-- SINGULAR example 3.7.3 modified slightly to make it more interesting
------------------------------------------------------------------------------
    wtR1 = matrix{{5,6,6},{3,6,0}};
      Rq = ZZ/109[x,y,z,Weights=>entries(weightGrevlex(wtR1))];
      GB = {x^6+x^3*z-y^3*z^2};
time icq = qthIntegralClosure(wtR1,Rq,GB); toString(icq)
assert(icq#3 == matrix{{10,9,8,7,5,6,6},{6,9,6,3,3,6,0}})
      R0 = QQ[x,y,z,Weights=>entries(weightGrevlex(wtR1))];
      GB = {x^6+x^3*z-y^3*z^2};
time ic0 = rationalIntegralClosure(wtR1,R0,GB); toString(ic0)
assert(ic0#3 == matrix{{10,9,8,7,5,6,6},{6,9,6,3,3,6,0}})
///

TEST ///
------------------------------------------------------------------------------
--example 1, disguised Hermitian----------------------------------------------
-- so, in need of minimization------------------------------------------------
------------------------------------------------------------------------------
wtR2 = matrix{{9,8}};
      Rq = ZZ/109[y,x,Weights=>entries(weightGrevlex(wtR2))];
      GB = {y^8-x^9+2*y*x^6-y^2*x^3};
time ic1 = qthIntegralClosure(wtR2,Rq,GB);toString(ic1)
time ic2 = minimization(ic1); toString(ic2)
time ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2); toString(ic3)
assert( ic3#3 == matrix{{15,10,5,4}})
      R0 = QQ[y,x,Weights=>entries(weightGrevlex(wtR2))];
      GB = {y^8-x^9+2*y*x^6-y^2*x^3};
time ic1 = rationalIntegralClosure(wtR2,R0,GB); toString(ic1)
time ic2 = minimization(ic1); toString(ic2)
time ic3 = rationalIntegralClosure(ic2#0,ic2#1,ic2#2); toString(ic3)
assert(ic3#3 == matrix{{15,10,5,4}})
///

TEST ///
------------------------------------------------------------------------------
--pseudo-weights--------------------------------------------------------------
------------------------------------------------------------------------------
    wtR3 = matrix{{7,4}};
      R0 = QQ[u,r,Weights=>entries weightGrevlex(wtR3)];
      GB = {u^7-u^3*r^7-r^3};
time ic0 = rationalIntegralClosure(wtR3,R0,GB); toString(ic0)
assert(ic0#3==matrix{{34,27,24,17,14,7,4}})
///

TEST ///
------------------------------------------------------------------------------
--generic example over QQ with moderate coefficients to be reconstructed------
------------------------------------------------------------------------------ 
    wtR4 = matrix{{11,6}};
      R0 = QQ[y,x,MonomialOrder=>{Weights=>{11,6},Weights=>{1,0}}];
      GB = {(y^2-3/4*y-15/17*x)^3-9*y*x^4*(y^2-3/4*y-15/17*x)-27*x^11};
time ic0 = rationalIntegralClosure(wtR4,R0,GB); toString(ic0)
assert(ic0#3 == matrix{{25,21,20,11,10,6}})
///

TEST ///
------------------------------------------------------------------------------
--example 7, Leonard 2009 with a presentation that is not free as a P-module--
------------------------------------------------------------------------------
    wtR5 = matrix{{19,15,12,9,9},{12,9,9,9,0}};
      Rq = ZZ/2[z19,y15,y12,x9,u9,Weights=>entries(weightGrevlex(wtR5))];
      GB = {y15^2+y12*x9*u9,
           y15*y12+x9^2*u9+x9*u9^2+y15,
           y12^2+y15*x9+y15*u9+y12,
           z19^3+z19*(y15+y12)*(x9+1)*u9+(y15*(x9+u9)+y12*(x9*u9+1))*x9^2*u9};
time icq = qthIntegralClosure(wtR5,Rq,GB); toString(icq)
assert(icq#3 == matrix{{34,34,31,29,26,23,19,15,12,9,9},{30,21,21,24,24,15,12,9,9,9,0}})
///

TEST ///
------------------------------------------------------------------------------
--generic example with non-trivial minimization-------------------------------
-- M2's integralClosure seizes on flattenRing --------------------------------
-- icFracP takes 8873 seconds for even ZZ/5-----------------------------------
------------------------------------------------------------------------------
    wtR6 = matrix{{31,12}};
      Rq = ZZ/2[y,x,Weights=>entries(weightGrevlex(wtR6))];
      GB = {y^12+y^11+y^10*x^2+y^8*x^9+x^31};
time ic1 = qthIntegralClosure(wtR6,Rq,GB); toString(ic1)
time ic2 = minimization(ic1); toString(ic2)
time ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2); toString(ic3)
assert(ic3#3 == matrix{{38,31,24,12,5}})
///

TEST ///
------------------------------------------------------------------------------
--from Eisenbud-Neumann p.11: simplest poly with 2 characteristic pairs.------ 
--QQ[y,x]/(y^4-2*x^3*y^2-4*x^5*y+x^6-x^7)-------------------------------------
--written as though it is in (QQ[x])[y]--------------------------------------- 
--and without any thought of the underlying weights 7 and 4-------------------
------------------------------------------------------------------------------
    wtR7 = matrix{{7,4}};
      Rq = QQ[y,x,Weights=>entries weightGrevlex(wtR7)];
      GB = {y^4-x^7-4*y*x^5-2*y^2*x^3+x^6};
time ic1 = rationalIntegralClosure(wtR7,Rq,GB); toString(ic1)
time ic2 = minimization(ic1); toString(ic2)
assert(ic1#3==matrix{{3,2,1,4}})
///

TEST ///
------------------------------------------------------------------------------
--from the IntegralClosure package--------------------------------------------
-- not type I, but amenable to grevlex weights--------------------------------
------------------------------------------------------------------------------
    wtR8 = matrix{{1,1}};
      R0 = QQ[v,u,Weights=>entries weightGrevlex(wtR8)];
      GB = {1/5*(5*v^6+7*v^2*u^4+6*u^6+21*v^2*u^3+12*u^5+21*v^2*u^2+6*u^4+7*v^2*u)};
time ic0 = rationalIntegralClosure(wtR8,R0,GB); toString(ic0)
assert(ic0#3 == matrix{{2,2,2,2,1,1}})
///

TEST ///
------------------------------------------------------------------------------
--MAGMA example found in GLS--------------------------------------------------
-- genus 1, but try to figure that out from most presentations-----------------
--(x-y)*x*(y+x^2)^3-y^3*(x^3+x*y-y^2)=0---------------------------------------
--change of variables gives a type I problem----------------------------------
------------------------------------------------------------------------------
    wtR9 = matrix{{11,7}};
      Rq = ZZ/109[w,z,Weights => entries weightGrevlex(wtR9)];
      GB = {w^7+3*w^6*z+w^6+3*w^5*z^3+6*w^5*z^2+9*w^4*z^4+4*w^3*z^6-w^3*z^5-3*w^2*z^7-3*w*z^9-z^11};
time ic1 = qthIntegralClosure(wtR9,Rq,GB); toString(ic1)
time ic2 = minimization(ic1); toString(ic2)
time ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2); toString(ic3)
assert(ic3#3 == matrix{{3,2}})
///

TEST ///
---------------------------------------------------------------------------------
-- Example I_3 from the Normalization paper by the Singular people, not type I---
-- has currently large numerators and denominators using the deJong approach-----
---------------------------------------------------------------------------------
   wtR10 = matrix{{1,1}};
      R0 = QQ[y,x,Weights=>entries weightGrevlex(wtR10)];
      GB = {y^9+y^8*(x+1)+y^5+y^4*x+y^3*x^2+y^2*x^3+y*x^8+x^9}
time ic0 = rationalIntegralClosure(wtR10,R0,GB)
assert(ic0#3==matrix{{4,4,4,4,3,2,2,1,1}})
-------------------------------------------------------------------------------
   wtR11 = matrix{{7,6}};
      Rq = ZZ/2[y,x,Weights => entries weightGrevlex(wtR11)];
      GB = {y^6+y^4*x+y^2*(x^3+x^2)+(x^7+x^4+x^3)};
time ic1 = qthIntegralClosure(wtR11,Rq,GB); toString(ic1)
time ic2 = minimization(ic1); toString(ic2)
time ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2); toString(ic3)
assert(ic3#3==matrix{{5,4,3}})
------------------------------------------------------------------------------
--   wtR12 = matrix{{5,1,1,1,1},{4,1,1,1,0},{4,1,1,0,0},{2,1,0,0,0}};
--      Rq = QQ[w,x,z,y,v,Weights=>entries weightGrevlex(wtR12)];
--      GB = {w^2+w*v*y^4+v^2*x^4*z^4};
--time ic1 = rationalIntegralClosure(wtR12,Rq,GB); toString(ic1)
------------------------------------------------------------------------------
--possibly too large an example unless separated------------------------------
wtR13 = matrix{{25,21}};
   Rq = ZZ/2[y,x,Weights=>entries(weightGrevlex(wtR10))];
   GB = {
         y^21
        +y^20* x
        +y^18*(x^3+x+1)
        +y^17*(x^3+1)
        +y^16*(x^4+x)
        +y^15*(x^7+x^6+x^3+x+1)
        +y^14* x^7
        +y^13*(x^8+x^7+x^6+x^4+x^3+1)
        +y^12*(x^9+x^8+x^4+1)
        +y^11*(x^11+x^9+x^8+x^5+x^4+x^3+x^2)
        +y^10*(x^12+x^9+x^8+x^7+x^5+x^3+x+1)
        +y^9* (x^14+x^13+x^10+x^9+x^8+x^7+x^6+x^3+x^2+1)
        +y^8* (x^13+x^9+x^8+x^6+x^4+x^3+x)
        +y^7* (x^16+x^15+x^13+x^12+x^11+x^7+x^3+x)
        +y^6* (x^17+x^16+x^13+x^9+x^8+x)
        +y^5* (x^17+x^16+x^12+x^7+x^5+x^2+x+1)
        +y^4* (x^19+x^16+x^15+x^12+x^6+x^5+x^3+1)
        +y^3* (x^18+x^15+x^12+x^10+x^9+x^7+x^4+x)
        +y^2* (x^22+x^21+x^20+x^18+x^13+x^12+x^9+x^8+x^7+x^5+x^4+x^3)
        +y  * (x^23+x^22+x^20+x^17+x^15+x^14+x^12+x^9)
        +     (x^25+x^23+x^19+x^17+x^15+x^13+x^11+x^5)};

--this took 41 hours and 16 gb-----------------------------------------------
--ic1 = qthIntegralClosure(wtR10,Rq,GB)--------------------------------------
--ic2 = minimization(ic1);---------------------------------------------------
--ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)--------------------------------
--assert(ic3#3 == matrix{{16,15,13,12,11,10,7}}------------------------------
///
























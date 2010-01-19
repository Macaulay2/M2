
------------
-- Header --
------------
--newPackage(
--    "QthPower",
--    Version => "0.3", 
--    Date => "January 18, 2010",
--    Authors => {{Name => "Douglas Leonard",
--                 Email => "leonada@auburn.edu",
--                 HomePage => "http://www.dms.auburn.edu/~leonada"}},
--    Headline => "An implementation of the Qth-Power algorithm 
--                for computing the integral closure 
--		of an integral extension, R, 
--		of a ring, P, of free variables",
--    DebuggingMode => true
--)

--export {qthConductor, icRfractions, qthIntegralclosure, rationalIntegralClosure};

------------
--  Code  --
------------



------------------------------
-- non-exported helper methods 
------------------------------

-- Reduction modulo \Delta^(q-1) times the previous module.

--red = method(TypicalValue => RingElement);
--red (RingElement, Ideal, RingElement, List) 
red:= (g, I, dq, modfoot) -> (
   h := g;
   i := #modfoot-1;
   while i >= 0 and g != 0 do (
      h = (g % (dq*modfoot#i)) % I;
      if g != h then (
         g = h;
         i = #modfoot-1;
      );
      i = i-1;
   );
   g%I
);

-- repeated squaring to compute qth powers of a RingElement modulo I

--fastq = method(TypicalValue => RingElement);
--fastq (RingElement, Ideal) 
fastq := (g, I) -> (
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


-- dependent and independent "logs" and leading monomials of a polynomial.

--logpoly = method(TypicalValue => List);
--logpoly (RingElement, List, List) 
logpoly := (v, dep, ind) -> (
   lv := leadMonomial v;
   indlog := apply(#ind, i->degree(ind#i, lv));
   indprod := product(#ind, i->(ind#i)^(indlog#i));
   deplog := apply(#dep, i->degree(dep#i, lv));
   depprod := product(#dep, i->(dep#i)^(deplog#i));
   {deplog, depprod, indlog, indprod}
);

-- Compare "logs".

--geqlog = method(TypicalValue => Boolean);
--geqlog (List, List) 
geqlog := (v, w) -> (
    for i from 0 to #v-1 do if v#i < w#i then return false; true
);

----------------------------------------------
--non-exported basic number-theoretic methods
----------------------------------------------

--the extended Euclidean algorithm

--eea = method(TypicalValue => List);
--eea (ZZ,ZZ) 
eea :=(a,b)->(
     r:={a,b};
     u:={0,1};
     v:={1,0};
     i:=1;
     while r#i!=0 do(
	  q=r#(i-1)//r#i;
	  r=append(r,r#(i-1)%r#i);
	  u=append(u,q*u#i+u#(i-1));
	  v=append(v,q*v#i+v#(i-1));
	  i=i+1;
     );
     (r,u,v)
);

--the Chinese remainder theorem for two integers-------

--crt=method(TypicalValue=>RingElement)
--crt(ZZ,ZZ,ZZ,ZZ)
crt :=(c,d,p,n)->(
     (r,u,v):=eea(p,n);
     (-c*u#-2*n+d*v#-2*p)*(-1)^(#r)
     );

--the Chinese remainder theorem for two polynomials 

--polycrt=method(TypicalValue=>RingElement)
--polycrt(RingElement,RingElement,ZZ,ZZ)
polycrt :=(poly1,poly2,mod1,mod2)->(
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
         temp1=temp1-coefficient(lm,temp1)*lm1;
         temp2=temp2-coefficient(lm,temp2)*lm2;
      )
      else(
         co3=crt(coefficient(lm1,temp1),0,mod1,mod2);
         poly3=poly3+co3*lm1;
         temp1=temp1-leadTerm(temp1);
      );
   );
   while temp2!=0 do(
      co3=crt(0,coefficient(lm1,temp1),mod1,mod2);
         poly3=poly3+co3*leadMonomial(temp2);
         temp2=temp2-leadTerm(temp2);
   );
   poly3
);

--reconstructing fractions from reaminders mod N using the extended Euclidea algorithm

--recon:=method(TypicalValue=>List)
--recon(ZZ,ZZ)
recon :=(a,n)->(
   rold:=n;
   uold:=0;
   rnew:=a;
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

---reconstructing polynomials over Q from polynomials over Z
polyrecon:=(pol,md)->(
   co:=(coefficients(pol))#1;
   --list of coefficients as integers mod mo
   ent:=flatten entries(co);     
   --pairs (numerator,denominator)          
   ap:=apply(ent,v->recon(v,md));
   --lift these to QQ
   app:=apply(ap, v->toQ(v#0)/toQ(v#1));
   --list of monomials
   appp:=apply( flatten (entries (coefficients(pol))#0),v->toQ(v));
   mons:=(coefficients(pol))#0;
   newpoly:=0;
   for i to #mons-1 do(
      newpoly=newpoly+mons#i*app#i;
   );
   newpoly
);

-----------------------------------------------------
--non-exported methods for defining monomial orderings from matrices
-------------------------------------------------------

--defining the grevlex over weight monomial ordering matrix

--grevlexWeight=method(TypicalValue=>Matrix)
--grevlexWeight(Matrix)
grevlexWeight :=(matr)->(
grev:=for i to numColumns(matr)-numRows(matr)-1 list (
         for j to numColumns(matr)-1 list(
	    if i+j<numColumns(matr)-numRows(matr) then 1 else 0
            )
         );
         matrix(grev)||matr
     );

--defining the weight-over-grevlex monomial ordering matrix

--weightGrevlex=method(TypicalValue=>Matrix)
--weightGrevlex(Matrix)
weightGrevlex :=(matr)->(
grev:=for i to numColumns(matr)-numRows(matr)-1 list (
         for j to numColumns(matr)-1 list(
	    if i+j<numColumns(matr)-numRows(matr) then 1 else 0
            )
         );
         matr||matrix(grev)
     );


--defining a ring with a monomial ordering given by a matrix
--not just from the two types of matrices defined above

--matrixOrder=method(TypicalValue=>List)
--matrixOrder(Ring,Matrix)
matrixOrder :=(field,matr)->(
     field[Variables=>numColumns matr,Weights=>entries matr]
);   

--weights from a matrix

--wt = method()
--wt(RingElement, Matrix) 
wt := (F,M) -> (
     for i to numRows(M)-1 list 
        (weightRange(flatten entries M^{i},F))#1
     )


------------------------
-- Exportable methods --
------------------------

-- Find an element D in the Noether noramlization P of R 
-- also in the conductor of R,
-- with an option to provide that element for cases when this is called 
-- possibly unnecessarily by the qth-power method

--qthConductor = method(TypicalValue => RingElement);
--qthConductor (Ideal,ZZ) 
qthConductor := (I,depno) -> (
     R := ring I;
    RP := (coefficientRing R)[gens R,MonomialOrder=>{Position=>Up,{depno,#gens R-depno}}];
    IP := sub(I,RP);
    GP := gens gb sub(transpose jacobian IP, RP/IP);
    DP := 1;
   col := 0;
   for row to numRows GP-1 do(
      rowDP = 0;
      while GP_(row,col)==0 do col=col+1;
      while max apply(depno, v -> degree(RP_v, leadMonomial(GP_(row,col))))==0 do(
         rowDP = gcd(rowDP,GP_(row,col));
         col=col+1;
      );
      if rowDP!=0 then DP=DP*rowDP;
   );
   sub(DP,R)
   
);

-- Find the numerators of the fractions defining the P-module presentation 
-- of the integral closure of a quotient ring 
-- using the qth-power algorithm,
-- the first numerator being also the common denominator, an element of P
-- There is an option to provide your own conductor element

icRfractions = method(TypicalValue => List,Options=>{Delta=>null});
icRfractions (Ideal,ZZ,List) :=o-> (I,depno,footq) ->(
   -- Initialisation
   R := ring I;
   indno := #gens R -depno;
   depvars := take(gens R, depno);
   indvars := take(gens R, -indno);
   q := char R;
   Deltaq := if o.Delta===null then qthConductor(I,depno) else o.Delta;
   dq := Deltaq^(q-1);   
   g := footq;
   h := apply(g,s->fastq(s,I));
   e := apply(h, s->red(s,I,dq,footq));
   s := apply(#footq, i->-1);
   modulenumber := -1;
   numerators := footq;
   --------------------------------------------    
   -- local variables
   i := j := w := k := prod := mx := ll := ex := l := gx := pos := 0;
   skip := skip1 := updatng := true;
   logei := logj := logk := posl := pos2 := ww := {};
   -----------------------------------------------------    
   -- Compute the next module generating set; 
   -- continue to loop as long as different modules are generated.
   loop := true;
   while loop==true do(       	
      loop = false;
      i = 0;
      skip = false;
      while i < #g do(	   	
         if s#i==modulenumber then(	      
            updating=true;
            -- look for the next unique leading monomial
            while updating==true do(		 
               updating=false;
               for j to i-1 do(		    
                  if leadMonomial(g#i)==leadMonomial(g#j) then(
                     i=i+1;
                     updating=true;
                     break;
                  );
               );
            );
            e=replace(i,red(e#i,I,dq,numerators),e);	    
            -- "row-reduction", that is, taking F-linear combinations
            j=0;
            while j<i and e#i!=0 do(
               if s#j==modulenumber and e#j!=0 then(
                  logei=logpoly(e#i,depvars,indvars);
                  logj=logpoly(e#j,depvars,indvars);	
                  if e#j!=0 and logj#0==logei#0 and geqlog(logei#2,logj#2) and apply((logei#2-logj#2), v->v%q)==apply(indno, v->0) then(
		     w = 1;
		     for k to indno-1 do( w=w*(indvars#k)^((logei#2-logj#2)#k//q));
		     loop=true;
		     if g#i>g#j*w then(
		        lc=leadCoefficient(e#i)//leadCoefficient(e#j);
                        g=replace(i,g#i-lc*(g#j)*w,g);
                        h=replace(i,(h#i-lc*(h#j)*w^q)%I,h);
	                e=replace(i,red(e#i-lc*(e#j)*w^q,I,dq,numerators),e);
                        j=-1; 
			
	    	     )
		     else( 
		        if g#i<g#j*w then(
                           gx=(g#j)*w;
                           g=sort(append(g,gx));
			   pos=position(g, a->a==gx);
                           h=insert(pos,(h#j)*w^q,h);
                           e=insert(pos,red((e#j)*w^q,I,dq,numerators),e);
                           s=insert(pos,modulenumber,s);
		        );   
                     );
                  );
               );
               j=j+1;
	    );
	    -- shifting, that is multiplication by a free variable	       
            if e#i==0 then(		 
               s=replace(i,modulenumber+1,s);
            )
            else(		    
	       if s#i>=modulenumber then(
	          loop=true;   
                  logei=logpoly(e#i,depvars,indvars);
	          for j to #numerators-1 do(
	             logj=logpoly(dq*numerators#j,depvars,indvars);
	             if logj#1==logei#1 then(
	                prod=1;
		        for k to indno-1 do(
		           mx=-((-(((logj)#2)#k-((logei)#2)#k))//q);
		           if mx>0 then( prod=prod*(indvars#k)^mx);
		        );
	                if all(apply(#g,l->s#l>=modulenumber and g#l!=0 and leadMonomial(g#l)==leadMonomial(g#i*prod)), l->l==false) then(
                           gx=(g#i)*prod;
                           pos=position(sort(append(g,gx)),a->a==gx);
                           g=insert(pos,gx,g);
                           h=insert(pos,(h#i)*prod^q,h);
                           e=insert(pos,red((e#i)*prod^q,I,dq,numerators),e);
                           s=insert(pos,modulenumber,s);
                           loop=true;
	                );
	             );          
	          );
                  for k to #g-1 do(
	             if s#k>=modulenumber and g#k!=0 and e#k!=0 then(	 	 
	                logk=logpoly(e#k,depvars,indvars);
	                if k<i and logk#0==logei#0 then(
	                   if all(apply((logei#2-logk#2), v->v%q), v->v==0) then(
		              ww=apply(indno, v->degree(indvars#v,lcm(logei#3,logk#3)//logei#3));
                              prod=product(#ww, l->(indvars#l)^((ww#l)//q));
                              gx=(g#i)*prod;
                              g=sort(append(g,gx));
                              pos=position(g,a->a==gx);
                              h= insert(pos,(h#i)*prod^q,h);
                              e=insert(pos,red((e#i)*prod^q,I,dq,numerators),e);
                              s=insert(pos,modulenumber,s);
		              loop=true;
                           );
	                );
	             );
	          );
               );
            );
         );
         i=i+1;
      );    
      -- initialize next P-module generator
      modulenumber=modulenumber+1;
      gg=toList g;	 
      --remove any elements that are zeroed out  
      posl=positions(gg, v->v!=0);
      g=g_posl;
      h=h_posl;
      e=e_posl;
      s=s_posl;
      pos2=positions(s, v->v==modulenumber);
      --save only elements describing the next module 
      g=g_pos2;
      h=h_pos2;
      e=e_pos2;
      s=s_pos2;	 
      numerators=for i to #g-1 list if s#i==modulenumber and e#i==0 then g#i else  continue
print(numerators);
      e=apply(h,s->red(s,I,dq,numerators));	 
   );
   -- paring down to the final set of numerators
   i=#numerators-1;
   while i>0 do(
      logi=logpoly(numerators#i,depvars,indvars);
      for j to i-1 do(
         logj=logpoly(numerators#j,depvars,indvars);
         if logi#0==logj#0 and geqlog(logi#2,logj#2) then(
            numerators=delete(numerators#i,numerators);
	    break;
         );
      );
      i=i-1;
   );
   for i to #numerators-1 list(
      head=leadTerm(numerators#i);
      tail=numerators#i-head;
      for j to i-1 do(
	  tail=tail%numerators#(i-1-j);
      );
      head+tail
    )  	  
);

------------------------------
--weight matrix from fractions
------------------------------

--icRweightmatrix = method(TypicalValue => Matrix);
--icRweightmatrix (List,Matrix) 
icRweightmatrix := (fractions,matr) ->(
   revfrac:=rsort(fractions);
   weightlist:=for i to #revfrac-2 list( 
      wt(revfrac#i,matr)-wt(revfrac#-1,matr)
   );
   transpose matrix(weightlist)|matr_{numColumns(matr)-numRows(matr)..numColumns(matr)-1}   
     
);

----------------------------------
--integral closure polynomial ring
----------------------------------

--icR=method(TypicalValue=>Ring);
--icR(List,Matrix) 
icR :=(fractions,wtR)->
   matrixOrder(coefficientRing Rq,grevlexWeight(icRweightmatrix(fractions,wtR))
);

---------------------------------
--icRrelations
---------------------------------

--icRrelations=method(TypicalValue=>List);
--icRrelations(List,Ideal,Ring,ZZ) 
icRrelations := (fractions,Rq,I,icR,depno)->(
--   Rq:=ring I;  
   noeth:=for i to #gens Rq-1 list if i<depno then 0 else icR_(#fractions+i-depno-1);
   psi:=map(icR,Rq,noeth);
   revold:=rsort(fractions);
   quads:=flatten( 
      for i to #fractions-2 list(
         for j from i to #fractions-2 list(
            relij= icR_i*icR_j;
            tailij=((revold#i)*(revold#j))%I//(revold#-1);
            k=#fractions-1;
            while tailij !=0 and k>=0 do(
	       if (logpoly(tailij,depvars,indvars))#0==(logpoly(revold#k,depvars,indvars))#0
	       and geqlog((logpoly(tailij,depvars,indvars))#2,(logpoly(revold#k,depvars,indvars))#2)
	       then(
	          l=leadTerm(tailij)//leadTerm(revold#k);
	          tailij=tailij-l*revold#k;
	          relij=if k==#fractions-1 then relij-psi(l) else relij-psi(l)*(icR_k);
	          k=#fractions-1;
	       )
               else(
	          k=k-1;
	       );
	    );
            relij
         )
      )	
   );
   lins:=flatten( 
      for i to #fractions-2 list(
         for j from i+1 to #fractions-2 list(
	    if (logpoly(revold#i,depvars,indvars))#0==(logpoly(revold#j,depvars,indvars))#0 then(
	       GCD=gcd(leadMonomial(revold#i),leadMonomial(revold#j));	
	       tailji=(revold#i*leadTerm(revold#j))//GCD-(revold#j*leadTerm(revold#i))//GCD;
	       relji=icR_i*psi(leadTerm(revold#j)//GCD)-icR_j*psi(leadTerm(revold#i)//GCD);
               k=#fractions-2; 
               while tailji !=0 do(
	          if (logpoly(tailji,depvars,indvars))#0==(logpoly(revold#k,depvars,indvars))#0
	          and geqlog((logpoly(tailji,depvars,indvars))#2,(logpoly(revold#k,depvars,indvars))#2)
	          then(
	             l=leadTerm(tailji)//leadTerm(revold#k);
	             tailji=tailji-l*revold#k;
	             relji=relji-psi(l)*(icR_k);
	    	     k=#fractions-2;
	          )
                  else(
	             k=k-1;
	          );
	       );
               relji
            )
         )	
      )
   );
   delete(0_icR,delete(null,quads|lins))
);


-------------------------------------
-- ZZ/q version
-------------------------------------


--qthIntegralclosure:=method(TypicalValue=>List);
--qthIntegralclosure(Matrix,Ring,List)
qthIntegralclosure :=(wtR,Rq,Iq)->(
   q:=char Rq;
   indno:=numRows wtR;
   depno:=(numColumns wtR) -indno;
   indvars:=take(gens Rq, -indno);
   depvars:=take(gens Rq, depno);
   I:=ideal(Iq);
   degs:=for dv in depvars list max(apply(Iq,v->degree(dv,v)));
   footq:={1};
   for i to depno-1 do footq=flatten for j to degs#i-1 list for f in footq list f*(depvars#i)^j;
   LMideal:=ideal(apply(Iq, v->leadMonomial v));
   footq=sort(delete(0_Rq,for f in footq list f%LMideal));
   degr:=#footq;
   fractions:=icRfractions(I,depno,footq);
   wticR:=icRweightmatrix(fractions,wtR);
   icR:=matrixOrder(coefficientRing Rq,grevlexWeight(wticR));
   relicR:=icRrelations(fractions,I,icR,depno);
   {fractions,wticR,icR,relicR}
);


-----------------------------------
--QQ version
-----------------------------------

--rationalIntegralClosure:=method(TypicalValue=>List);
--rationalIntegralClosure(Ideal,ZZ,ZZ,List,List,RingElement,List):=(I,depno,degr,dep,ind,f,foot)->(
--   R:=ring I;
--   RZ=ZZ(monoid R);
--   toQ=map(R,RZ);
--   -----------------------------------------
--   D0=qthConductor(I,depno);
--   ----------------------------------------------------------------------
--   --find primes that definitely can't be used---------------------------
--   badprimes=lcm(apply(apply(flatten entries ((coefficients(f))#1),v->substitute(v,QQ)),v->denominator(v)));
--   badprimes=badprimes*degr;
--   ----------------------------------------------------------------------
--   --look for good primes, and run the qth power algorithm using ZZ/q instead of QQ
--   q=2;
--   firstPrime=true;
--   stable=false;
--   while stable==false do(
--      stable=true;
--      while isPrime(q)==false do(
--         q=q+1;
--      );
--      Rq=(ZZ/q)(monoid R);
--      toq=map(Rq,R);
--      Iq=toq(I);
--      footq=apply(foot,v->toq(v));
--      depq=apply(dep,v->toq(v));
--      indq=apply(ind,v->toq(v));
--      ----------------------------------------------------
--      Dq=qthConductor(I,depno);
--      --q is good if D%q matches Dq----------------------------------   
--      if Dq==sub(D,Rq) then(
--         d=Dq^(q-1);
--         --this next part will be replaced by a call to the qth power method
--         --with Iq, footq, etc..      
--         {icRfractions,icRweightmatrix,icRring,icRrelations}:=
--         moduleIntegralClosure(wtR,I,footq); 
--         -----------------------------------------------------------------
--         --if more than one good prime has been used, 
--         --it is then necessary to use CRT and EEA------------------------
--         if firstPrime==true then(
--	    oldvarsZ=apply(icRfractions,v->toZ(v));
--	    olrelsZ=apply(icRrelations,v->toZ(v));  
--	    oldvarsQ=for i to #oldvarsZ-1 list polyrecon(oldvarsZ#i,q);
--	    oldrelsQ=for i to #oldrelsZ-1 list polyrecon(oldrelsZ#i,q);
--	    oldmod=q;
--	    stable=false;
--         )
--         else(
--            --find variables and relations mod the product of all primes so far
--            toZ=map(RZ,Rq);	      
--	    newvarsZ=for i to #oldvarsZ-1 list polyCRT(oldvarsZ#i,qfractions#i,oldmod,q);
--	    newrelsZ=for i to #oldrelsZ-1 list polyCRT(oldrelsZ#i,rels2#i,oldmod,q);
--	    oldmod=oldmod*q;
--	    newvarsQ=for i to #newvarsZ-1 list polyrecon(newvarsZ#i,oldmod);
--	    newrelsQ=for i to #newrelsZ-1 list polyrecon(newrelsZ#i,oldmod);
--            --Has this process stabilized yet or not?	 
--	    if newvarsQ!=oldvarsQ or newrelsQ!=oldrelsQ then(
--	       oldvarsQ=newvarsQ;
--	       oldrelsQ=newrelsQ;
--	       stable=false;
--	    );   
--         );
--         q=q+1;
--      );
--   );
--   {weightmatrixICR,icR,oldvarsQ,oldrelsQ}   
--);


--example 1, disguised Hermitian-----------------------------------------
wtR=matrix{{9,8}}
Rq=ZZ/31[y,x,Weights=>entries(weightGrevlex(wtR))]
Iq={y^8-x^9+2*y*x^6-y^2*x^3}

toString(qthIntegralclosure(wtR,Rq,Iq))

--example 7, Leonard 2009 -----------------------------------------------
wtR:=matrix{{19,15,12,9,9},{12,9,9,9,0}};

Rq=ZZ/2[z19,y15,y12,x9,u9,Weights=>entries(weightGrevlex(wtR))]

Iq = {y15^2+y12*x9*u9,
      y15*y12+x9^2*u9+x9*u9^2+y15,
      y12^2+y15*x9+y15*u9+y12,
      z19^3+z19*(y15+y12)*(x9+1)*u9+(y15*(x9+u9)+y12*(x9*u9+1))*x9^2*u9}

toString(qthIntegralclosure(wtR,Rq,Iq))
-------------------------------------------------------------------------
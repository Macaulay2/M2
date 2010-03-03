



newPackage(
    "QthPower",
    Version => "0.9", 
    Date => "March 1, 2010",
    Authors => {{Name => "Douglas A. Leonard",
                 Email => "leonada@auburn.edu",
                 HomePage => "http://www.dms.auburn.edu/~leonada"}},
    Headline => "An implementation of the Qth-Power algorithm",
    DebuggingMode => true
)
------------------------------------------------------------------------
export{qthConductor, 
       qthIntegralClosure, 
       rationalIntegralClosure, 
       minimization,
       weightGrevlex
}


-- PURPOSE : Generally implements Leonard's qth-power algorthim 
--           to compute the integral closure of type I integral domains
--	   over both ZZ/q and QQ
-- INPUT : weight matrix, ring and generating set for
--           a type I affine domain (or something close to same) 
-- OUTPUT : weight matrix, ring and generating set for
--           the integral closure, 
--	   (and fractions giving the map back to the input ring)
-- COMMENTS: (1) The package contains short functions for doing basic 
--           number theory, in particular the extended Euclidean algorithm
--	   and the Chinese Remainder Theorem algorithm, as well as
--	   standard modular exponentiation, as well as functions
--	   for dealing with certain weighted monomial orderings,
--	   none of which are exported.
--	   (2) The type I restriction can probably be generalized a bit 
--         to integral extensions if wt(LT(f))=wt(f-LT(f)), 
--	   and maybe the domain part as well,
--         though it is better to tackle problems one domain at a time.
--	   (3) Methods are local, in that the conductor is factored,
--	   and separate closures for each local factor are reconciled
--	   to get the result.  


------------------------------------------------------------------------
-----------------------  Code  -----------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
-------- non-exported helper functions --------------------------------- 
------------------------------------------------------------------------

-- dependent and independent "logs" and leading monomials of a polynomial
logpoly := (v,depvars,indvars) -> (
        lv := leadMonomial v;
    indlog := apply(#indvars, i->degree(indvars#i, lv));
   indprod := product(#indvars, i->(indvars#i)^(indlog#i));
    deplog := apply(#depvars, i->degree(depvars#i, lv));
   depprod := product(#depvars, i->(depvars#i)^(deplog#i));
             {deplog, depprod, indlog, indprod}
);
-------------------------------------------------------------------------
-- Compare "logs" entrywise
geqlog := (v, w) -> (
    for i from 0 to #v-1 do if v#i < w#i then return false; true
);
-------------------------------------------------------------------------
--equal dependent monomials , comparable independent monomials
pgeq := (a,b,depvars,indvars)->(
  if (logpoly(a,depvars,indvars))#0 == (logpoly(b,depvars,indvars))#0
  and geqlog((logpoly(a,depvars,indvars))#2,(logpoly(b,depvars,indvars))#2)
  then return true;false
);       
-------------------------------------------------------------------------
-- Reduction modulo \Delta^(q-1) times the previous module
red:= (ee, I, dq, foot) -> (
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
-------------------------------------------------------------------------
--non-exported basic number-theoretic methods
-------------------------------------------------------------------------
-- the extended Euclidean algorithm
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
----------------------------------------------------------------------
--the Chinese remainder theorem for two integers
crt :=(c,d,p,n)->(
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
--------------------------------------------------------------------------------
-- reconstructing fractions from remainders mod N 
-- using the extended Euclidean algorithm
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
-------------------------------------------------------------------------------
--- reconstructing polynomials over Q from polynomials over Z
polyrecon:=(pol,md,rng)->(
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
--defining the grevlex over weight monomial ordering matrix
grevlexWeight :=(matr)->(
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
-- Find an element D in the Noether noramlization P of R 
-- also in the conductor,
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
    sub(qthconductor,R)
);     
--------------------------------------------------------------------------------
-- Find the numerators of the fractions defining the P-module presentation 
-- of the integral closure of a quotient ring 
-- using the qth-power algorithm,
-- the first numerator also being the common denominator, an element of P.
icRfractions = method();
icRfractions (Ideal,ZZ,List) := (I,depno,foot) ->(
   -- Initialization
              R = ring I;
          indno = #gens R -depno;
        depvars = take(gens R, depno);
        indvars = take(gens R, -indno);
              q = char R;
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
      -- local variables
      i := j := w := k := prod := mx := ll := ei := l := gi := pos := n := m := 0;
      dominating := skip := true;
      logei := logj := logk := poslist := ww := {};
      -------------------------------------------------------------------------    
      -- Compute the next module generating set; 
      -- continue to loop as long as different modules are generated.
      loop := true;
      while loop == true do(       	
         loop = false;
         i = 0;
         while i < #g do(
	    skip = false;
	    if i != 0 and leadMonomial g#i == leadMonomial g#(i-1) then(  	   	
               skip = true;
	    );
            if i !=0 then(
	       for j to i-1 do(
	          if e#(i-1-j) == 0 and pgeq(g#i,g#(i-1-j),depvars,indvars) then(
		     skip = true;
		     break;
	          );
	       );
            );    
	    if skip == true then(   
	       g = drop(g,{i,i});
	       h = drop(h,{i,i});
	       e = drop(e,{i,i});
	       i = i-1;
	    );
            if skip == false then(
               if e#i != 0 then(
                  j = #g-1;
                  while j > i and e#j != 0 do(
                     if pgeq(g#j,g#i,depvars,indvars) and pgeq(e#j,e#i,depvars,indvars) then(  
                        g = drop(g,{j,j});
		        h = drop(h,{j,j}); 
                        e=drop(e,{j,j});
                     );
                     j = j-1;
                  );
               );      	      
	       --removal-----------------------
	       j = #g-1;
	       while j > i do(
	          if pgeq(g#j,g#i,depvars,indvars) then(
		     g = drop(g,{j,j});
		     h = drop(h,{j,j});
		     e = drop(e,{j,j});
	          );
	          j = j-1;
	       );
               -- "row-reduction", that is, taking F-linear combinations
               j = 0;
               while j < i and e#i != 0 do(
                  if e#j != 0 then(
                     logei = logpoly(e#i,depvars,indvars);
                     logj = logpoly(e#j,depvars,indvars);	
                     if pgeq(e#i,e#j,depvars,indvars) 
		     and apply((logei#2-logj#2), v->v%q)==apply(indno, v->0) 
		     then(
		        prod = 1;
		        for k to indno-1 do( 
			   prod = prod*(indvars#k)^((logei#2-logj#2)#k//q)
		        );
		        loop = true;
		        if g#i > (g#j)*prod then(
		           lc = leadCoefficient(e#i)//leadCoefficient(e#j);
                           gi = g#i-lc*prod*g#j;
			   hi = h#i-lc*prod^q*h#j;
			   ei = e#i-lc*prod^q*e#j;
			    g = replace(i,gi,g);
			    h = replace(i,hi,h);
                            m = #numerators-1;
			   while m >= 0 and ei != 0 do(
			      if pgeq(ei,dq*numerators#m,depvars,indvars) then(
			         ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m; 
			          m = #numerators;
			      );
		              m = m-1;
			   );
		           e = replace(i,ei,e);
                           j = -1; 
	    	        )
		        else(
		           if g#i < (g#j)*prod then(
                              gi = (g#j)*prod;
                               g = sort(append(g,gi));
		             pos = position(g, a->a==gi);
                               m = #numerators-1;
			      hi = prod^q*h#j;
			       h = insert(pos,hi,h);
			      ei = prod^q*e#j;
			      while m >= 0 and ei != 0 do(
			         if pgeq(ei,dq*numerators#m,depvars,indvars) then(
			            ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m; 
			             m = #numerators;
			         );
		                 m = m-1;
			      );
		              e = insert(pos,ei,e);
                           );
		        );   
                     );
                  );
                  j = j+1;
	       );
               if e#i == 0 then( 
	          j = #g-1;
	          while j > i do(
		     if pgeq(g#j,g#i,depvars,indvars) then( 
		        g=drop(g,{j,j});
		        h=drop(h,{j,j});
		        e=drop(e,{j,j});
		     );         
	             j = j-1;
	          );
	       )
               else( 
	       );   
	       -- shifting, that is multiplication by a free variable	       
               if e#i != 0 then(
                  loop = true;   
                  logei = logpoly(e#i,depvars,indvars);
	          for j to #numerators-1 do(
	             logj = logpoly(dq*numerators#j,depvars,indvars);
	             if logj#1 == logei#1 then(
	                prod = 1;
	                for k to indno-1 do(
		           mx = -((-(((logj)#2)#k-((logei)#2)#k))//q);
		           if mx > 0 then( prod = prod*(indvars#k)^mx);
		        );
	                if all(apply(#g,l->g#l != 0 and leadMonomial(g#l) == leadMonomial(g#i*prod)), l->l==false) 
		        then(
                           gi = (g#i)*prod;
                          pos = position(sort(append(g,gi)),a->a==gi);
                            g = insert(pos,gi,g);
			   hi = (h#i)*prod^q;
			    h = insert(pos,hi,h);
		            m = #numerators-1;
			   ei = (e#i)*prod^q;
			   while m >= 0 and ei != 0 do(
			      if pgeq(ei,dq*numerators#m,depvars,indvars) then(
	                         ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m;
			          m = #numerators;
			      );
		              m = m-1;
			   );
		           e = insert(pos,ei,e);
                           loop = true;
                        );
	             );          
	          );
	          k = 0;
                  while k < #g do(
	             if g#k != 0 and e#k != 0 then(
	                logk = logpoly(e#k,depvars,indvars);
	                if k < i and logk#0 == logei#0 then(
	                   if all(apply((logei#2-logk#2), v->v%q), v->v==0) then(
	                      ww = apply(indno, v->degree(indvars#v,lcm(logei#3,logk#3)//logei#3));
                            prod = product(#ww, l->(indvars#l)^((ww#l)//q));
                              gi = (g#i)*prod;
                               g = sort(append(g,gi));
                             pos = position(g,a->a == gi);
                               m = #numerators-1;
                              hi = (h#i)*prod^q;
			       h = insert(pos,hi,h);
			      ei = (e#i)*prod^q;
			      while m >= 0 and ei != 0 do(
			         if pgeq(ei,dq*numerators#m,depvars,indvars) then(
	                            ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m; 
			             m = #numerators;
			         );
		                 m = m-1;
			      );
		              e = insert(pos,ei,e);
                              loop = true;
                           );
	                );
	             );
	             k = k+1;
                  );
	       );
            );
            i = i+1;
         );
         --initialize next P-module generator
         modulenumber = modulenumber+1;
         --remove any elements that are zeroed out  
         poslist = positions(g, v->v!=0);
         g = g_poslist;
         h = h_poslist;
         e = e_poslist;
         poslist = positions(e, v->v==0);
         --save only elements describing the next module 
         g = g_poslist;
         h = h_poslist;
         e = e_poslist;
         numerators = g;
         i = #numerators-1;
         while i > 0 do(
            for j to i-1 do(
               if pgeq(numerators#i,numerators#j,depvars,indvars) then(
                  numerators = delete(numerators#i,numerators);
	          break;
               );
            );
            i = i-1;
         );
         numerators=for i to #numerators-1 list(  
            head = leadTerm(numerators#i);
            tail = numerators#i-head;
	    j = i-1;
	    while j >= 0 and tail != 0 do(
	       w = tail%numerators#j;    
	       if tail > w  then(
	          tail = w;
	          j = i;
	       );
               j = j-1;
	    );   	
            (head+tail) % I
         );
         numerators=for i to #numerators-1 list if numerators#i !=0 then numerators#i;
         numerators=delete(,numerators);
--	 print(numerators);
         e= apply(h,v->red(v,I,dq,numerators));
collectGarbage;	 
      );
      J=intersect(J,ideal(numerators));
   );
   J1 = first entries gens gb J;
   J2 = sort flatten for i to #foot-1 list for j to #J1-1 list (foot#i)*(J1#j) % I;
   J3 = for i to #J2-1 list if J2#i != 0 and member((logpoly(J2#i,depvars,indvars))#1,foot) then J2#i else continue;
   J4 = unique J3;
   i = #J4-1;
   while i > 0 do(
      j = i-1;
      while j >= 0 do(
         if pgeq(J4#i,J4#j,depvars,indvars) then(
	    J4 = drop(J4,{i,i});
	    break;
         );
         j = j-1;
      );
      i = i-1;
   );   	 	   	
   J5 = sort(for i to #J4-1 list (
	        head = leadTerm(J4#i); 
		tail = J4#i-head; 
		for j to i-1 do 
		   tail = tail % J4#(i-1-j); 
		   head+tail
	     )
	);
   GCD = gcd(J5);
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
icRring(List,Matrix) := (fractions,wtR) -> 
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
               while tailji != 0 do(
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
         q = char Rq;
     indno = numRows wtR;
     depno = (numColumns wtR) -indno;
   indvars = take(gens Rq, -indno);
   depvars = take(gens Rq, depno);
         I = ideal(Iq);
      degs = for dv in depvars list 
                max(apply(Iq,v->degree(dv,v)));
      foot = {1};
             for i to depno-1 do
	        if degs#i > 0 then(  
	           foot = flatten for j to degs#i-1 list 
		                     for f in foot list 
			   	     f*(depvars#i)^j;
		   );		     
   LMideal = ideal(apply(Iq, v->leadMonomial v));
      foot = sort(delete(0_Rq,for f in foot list f % LMideal));
      degr = #foot;
 fractions = icRfractions(I,depno,foot);
     wticR = icRweightmatrix(fractions,wtR);
       icR = matrixOrder(coefficientRing Rq,grevlexWeight(wticR));
    relicR = icRrelations(fractions,I,icR,depno);
       icR = ring(relicR#0);
   ---------------------------------------------------------------------------	
             (fractions,relicR,icR,wticR)
);
------------------------------------------------------------------------------
--QQ version
------------------------------------------------------------------------------
rationalIntegralClosure = method();
rationalIntegralClosure(Matrix,Ring,List) := (wtR,R0,I0) -> (
           I = ideal(I0);
       indno = numRows(wtR);
       depno = numColumns(wtR)-indno;
    indvars0 = take(gens R0,-indno);
     indvars = indvars0;
    depvars0 = take(gens R0,depno);
     depvars = depvars0;
        degs = for dv in depvars0 list max(apply(I0,v->degree(dv,v)));
       foot0 = {1};
               for i to depno-1 do
	          if degs#i >0 then( 
	             foot0 = flatten for j to degs#i-1 list 
		     for f in foot0 list 
		        f*(depvars0#i)^j;
		  );
     LMideal = ideal(apply(I0, v->leadMonomial v));
       foot0 = sort(delete(0_R0,for f in foot0 list f % LMideal));
	foot = foot0;
        degr = #foot0;
          RZ = ZZ(monoid R0);
         toQ = map(R0,RZ);
   ---------------------------------------------------------------------------
         D0 = qthConductor(I,depno);
   ---------------------------------------------------------------------------
   --find primes that definitely can't be used--------------------------------
   badprimes := 1;
   for f in I0 do(
      badprimes = badprimes*lcm(apply(apply(flatten entries ((coefficients(f))#1),v->substitute(v,QQ)),v->denominator(v))); );
   badprimes = badprimes*degr;
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
	   Rq = (ZZ/q)(monoid R0);
          toq = map(Rq,R0);
	  toZ = map(RZ,Rq);
           Iq = I0/toq;
         foot = foot0/toq;
      depvars = depvars0/toq;
      indvars = indvars0/toq;
           Jq = ideal(Iq);
   ----------------------------------------------------------------------------
           Dq = qthConductor(Jq,depno);
   --q is good if D%q matches Dq-----------------------------------------------   
      if Dq == sub(D0,Rq) then(
            d = Dq^(q-1);
         --this next part will be replaced by a call to the qth power method
         --with Iq, footq, etc..      
         (Numerators,relationsicRq,icRq,wticR) := 
               qthIntegralClosure(wtR,Rq,Iq);	 
         ----------------------------------------------------------------------
         --if more than one good prime has been used, 
         --it is then necessary to use CRT and EEA----------------------------------
         if firstPrime == true then(
	      oldvarsZ := for i to #Numerators-1 list sub(Numerators#i,RZ);
                  icRZ := ZZ(monoid icRq);
	      oldrelsZ := for i to #relationsicRq-1 list sub(relationsicRq#i,icRZ);  
              oldvarsQ := for i to #oldvarsZ-1 list polyrecon(oldvarsZ#i,q,R0); 
	          icRQ := QQ(monoid icRZ);		 
	      oldrelsQ := for i to #oldrelsZ-1 list polyrecon(oldrelsZ#i,q,icRQ); 
	        oldmod := q;
	       stable   = false;
	    firstPrime  = false;
         )
         else(
            --find variables and relations mod the product of all primes so far
	    newvarsZ = for i to #oldvarsZ-1 list 
	                  polycrt(oldvarsZ#i,sub(Numerators#i,RZ),oldmod,q);
	    newrelsZ = for i to #oldrelsZ-1 list 
	                  polycrt(oldrelsZ#i,sub(relationsicRq#i,icRZ),oldmod,q);
	      oldmod = oldmod*q;
	    newvarsQ = for i to #newvarsZ-1 list 
	                  polyrecon(newvarsZ#i,oldmod,R0);
	    newrelsQ = for i to #newrelsZ-1 list 
	                  polyrecon(newrelsZ#i,oldmod,icRQ);
            --Has this process stabilized yet or not?	 
	    if newvarsQ != oldvarsQ or newrelsQ != oldrelsQ then(
	       oldvarsQ = newvarsQ;
	       oldrelsQ = newrelsQ;
	       oldvarsZ = newvarsZ;
	       oldrelsZ = newrelsZ;
	       stable = false;
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
   icR := ring(oldrelsQ#0); 
   (oldvarsQ,oldrelsQ,icR,wticR)   
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
   for i to indno -1 when minimize == false do(
      for j from indno to #T -1 do(
	 if apply(T#i,v->v*((T#j)#0)) == apply(T#j,v->v*((T#i)#0)) then(
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
     --           (see Leonard 2001, Leonard and Pellikaan 2003, Leonard 2009)
     -- PROGRAMS : qthConductor
     --            qthIntegralClosure
     --            rationalIntegralClosure
     --            minimization
     -- wtR is a weight matrix for the input R/I 
     -- with the last indno variables defining a Noether normalization of R 
     -- qthConductor computes a conductor element in P
     -- icRfractions computes a list of numerators
     -- defining numerators for a P-module generating set of fractions for icR,  
     -- with the zero-th numerator also the common denominator 
     -- qthIntegralClosure also returns the ring icR, 
     -- and the induced relations for the ideal presenting the integral closure
     -- PROGRAMMERs : This implementation was written by and is maintained by
     --               Douglas A. Leonard.
     -- THANKS: Mark van Hoeij, David Cook, and those who wrote IntegralClosure.m2
     --         and/or helped me understand some of Macaulay 2's philosophy  
     -- UPDATE HISTORY : 11 February 2010
     ---------------------------------------------------------------------------
     PARA {
	  "This package computes the integral closure of type I affine domains 
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
	  (icFracP in the IntegralClosure package of Macaulay 2 and normalP
          in Singular's normal package are attempts to generalize this by ignoring
          all of the structure that is required by this package in order to produce similarly
          structured output.) Also this package contains the extension to examples
          over the rationals; which, in turn, allows for quicker answers over ZZ/q for most large q."
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
     	  "The rest of the world only completes single row weights this way,
	   whereas M2 only completes a single row and does the other rows differently."
     	  }
     }	
----------------------------------------------------------------------------
document {
     Key => {
	  qthIntegralClosure, (qthIntegralClosure, Matrix, Ring, List)
	  },
     Headline => "computes integral closures in positive characteristic",
     Usage => "(fractions,relicR,icR,wticR) = qthIntegralClosure(wtR,Rq,Iq)",
     Inputs => {
	  "wtR" => "weight matrix",
	  "Rq" => "ring with weight-over-grevlex order",
	  "Iq" => "set of generators for the ideal of relations"
	  },
     Outputs => {
	  "fractions" => List => "numerators of P-module generating set", 
	  "relicR" => List => "and Gr\"obner basis for the ideal of induced relations",
          "icR" => Ring => "grevlex-over-weight polynomial ring for the presentation",
	  "wticR" => Matrix => "weight matrix for the integral closure"
       	  },
     EXAMPLE lines ///
           wtR = matrix{{5,6,6},{3,6,0}}; 
           Rq = ZZ/23[x,y,z,Weights=>entries weightGrevlex(wtR)];
           Iq = {x^6+x^3*z-y^3*z^2};
      	   (fractions,relicR,icR,wticR) = qthIntegralClosure(wtR,Rq,Iq)
          ///,
     PARA {
     	  "There are probably still problems with this taking 
	  too much time and space on the last pass."
     	  }
     }	   
-----------------------------------------------------------------------------------
 document {
     Key => {
	  rationalIntegralClosure, (rationalIntegralClosure, Matrix, Ring, List)
	  },
     Headline => "computes integral closures over the rationals",
     Usage => "(fractions,relicR,icR,wticR) = rationalIntegralClosure(wtR,Rq,Iq)",
     Inputs => {
	  "wtR" => {"weight matrix"},
	  "R0" => {"ring with weight-over-grevlex order"},
	  "I0" => {"set of generators for the ideal of relations"}
	  },
     Outputs => {
	  "fractions" => List => "a list of numerators for a P-module generating set", 
	  "relicR" => List => "a Gr\"obner basis for the ideal of induced relations",
          "icR" => Ring => "a grevlex-over-weight polynomial ring for the presentation", 
	  "wticR" => Matrix => " weight matrix for the integral closure" 
	  },
     EXAMPLE lines ///
           wtR = matrix{{11,6}};
           R0 = QQ[y,x,Weights=> entries weightGrevlex(wtR)];
           I0 = {(y^2-3/4*y-15/17*x)^3-9*y*x^4*(y^2-3/4*y-15/17*x)-27*x^11};
           (fractions,relicR,icR,wticR) = rationalIntegralClosure(wtR,R0,I0)
          ///,
     PARA {
     	  "There are no extra problems noted here yet."
     	  }
     }	   
 -----------------------------------------------------------------------------------
 document {
     Key => {
	  qthConductor, (qthConductor, Ideal, ZZ)
	  },
     Headline => "computes a conductor element which also lives in the given Noether normalization",
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
     	  "This gives a quick conductor element in P."
     	  }
     }	   

-----------------------------------------------------------------------------------
 document {
     Key => {
	  minimization, (minimization, List, List, Ring, Matrix)
	  },
     Headline => "change to a better Noether normalization",
     Usage => "(newwticR,newicR,newrelicR) = minimization(fractions, relicR, icR, wticR)",
     Inputs => {
	  "fractions" => {"ring elements defining a P-module generating set for icR"},
          "relicR" => {"a grobner basis for the relation ideal"},
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
          ic2 = minimization(ic1);
    	  ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)
          ///,
     PARA {
     	  "What can I do to pass the output of ic2 to ic3
	   the same way I passed the output of ic1 to ic2?
	   Also if the min weight is 1, ic3 will give an error,
	   since I don't know how/why to present a genus 0 curve."
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
Iq = {x^6+x^3*z-y^3*z^2};
icq = qthIntegralClosure(wtR1,Rq,Iq)
assert(icq#3 == matrix{{10,9,8,7,5,6,6},{6,9,6,3,3,6,0}})

R0 = QQ[x,y,z,Weights=>entries(weightGrevlex(wtR1))];
I0 = {x^6+x^3*z-y^3*z^2};
ic0 = rationalIntegralClosure(wtR1,R0,I0)
assert(ic0#3 == matrix{{10,9,8,7,5,6,6},{6,9,6,3,3,6,0}})

------------------------------------------------------------------------------
--example 1, disguised Hermitian----------------------------------------------
-- so, in need of minimization------------------------------------------------
------------------------------------------------------------------------------
wtR2 = matrix{{9,8}};
Rq = ZZ/109[y,x,Weights=>entries(weightGrevlex(wtR2))];
Iq = {y^8-x^9+2*y*x^6-y^2*x^3};
ic1 = qthIntegralClosure(wtR2,Rq,Iq)
ic2 = minimization(ic1);
ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)
assert( ic3#3 == matrix{{15,10,5,4}})

R0 = QQ[y,x,Weights=>entries(weightGrevlex(wtR2))];
I0 = {y^8-x^9+2*y*x^6-y^2*x^3};
ic1 = rationalIntegralClosure(wtR2,R0,I0)
ic2 = minimization(ic1);
ic3 = rationalIntegralClosure(ic2#0,ic2#1,ic2#2)
assert(ic3#3 == matrix{{15,10,5,4}})

------------------------------------------------------------------------------
--pseudo-weights--------------------------------------------------------------
------------------------------------------------------------------------------
wtR3=matrix{{7,4}};
R0 = QQ[u,r,Weights=>entries weightGrevlex(wtR3)];
I0 = {u^7-u^3*r^7-r^3};
ic0 = rationalIntegralClosure(wtR3,R0,I0)
assert(ic0#3==matrix{{34,27,24,17,14,7,4}})

------------------------------------------------------------------------------
--generic example over QQ with moderate coefficients to be reconstructed------
------------------------------------------------------------------------------ 
wtR4 = matrix{{11,6}};
R0 = QQ[y,x,MonomialOrder=>{Weights=>{11,6},Weights=>{1,0}}];
I0 = {(y^2-3/4*y-15/17*x)^3-9*y*x^4*(y^2-3/4*y-15/17*x)-27*x^11};
ic0 = rationalIntegralClosure(wtR4,R0,I0)
assert(ic0#3 == matrix{{25,21,20,11,10,6}})

------------------------------------------------------------------------------
--example 7, Leonard 2009 with a presentation that is not free as a P-module--
------------------------------------------------------------------------------
wtR5 := matrix{{19,15,12,9,9},{12,9,9,9,0}};
Rq = ZZ/2[z19,y15,y12,x9,u9,Weights=>entries(weightGrevlex(wtR5))];
Iq = {y15^2+y12*x9*u9,
      y15*y12+x9^2*u9+x9*u9^2+y15,
      y12^2+y15*x9+y15*u9+y12,
      z19^3+z19*(y15+y12)*(x9+1)*u9+(y15*(x9+u9)+y12*(x9*u9+1))*x9^2*u9};
icq = qthIntegralClosure(wtR5,Rq,Iq)
assert(icq#3 == matrix{{34,34,31,29,26,23,19,15,12,9,9},{30,21,21,24,24,15,12,9,9,9,0}})

------------------------------------------------------------------------------
--generic example with non-trivial minimization-------------------------------
-- M2's integralClosure seizes on flattenRing --------------------------------
-- icFracP takes 8873 seconds for even ZZ/5-----------------------------------
------------------------------------------------------------------------------
wtR6 = matrix{{31,12}};
Rq = ZZ/2[y,x,Weights=>entries(weightGrevlex(wtR6))];
Iq = {y^12+y^11+y^10*x^2+y^8*x^9+x^31};
ic1 = qthIntegralClosure(wtR6,Rq,Iq)
ic2 = minimization(ic1);
ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)
assert(ic3#3 == matrix{{38,31,24,12,5}})

------------------------------------------------------------------------------
--from Eisenbud-Neumann p.11: simplest poly with 2 characteristic pairs.------ 
--QQ[y,x]/(y^4-2*x^3*y^2-4*x^5*y+x^6-x^7)-------------------------------------
--written as though it is in (QQ[x])[y]--------------------------------------- 
--and without any thought of the underlying weights 7 and 4-------------------
------------------------------------------------------------------------------
wtR7 = matrix{{7,4}};
Rq = QQ[y,x,Weights=>entries weightGrevlex(wtR7)];
Iq = {y^4-x^7-4*y*x^5-2*y^2*x^3+x^6};
ic1 = rationalIntegralClosure(wtR7,Rq,Iq)
ic2 = minimization(ic1);
assert(ic1#3==matrix{{3,2,1,4}})

------------------------------------------------------------------------------
--from the IntegralClosure package--------------------------------------------
-- not type I, but amenable to grevlex weights--------------------------------
------------------------------------------------------------------------------
wtR8 = matrix{{1,1}};
R0=QQ[v,u,Weights=>entries weightGrevlex(wtR8)];
I0={1/5*(5*v^6+7*v^2*u^4+6*u^6+21*v^2*u^3+12*u^5+21*v^2*u^2+6*u^4+7*v^2*u)};
ic0 = rationalIntegralClosure(wtR8,R0,I0)
assert(ic0#3 == matrix{{2,2,2,2,1,1}})

------------------------------------------------------------------------------
--MAGMA example found in GLS--------------------------------------------------
-- genus 1, but try to figure that out from most pesentations-----------------
--(x-y)*x*(y+x^2)^3-y^3*(x^3+x*y-y^2)=0---------------------------------------
--change of variables gives a type I problem----------------------------------
------------------------------------------------------------------------------
wtR9 = matrix{{11,7}};
Rq = ZZ/109[w,z,Weights => entries weightGrevlex(wtR9)];
Iq = {w^7+3*w^6*z+w^6+3*w^5*z^3+6*w^5*z^2+9*w^4*z^4+4*w^3*z^6-w^3*z^5-3*w^2*z^7-3*w*z^9-z^11};
ic1 = qthIntegralClosure(wtR9,Rq,Iq)
ic2 = minimization(ic1);
ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)
assert(ic3#3 == matrix{{3,2}})

------------------------------------------------------------------------------
--possibly too large an example unless separated------------------------------
wtR10 := matrix{{25,21}};
Rq := ZZ/2[y,x,Weights=>entries(weightGrevlex(wtR10))];
Iq := {
   y^21
  +y^20*x
  +y^18*(x^3+x+1)
  +y^17*(x^3+1)
  +y^16*(x^4+x)
  +y^15*(x^7+x^6+x^3+x+1)
  +y^14*x^7
  +y^13*(x^8+x^7+x^6+x^4+x^3+1)
  +y^12*(x^9+x^8+x^4+1)
  +y^11*(x^11+x^9+x^8+x^5+x^4+x^3+x^2)
  +y^10*(x^12+x^9+x^8+x^7+x^5+x^3+x+1)
  +y^9*(x^14+x^13+x^10+x^9+x^8+x^7+x^6+x^3+x^2+1)
  +y^8*(x^13+x^9+x^8+x^6+x^4+x^3+x)
  +y^7*(x^16+x^15+x^13+x^12+x^11+x^7+x^3+x)
  +y^6*(x^17+x^16+x^13+x^9+x^8+x)
  +y^5*(x^17+x^16+x^12+x^7+x^5+x^2+x+1)
  +y^4*(x^19+x^16+x^15+x^12+x^6+x^5+x^3+1)
  +y^3*(x^18+x^15+x^12+x^10+x^9+x^7+x^4+x)
  +y^2*(x^22+x^21+x^20+x^18+x^13+x^12+x^9+x^8+x^7+x^5+x^4+x^3)
  +y*(x^23+x^22+x^20+x^17+x^15+x^14+x^12+x^9)
  +(x^25+x^23+x^19+x^17+x^15+x^13+x^11+x^5)};

--this took 41 hours and 16 gb-----------------------------------------------
--ic1 = qthIntegralClosure(wtR10,Rq,Iq)--------------------------------------
--ic2 = minimization(ic1);---------------------------------------------------
--ic3 = qthIntegralClosure(ic2#0,ic2#1,ic2#2)--------------------------------
--assert(ic3#3 == matrix{{16,15,13,12,11,10,7}}------------------------------

///











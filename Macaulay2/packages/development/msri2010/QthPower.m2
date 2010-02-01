------------
-- Header --
------------
--newPackage(
--    "QthPower",
--    Version => "0.3", 
--    Date => "January 26, 2010",
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

-- Reduction modulo \Delta^(q-1) times the previous module.

--red = method(TypicalValue => RingElement);
--red (RingElement, Ideal, RingElement, List) 
red:= (g, I, dq, modfoot,depvars,indvars) -> (
   gold := g;
   gnew := 0;
   i := #modfoot-1;
   while i >= 0 and gold != 0 do (	
      gnew = (gold % (dq*modfoot#i)) % I;
      if gold > gnew then (
         gold = gnew;
         i = #modfoot;
      );
      i = i-1;
   );
   gnew%I
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
   app:=apply(ap, v->sub(v#0,QQ)/sub(v#1,QQ));
   --list of monomials
   appp:=apply( flatten (entries (coefficients(pol))#0),v->sub(v,QQ));
--   mons:=(coefficients(pol))#0;
   newpoly:=0;
   for i to #appp-1 do(
      newpoly=newpoly+appp#i*app#i;
   );
   newpoly
);

polyreconq:=(pol,q,RZ)->(
   co:=(coefficients(pol))#1;
   --list of coefficients as integers mod q
   ent:=flatten entries(co);     
   --pairs (numerator,denominator)          
   ap:=apply(ent,v->sub(v,ZZ));
   --list of monomials
   appp:=apply( flatten (entries (coefficients(pol))#0),v->sub(v,RZ));
   mons:=(coefficients(pol))#0;
   newpoly:=0;
   for i to #mons-1 do(
      newpoly=newpoly+sub(mons#i,ZZ)*app#i;
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
     );


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
    GP := gens gb  (transpose jacobian IP|matrix{{gens IP}}**identity(RP^(numColumns(jacobian IP))));
    depvars:=take(gens RP,depno);
    indvars:=take(gens RP,depno-#gens RP);
    DP := 1;
    rowDP:=0;
    j:=numColumns(GP)-1;
    i:=numRows(GP)-1;
    rowDP=0;
    while i>=0 and j>=0 do(
       while i>=0 and j>=0 and (GP_(i,j)==0 or (logpoly(GP_(i,j),depvars,indvars))#1!=1) do(
          j=j-1;
       );
       rowDP=0;
       while i>=0 and j>=0 and GP_(i,j)!=0 and (logpoly(GP_(i,j),depvars,indvars))#1==1 do(
	  rowDP=gcd(rowDP,GP_(i,j));
	  j=j-1;
       );
       if rowDP!=0 then DP=DP*rowDP;
       i=i-1; 
    );
    sub(DP,R)
);     
	    

-- Find the numerators of the fractions defining the P-module presentation 
-- of the integral closure of a quotient ring 
-- using the qth-power algorithm,
-- the first numerator being also the common denominator, an element of P
-- There is an option to provide your own conductor element

icRfractions = method(Options=>{Del=>null});
icRfractions (Ideal,ZZ,List) :=(Ideal,ZZ,List)=>o->(I,depno,footq) ->(
   -- Initialisation
              R := ring I;
          indno := #gens R -depno;
        depvars := take(gens R, depno);
        indvars := take(gens R, -indno);
              q := char R;
	    del := o.Del;  
         Deltaq := if del === null then qthConductor(I,depno) else del;
             dq := Deltaq^(q-1);   
              g := footq;
              h := apply(g, v->fastq(v,I));
              e := apply(h, v->red(v,I,dq,footq,depvars,indvars));
   modulenumber := -1;
     numerators := footq;
--            tbl := for i to #g-1 list {g#i,(logpoly(g#i,depvars,indvars))#1,(logpoly(g#i,depvars,indvars))#3,e#i,(logpoly(e#i,depvars,indvars))#0,(logpoly(e#i,depvars,indvars))#2};
   --------------------------------------------    
   -- local variables
   i := j := w := k := prod := mx := ll := ei := l := gi := pos := n := m := 0;
   dominating := skip := true;
   logei := logj := logk := posl := pos2 := ww := lst := {};
   -----------------------------------------------------    
   -- Compute the next module generating set; 
   -- continue to loop as long as different modules are generated.
   loop := true;
   for i to #numerators-1 do print(i,leadMonomial numerators#i);
--   for i to #tbl-1 do print(i,tbl#i);
   while loop == true do(       	
      loop = false;
      i = 0;
      while i < #g do(
--print(for i in g list leadMonomial i);	   
	 skip = false;
	 if i != 0 and leadMonomial g#i == leadMonomial g#(i-1) then(  	   	
            skip = true;
	 );
         if i !=0 then(
	    for j to i-1 do(
	       k = i-1-j;
	       if e#k == 0 
	       and (logpoly(g#i,depvars,indvars))#0 == (logpoly(g#k,depvars,indvars))#0
	       and geqlog((logpoly(g#i,depvars,indvars))#2,(logpoly(g#k,depvars,indvars))#2)
	       then(
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
--if e#i != 0 then(
--   j = #g-1;
--   while j > i and e#j != 0 do(
--      if (logpoly(g#j,depvars,indvars))#0 == (logpoly(g#i,depvars,indvars))#0
--      and geqlog((logpoly(g#j,depvars,indvars))#2,(logpoly(g#i,depvars,indvars))#2)
--      and (logpoly(e#j,depvars,indvars))#0 == (logpoly(e#i,depvars,indvars))#0
--      and geqlog((logpoly(e#j,depvars,indvars))#2,(logpoly(e#i,depvars,indvars))#2)
--      then(
--	 print(-6,leadMonomial g#i,leadMonomial g#j);  
--         g=drop(g,{j,j});
--         e=drop(e,{j,j});
--      );
--      j = j-1;
--   );
--);      	      
--	    headg := leadTerm(g#i);
--	    tailg := g#i-headg;	 
--	    for j to i-1 do( 
--	       if e#(i-1-j) == 0 then(  
--		  tailg = tailg%(g#(i-1-j));
--	       );    
--	    );
--	    g = replace(i,headg+tailg,g);
--           e = replace(i,red((g#i)^q,I,dq,numerators,depvars,indvars),e);
	    --removal-----------------------
	    j = #g-1;
	    while j > i do(
	       if (logpoly(g#j,depvars,indvars))#0 == (logpoly(g#i,depvars,indvars))#0
	       and geqlog((logpoly(g#j,depvars,indvars))#2,(logpoly(g#i,depvars,indvars))#2)
	       then(
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
                  if e#j != 0 
		  and logj#0 == logei#0 
		  and geqlog(logei#2,logj#2) 
		  and apply((logei#2-logj#2), v->v%q)==apply(indno, v->0) 
		  then(
		     w = 1;
		     for k to indno-1 do( 
			w = w*(indvars#k)^((logei#2-logj#2)#k//q)
		     );
		     loop = true;
		     if g#i > (g#j)*w then(
		        lc = leadCoefficient(e#i)//leadCoefficient(e#j);
                        gi = g#i-lc*w*g#j;
			hi = h#i-lc*w^q*h#j;
			ei = e#i-lc*w^q*e#j;
			g = replace(i,gi,g);
			h = replace(i,hi,h);
                        m = #numerators-1;
			while m >= 0 and ei != 0 do(
			   if (logpoly(ei,depvars,indvars))#0 == (logpoly(numerators#m,depvars,indvars))#0
			   and geqlog((logpoly(ei,depvars,indvars))#2,(logpoly(dq*numerators#m,depvars,indvars))#2)
		           then(
			      ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m; 
--			      print(-1,i,leadMonomial ei);
			      m = #numerators;
			   );
		           m = m-1;
			);
		        e = replace(i,ei,e);
                        j = -1; 
	    	     )
		     else(
		        if g#i < (g#j)*w then(
                           gi = (g#j)*w;
                           g = sort(append(g,gi));
		           pos = position(g, a->a==gi);
                           m = #numerators-1;
			   hi = w^q*h#j;
			   h = insert(pos,hi,h);
			   ei = w^q*e#j; 
			   while m >= 0 and ei != 0 do(
			      if (logpoly(ei,depvars,indvars))#0 == (logpoly(numerators#m,depvars,indvars))#0
			      and geqlog((logpoly(ei,depvars,indvars))#2,(logpoly(dq*numerators#m,depvars,indvars))#2)
		              then(
			         ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m; 
--			         print(-2,i,leadMonomial ei);
			         m = #numerators;
			      );
		              m = m-1;
			   );
		           e = insert(pos,ei,e);
--                           print(-2,pos,leadMonomial g#pos,e#pos);
--   m = #g-1;
--   while m > pos do(
--      if (logpoly(g#m,depvars,indvars))#0 == (logpoly(g#pos,depvars,indvars))#0
--      and geqlog((logpoly(g#m,depvars,indvars))#2,(logpoly(g#pos,depvars,indvars))#2)
--      then(
--         print(-5,m,pos); 
--         g=drop(g,{m,m});
--         e=drop(e,{m,m});
--      );
--      m = m-1;
--   );     	  
--   print(pos,#g);
                        );
		     );   
                  );
               );
               j = j+1;
	    );
            if e#i == 0 then( 
	       print(i,leadMonomial g#i,0);
	       j = #g-1;
	       while j > i do(
		  if (logpoly(g#j,depvars,indvars))#0 == (logpoly(g#i,depvars,indvars))#0
		  and geqlog((logpoly(g#j,depvars,indvars))#2,(logpoly(g#i,depvars,indvars))#2)
		  then( 
		     g=drop(g,{j,j});
		     h=drop(h,{j,j});
		     e=drop(e,{j,j});
		  );         
	          j = j-1;
	       );
	    )
            else( 
	       print(i,leadMonomial g#i,leadMonomial e#i);
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
		        if mx > 0 then( prod=prod*(indvars#k)^mx);
		     );
	             if all(apply(#g,l->g#l!=0 and leadMonomial(g#l)==leadMonomial(g#i*prod)), l->l==false) 
		     then(
                        gi = (g#i)*prod;
                        pos = position(sort(append(g,gi)),a->a==gi);
                        g = insert(pos,gi,g);
			hi = (h#i)*prod^q;
			h = insert(pos,hi,h);
		        m = #numerators-1;
			ei = (prod^q)*e#i; 
			while m >= 0 and ei != 0 do(
			   if (logpoly(ei,depvars,indvars))#0 == (logpoly(numerators#m,depvars,indvars))#0
			   and geqlog((logpoly(ei,depvars,indvars))#2,(logpoly(dq*numerators#m,depvars,indvars))#2)
		           then(
	                      ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m;
--			      print(-3,i,leadMonomial ei);
			      m = #numerators;
			   );
		           m = m-1;
			);
		        e = insert(pos,ei,e);
--if e#pos != 0 then(
--                 print(-3,pos,leadMonomial g#pos,leadMonomial e#pos);
--	      )
--	      else(
--		 print(-3,pos,leadMonomial g#pos,0);
--	      );	 
--m = #g-1;
--while m > pos do(
--   if (logpoly(g#m,depvars,indvars))#0 == (logpoly(g#pos,depvars,indvars))#0
--   and geqlog((logpoly(g#m,depvars,indvars))#2,(logpoly(g#pos,depvars,indvars))#2)
--   then(
--      print(-3,m,pos,#g,#e);  
--      g=drop(g,{m,m});
--      e=drop(e,{m,m});
--   );
--   m = m-1;
--);     	  
--print(pos,#g);
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
--			   if geqlog((logpoly(gi,depvars,indvars))#2,(logpoly(Deltaq,depvars,indvars))#2) == false then(
                              g = sort(append(g,gi));
                              pos = position(g,a->a == gi);
                              m = #numerators-1;
			      hi = prod^q*h#i;
			      h = insert(pos,hi,h);
			      ei = prod^q*e#i; 
			      while m >= 0 and ei != 0 do(
			         if (logpoly(ei,depvars,indvars))#0 == (logpoly(numerators#m,depvars,indvars))#0
			         and geqlog((logpoly(ei,depvars,indvars))#2,(logpoly(dq*numerators#m,depvars,indvars))#2)
		                 then(
			            ei = ei-leadTerm(ei)//leadTerm(dq*numerators#m)*dq*numerators#m; 
--			            print(-4,i,leadMonomial ei);
			            m = #numerators;
			         );
		                 m = m-1;
			      );
		              e = insert(pos,ei,e);
--			   );
--if e#pos != 0 then(
--                 print(-4,pos,leadMonomial g#pos, leadMonomial e#pos); 
--	      )
--	      else(
--		  print(-4,pos,leadMonomial g#pos,0);
--	      );
--m = #g-1;
--while m > pos do( 
--   if (logpoly(g#m,depvars,indvars))#0 == (logpoly(g#pos,depvars,indvars))#0
--   and geqlog((logpoly(g#m,depvars,indvars))#2,(logpoly(g#pos,depvars,indvars))#2)
--   then(
--      print(-4,m,pos);  
--      g=drop(g,{m,m});
--      e=drop(e,{m,m});
--   );
--   m = m-1;
--);
--print(pos,#g);
                           loop = true;
                        );
	             );
	          );
	          k = k+1;
               );
	    );
         );
         i = i+1;
--	 print(i,#g);
      );
      --initialize next P-module generator
      modulenumber = modulenumber+1;
      --remove any elements that are zeroed out  
      posl = positions(g, v->v!=0);
      g = g_posl;
      h = h_posl;
      e = e_posl;
      posl = positions(e, v->v==0);
      --save only elements describing the next module 
      g = g_posl;
      h = h_posl;
      e = e_posl;
      numerators = g;
      for j to #numerators-1 do( 
--	 print(j,numerators#j);
      );
      i = #numerators-1;
      while i > 0 do(
         logi = logpoly(numerators#i,depvars,indvars);
         for j to i-1 do(
            logj = logpoly(numerators#j,depvars,indvars);
            if logi#0 == logj#0 and geqlog(logi#2,logj#2) then(
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
         head+tail
--         for j to i-1 do(
--	    tail = tail%numerators#(i-1-j);
--         );
--         head+tail
      );
for i to #numerators-1 do(print(i,toString(numerators#i)));   	  
time   e= apply(h,v->red(v,I,dq,numerators,depvars,indvars));	 
   );
   numerators
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
icRrelations := (fractions,I,icR,depno)->(
   Rq:=ring I;
   indno:=#gens Rq - depno;  
   depvars:=take(gens Rq,depno);
   indvars:=take(gens Rq,-indno);
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


qthIntegralclosure:=method(Options=>{Delta=>null});
qthIntegralclosure(Matrix,Ring,List):=(Matrix,Ring,List)=>o->(wtR,Rq,Iq)->(
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
     footq =sort(delete(0_Rq,for f in footq list f%LMideal));
      degr:=#footq;
 fractions:=icRfractions(I,depno,footq,Del=>o.Delta);
-- fractions:=icRfractions(I,depno,footq,Del=>sub(((fa#5)#0),Rq));
     wticR:=icRweightmatrix(fractions,wtR);
       icR:=matrixOrder(coefficientRing Rq,grevlexWeight(wticR));
    relicR:=icRrelations(fractions,I,icR,depno);
        Dl:=qthConductor(I,depno);
   (Dl,fractions,wticR,icR,relicR)
);


-----------------------------------
--QQ version
-----------------------------------

--rationalIntegralclosure:=method(TypicalValue=>List);
--rationalIntegralclosure(List,Matrix):=(I0,wtR)->(
--          I:=ideal(I0);
--          R:=ring I;
--      indno:=numRows(wtR);
--      depno:=numColumns(wtR)-indno;
--   indvars0:=take(gens R,-indno);
--   depvars0:=take(gens R,depno);
--       degs:=for dv in depvars0 list max(apply(I0,v->degree(dv,v)));
--      foot0:={1};
--           for i to depno-1 do foot0=flatten for j to degs#i-1 list for f in foot0 list f*(depvars0#i)^j;
--           LMideal:=ideal(apply(I0, v->leadMonomial v));
--      foot0 =sort(delete(0_R,for f in foot0 list f%LMideal));
--       degr:=#foot0;
--       
--   
--         RZ:=ZZ(monoid R);
--        toQ:=map(R,RZ);
--   -----------------------------------------
--         D0:=qthConductor(I,depno);
--   ----------------------------------------------------------------------
--   --find primes that definitely can't be used---------------------------
--  badprimes:=1;
--   for f in I0 do(
--      badprimes=badprimes*lcm(apply(apply(flatten entries ((coefficients(f))#1),v->substitute(v,QQ)),v->denominator(v)));
--   );
--   badprimes=badprimes*degr;
--   ----------------------------------------------------------------------
--   --look for good primes, and run the qth power algorithm using ZZ/q instead of QQ
--            q:=2;
--   firstPrime:=true;
--       stable:=false;
--   while stable==false do(
--      stable=true;
--      while isPrime(q)==false do(
--         q=q+1;
--      );
--print(q);        
--	   Rq:=(ZZ/q)(monoid R);
--          toq:=map(Rq,R);
--	  toZ:=map(RZ,Rq);
--           Iq:=I0/toq;
--        footq:=foot0/toq;
--      depvars:=depvars0/toq;
--      indvars:=indvars0/toq;
--           Jq:=ideal(Iq);
--      ----------------------------------------------------
--           Dq:=qthConductor(Jq,depno);
--      --q is good if D%q matches Dq----------------------------------   
--      if Dq==sub(D0,Rq) then(
--            d:=Dq^(q-1);
--         --this next part will be replaced by a call to the qth power method
--         --with Iq, footq, etc..      
--         (Delt,fractions,wticR,icR,relicR):=
--         qthIntegralclosure(wtR,Rq,Iq);
--print(fractions);	  
--         -----------------------------------------------------------------
--         --if more than one good prime has been used, 
--         --it is then necessary to use CRT and EEA------------------------
--         if firstPrime==true then(
--	    oldvarsZ:=for i to #fractions-1 list sub(fractions#i,RZ);
--	     olrelsZ:=for i  to #relicR-1 list sub(relicR#i,RZ);  
--	    oldvarsQ:=for i to #oldvarsZ-1 list polyrecon(oldvarsZ#i,q);
--	    oldrelsQ:=for i to #oldrelsZ-1 list polyrecon(oldrelsZ#i,q);
--	      oldmod:=q;
--	      stable=false;
--         )
--         else(
--            --find variables and relations mod the product of all primes so far
----                 toZ:=map(RZ,Rq);	      
--	    newvarsZ=for i to #oldvarsZ-1 list polyCRT(oldvarsZ#i,sub(fractions#i,RZ),oldmod,q);
--	    newrelsZ=for i to #oldrelsZ-1 list polyCRT(oldrelsZ#i,sub(relicR#i,RZ),oldmod,q);
--	      oldmod=oldmod*q;
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
--      )
--      else(
--	 q=q+1;
--	 stable=false;  
--      );
--   );
--   icR:=matrixOrder(QQ,grevlexWeight(wtR));
--   (oldvarsQ,wticR,icR,oldrelsQ)   
--);


--example 1, disguised Hermitian-----------------------------------------
wtR=matrix{{9,8}}
Rq=ZZ/17[y,x,Weights=>entries(weightGrevlex(wtR))]
Iq={y^8-x^9+2*y*x^6-y^2*x^3}
time qthIntegralclosure(wtR,Rq,Iq)
--time icp=icFracP(Rq/ideal(Iq))
wtR=matrix {{19, 15, 14, 10, 9, 8 ,5, 4}}
Rq=(ZZ/17)[p_0, p_1, p_2, p_3, p_4, p_7, p_5, p_6,Weights=>entries(weightGrevlex(wtR))]
Iq={p_0^2+p_1*p_7-p_2*p_7^3+p_7,p_0*p_1+2*p_0-p_3*p_7^3,p_0*p_2+p_3*p_7-p_4*p_7^3,p_0*p_3+p_2-p_5*p_7^3,p_0*p_4+p_5*p_7-p_6*p_7^3,p_0*p_5+p_4-p_7^3,p_0*p_6-p_1*p_7-p_7,p_1^2+3*p_1-p_2*p_7^2+2,p_1*p_2+2*p_2-p_5*p_7^3,p_1*p_3+2*p_3-p_4*p_7^2,p_1*p_4+2*p_4-p_7^3,p_1*p_5+2*p_5-p_6*p_7^2,p_1*p_6-p_0+p_6,p_2^2+p_5*p_7-p_6*p_7^3,p_2*p_3+p_4-p_7^3,p_2*p_4-p_1*p_7-p_7,p_2*p_5-p_0,p_2*p_6-p_3*p_7,p_3^2+p_5-p_6*p_7^2,p_3*p_4-p_0,p_3*p_5-p_1-1,p_3*p_6-p_2,p_4^2-p_3*p_7,p_4*p_5-p_2,p_4*p_6-p_5*p_7,p_5^2-p_3,p_5*p_6-p_4,p_6^2-p_7}

	 

--example 7, Leonard 2009 -----------------------------------------------
wtR:=matrix{{19,15,12,9,9},{12,9,9,9,0}}
Rq=ZZ/2[z19,y15,y12,x9,u9,Weights=>entries(weightGrevlex(wtR))]
Iq = {y15^2+y12*x9*u9,
      y15*y12+x9^2*u9+x9*u9^2+y15,
      y12^2+y15*x9+y15*u9+y12,
      z19^3+z19*(y15+y12)*(x9+1)*u9+(y15*(x9+u9)+y12*(x9*u9+1))*x9^2*u9}
time qthIntegralclosure(wtR,Rq,Iq)



-------------------------------------------------------------------------
wtR:=matrix{{31,12}}
Rq=ZZ/2[y,x,Weights=>entries(weightGrevlex(wtR))]
Iq={y^12+y^11+y^10*x^2+y^8*x^9+x^31}
time toString(qthIntegralclosure(wtR,Rq,Iq))





-----------------------------------------------
wtR:=matrix{{25,21}}
Rq:=ZZ/2[y,x,Weights=>entries(weightGrevlex(wtR))]
Iq:={
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

I:=ideal(Iq);
depno:=1;
fa=factor(qthConductor(I,depno));
fa
     
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
     footq =sort(delete(0_Rq,for f in footq list f%LMideal));
      degr:=#footq;
time fractions=icRfractions(I,depno,footq,Del=>sub(((fa#6)#0),Rq));
toString(fractions)
--time i0:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#0)#0)^((fa#0)#1),Rq));

--time i1:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#1)#0)^((fa#1)#1),Rq));

--time i2:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#2)#0)^((fa#2)#1),Rq));

--time i3:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#3)#0)^((fa#3)#1),Rq));

--time i4:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#4)#0)^((fa#4)#1),Rq));

--time i5:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#5)#0)^((fa#5)#1),Rq));

--time i6:=qthIntegralclosure(wtR,Rq,Iq,Delta=>sub(((fa#6)#0)^((fa#6)#1),Rq));











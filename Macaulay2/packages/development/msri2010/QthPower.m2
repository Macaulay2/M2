------------
-- Header --
------------
newPackage(
    "QthPower",
    Version => "0.1", 
    Date => "January 10, 2010",
    Authors => {{Name => "Douglas Leonard",
                 Email => "leonada@auburn.edu",
                 HomePage => "http://www.dms.auburn.edu/~leonada"}},
    Headline => "An implementation of the Qth-Power algorithm for computing the integral closure of a ring.",
    DebuggingMode => true
)

export {qConductor, qthPower};

------------
--  Code  --
------------

------------
-- Exportable methods --

-- Find an element D in the Noether noramlization of R and in the conductor of R,
-- with an option to provide that element for cases when this is called 
-- possibly unnecessarily by the qth-power method

--qConductor = method(TypicalValue => RingElement);
--qConductor (Ideal, ZZ) := (I, deps) -> (
--   R := ring I;
--   RP := ZZ/(char R)[gens R,MonomialOrder=>{Position=>Up,{deps,#gens R-deps}}];
--   IP := sub(I,RP);
--   GP := gens gb sub(transpose jacobian IP, RP/IP);
--   DP := 1;
--   j := 0;
--   for i to numRows GP-1 do(
--      gc = 0;
--      while GP_(i,j)==0 do j=j+1;
--      while max apply(deps, v -> degree(RP_v, leadMonomial(GP_(i,j))))==0 do(
--         gc = gcd(gc,GP_(i,j));
--         j=j+1;
--      );
--      if gc!=0 then DP=DP*gc;
--   );
--   sub(DP,R)
--);

qConductor = method(TypicalValue => RingElement,Options=>{Element=>null});
qConductor (Ideal, ZZ) := o->(I, deps) -> (
   if o.Element===null then(
      R := ring I;
      RP := ZZ/(char R)[gens R,MonomialOrder=>{Position=>Up,{deps,#gens R-deps}}];
      IP := sub(I,RP);
      GP := gens gb sub(transpose jacobian IP, RP/IP);
      DP := 1;
      col := 0;
      for row to numRows GP-1 do(
         gc = 0;
         while GP_(row,col)==0 do col=col+1;
         while max apply(deps, v -> degree(RP_v, leadMonomial(GP_(row,col))))==0 do(
            gc = gcd(gc,GP_(row,col));
            col=col+1;
         );
         if gc!=0 then DP=DP*gc;
      );
      sub(DP,R)
   )
   else( o.Element)
);

-- Find the numerators of the fractions defining the P-module presentation 
-- of the integral closure of a quotient ring 
-- using the qth-power algorithm,
-- the first numerator being also the common denominator, an element of P

qthPower = method(TypicalValue => List);
qthPower (Ideal, ZZ, List) := (I,deps,footprint) ->(
   -- Initialisation
   R := ring I;
   inds := #gens R -deps;
   depq := take(gens R, deps);
   indq := take(gens R, -inds);
   q := char R;
   qc := qConductor(I, deps);
   dq := qc^(q-1);
   g := footprint;
   h := apply(g,s->fastq(s,I));
   e := apply(h, s->red(s,I,dq,footprint));
   s := apply(#footprint, i->-1);
   now := 0;
   before := -1;
   oldg := footprint;
    
   -- local variables
   i := j := w := k := prod := mx := ll := ex := l := gx := pos := 0;
   skip := skip1 := updating := true;
   logei := logj := logk := posl := ww := {};
    
   -- Compute the next module generating set; 
   -- continue to loop as long as different modules are generated.
   loop := true;
   while loop do(
      loop = false;
      before = now-1;
      i = 0;
      skip = false;
      while i < #g do(	
         if s#i==before then(
            updating=true;
            -- look for the next unique leading monomial
            while updating do(
               updating=false;
               for j to i-1 do{
                  if leadMonomial(g#i)==leadMonomial(g#j) then(
                     i=i+1;
                     updating=true;
                     break;
                  );
               };
            );
            e=replace(i,red(e#i,I,dq,oldg),e);
            -- "row-reduction", that is, taking F-linear combinations
            j=0;
            while j<i and e#i!=0 do(
               if s#j==before and e#j!=0 then(
                  logei=logpoly(e#i,depq,indq);
                  logj=logpoly(e#j,depq,indq);	
                  if e#j!=0 
		  and logj#0==logei#0 
		  and geqlog(logei#2,logj#2) 
		  and apply((logei#2-logj#2), v->v%q)==apply(inds, v->0) 
		  then (
		     w = 1;
		     for k to inds-1 do w=w*(indq#k)^((logei#2-logj#2)#k//q);
			loop=true;
			if g#i>g#j*w then(
		           lc=leadCoefficient(e#i)//leadCoefficient(e#j);
                           g=replace(i,g#i-lc*(g#j)*w,g);
                           h=replace(i,(h#i-lc*(h#j)*w^q)%I,h);
	                   e=replace(i,red(e#i-lc*(e#j)*w^q,I,dq,oldg),e);
                           j=-1; 
			   -- updates to zero
	    		)
		        else if g#i<g#j*w then(
                           gx=(g#j)*w;
                           g=sort(append(g,gx));
                           pos=position(g, a->a==gx);
                           h=insert(pos,(h#j)*w^q,h);
                           e=insert(pos,red((e#j)*w^q,I,dq,oldg),e);
                           s=insert(pos,before,s);
			);   
                     );
                  );
                  j=j+1;
               );
               -- shifting, that is multiplication by a free variable
               if e#i==0 then(
                  s=replace(i,now,s);
               )
               else(
	          if s#i>=before then(
	             loop=true;   
                     logei=logpoly(e#i,depq,indq);
	             for j to #oldg-1 do(
	                logj=logpoly(dq*oldg#j,depq,indq);
	                if logj#1==logei#1 then(
	                   prod=1;
		           for k to inds-1 do(
		              mx=-((-(((logj)#2)#k-((logei)#2)#k))//q);
		              if mx>0 then prod=prod*(indq#k)^mx;
		           );
	                   if all(apply(#g,l->s#l>=before and g#l!=0 and leadMonomial(g#l)==leadMonomial(g#i*prod)), l->l==false) then(
                                 gx=(g#i)*prod;
                                 pos=position(sort(append(g,gx)),a->a==gx);
                                 g=insert(pos,gx,g);
                                 h=insert(pos,(h#i)*prod^q,h);
                                 e=insert(pos,red((e#i)*prod^q,I,dq,oldg),e);
                                 s=insert(pos,before,s);
                                 loop=true;
	                      );
	                   );          
	                );
                        for k to #g-1 do(
	                   if s#k>=before and g#k!=0 and e#k!=0 then(	 	 
	                      logk=logpoly(e#k,depq,indq);
	                      if k<i and logk#0==logei#0 then(
	                         if all(apply((logei#2-logk#2), v->v%q), v->v==0) then(
		                    ww=apply(inds, v->degree(indq#v,lcm(logei#3,logk#3)//logei#3));
                                    prod=product(#ww, l->(indq#l)^((ww#l)//q));
                                    gx=(g#i)*prod;
                                    g=sort(append(g,gx));
                                    pos=position(g,a->a==gx);
                                    h= insert(pos,(h#i)*prod^q,h);
                                    e=insert(pos,red((e#i)*prod^q,I,dq,oldg),e);
                                    s=insert(pos,before,s);
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
         now=now+1;
	 gg=toList g;
	--remove any elements that are zeroed out  
         posl=positions(gg, v->v!=0);
         g=g_posl;
         h=h_posl;
         e=e_posl;
         s=s_posl;
	 pos2=positions(s, v->v==before+1);
	--save only elemtns describing the next module 
	 g=g_pos2;
         h=h_pos2;
         e=e_pos2;
         s=s_pos2;
	 before=before+1;
         oldg=for i to #g-1 list if s#i==before and e#i==0 then g#i else continue;
         
	 
	 
         e=apply(h,s->red(s,I,dq,oldg));
      );

      -- paring down to the final set of numerators
      i=#oldg-1;
      while i>0 do(
         logi=logpoly(oldg#i,depq,indq);
         for j to i-1 do{
            logj=logpoly(oldg#j,depq,indq);
            if logi#0==logj#0 and geqlog(logi#2,logj#2) then(
	       oldg=delete(oldg#i,oldg);
	       break;
            );
         };
         i=i-1;
      );
      oldg
   );

--produce a presentation as a P-affine algebra
-- by producing the P-quadratic and P-linear relations 

presentationPalgebra = method(TypicalValue => List);
presentationPalgebra (List,Ideal,RingElement) := (oldvars,I,D) ->(

loop1:=true;lmi:=0;logi:=0;j:=0;lmj:=0;logj:=0;lc:=0;
lti:=0;ltj:=0;
H:=rsort(oldvars)

--compute the P-quadratic relations matrix, M
U:=new MutableList from 
   apply((first entries (symmetricPower(2,matrix{H}))),v->(v%I//(D )))
M:=mutableMatrix(ring I,#H,#U);

while loop1==true do(
   loop1=false;
   for i from 0 to #H-1 do(
      lmi=leadMonomial(H#i);
      logi=logev(lmi);	
      j=0;
      while j<#U do(
         if U#j!=0 then(
	    lmj=leadMonomial(U#j);
	    logj=logv(lmj);  
            if logj#0==logi#0 and geq(logj#2,logi#2) then(
               lc=leadCoefficient(U#j)//leadCoefficient(H#i);
	       lti=leadTerm(H#i);
	       ltj=leadTerm(U#j);
               M_(i,j)=M_(i,j)+ltj//lti;
               U#j=U#j-ltj//lti*H#i;
               j=0;
               loop1=true;
            )
            else(
               j=j+1;
            );
         )
         else(
            j=j+1;
         );
      );
   );
);

--compute the P-linear relations matrix,L
--construct the new ring icR
--turn both M and L into relations for the new ideal icI
-- return icR, icI, phi, psi, and the weight matrix explicitly

----------------
-- non-exported helper methods 
------------------------------

-- Reduction modulo \Delta^(q-1) times the previous module.

red = method(TypicalValue => RingElement);
red (RingElement, Ideal, RingElement, List) := (g, I, dq, modfoot) -> (
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

fastq = method(TypicalValue => RingElement);
fastq (RingElement, Ideal) := (g, I) -> (
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

logpoly = method(TypicalValue => List);
logpoly (RingElement, List, List) := (v, dep, ind) -> (
   lv := leadMonomial v;
   indlog := apply(#ind, i->degree(ind#i, lv));
   indprod := product(#ind, i->(ind#i)^(indlog#i));
   deplog := apply(#dep, i->degree(dep#i, lv));
   depprod := product(#dep, i->(dep#i)^(deplog#i));
   {deplog, depprod, indlog, indprod}
);

-- Compare "logs".

geqlog = method(TypicalValue => Boolean);
geqlog (List, List) := (v, w) -> (
    for i from 0 to #v-1 do if v#i < w#i then return false; true
);

----------------------------------------------
--non-exported basic number-theoretic methods
----------------------------------------------

--the extended Euclidean algorithm
eea = method(TypicalValue => List);
eea (ZZ,ZZ) :=(a,b)->(
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

crt=method(TypicalValue=>RingElement)
crt (ZZ,ZZ,ZZ,ZZ):=(c,d,p,n)->(
     (r,u,v):=eea(p,n);
     (-c*u#-2*n+d*v#-2*p)*(-1)^(#r)
     );

--the Chinese remainder theorem for two polynomials 

polycrt=method(TypicalValue=>RingElement)
polycrt(RingElement,RingElement,ZZ,ZZ):=(poly1,poly2,mod1,mod2)->(
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
recon:=method(TypicalValue=>List)
recon(ZZ,ZZ):=(a,n)->(
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

-----------------------------------------------------
--non-exported methods for defining monomial orderings from matrices
-------------------------------------------------------

--defining the grevlex over weight monomial ordering matrix
grevlexWeight=method(TypicalValue=>Matrix)
grevlexWeight(Matrix):=(matr)->(
grev:=for i to numColumns(matr)-numRows(matr)-1 list (
         for j to numColumns(matr)-1 list(
	    if i+j<numColumns(matr)-numRows(matr) then 1 else 0
            )
         );
         matrix(grev)||matr
     );

--defining the weight-over-grevlex monomial ordering matrix
weightGrevlex=method(TypicalValue=>Matrix)
weightGrevlex(Matrix):=(matr)->(
grev:=for i to numColumns(matr)-numRows(matr)-1 list (
         for j to numColumns(matr)-1 list(
	    if i+j<numColumns(matr)-numRows(matr) then 1 else 0
            )
         );
         matr||matrix(grev)
     );


--defining a ring with a monomial ordering given by a matrix
--not just from the two types of matrices defined above

matrixOrder=method(TypicalValue=>List)
matrixOrder(Ring,Matrix):=(field,matr)->(
     field[Variables=>numColumns matr,Weights=>entries matr]
);   




-------------------
-- Documentation --
-------------------

beginDocumentation()
doc ///
    Key
        QthPower
    Headline
        An implementation of the Qth-Power algorithm for computing the integral closure of a ring.
    Description
        Text
            This package is an alternative method for computing the integral closure of a ring.
///

doc ///
    Key
        qConductor
        (qConductor, Ideal, ZZ)
    Headline
        Computes the conductor of a ring where the number of dependent variables is known.
    Usage
        qConductor(I, deps)
    Inputs
        I:Ideal
        deps:ZZ
    Outputs
        D:RingElement
    Description
        Text
            {\tt D} is a conductor of the quotient ring of {\tt I} where there are {\tt deps} dependent variables.
        Example
            Rq = ZZ/23[y,x, MonomialOrder => {Weights => {11,7}, Weights => {1,0}}];
            deps = 1;
            Iq = ideal (y^7+y^6*(3*x+1)+y^5*(3*x^3+6*x^2)+y^4*9*x^4+y^3*(4*x^6-x^5)-3*y^2*x^7-3*y*x^9-x^11);
            D = qConductor(Iq, deps)
///

end



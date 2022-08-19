newPackage("FunctionFieldDesingularization",
     Version => "1.0", 
     Date => "September 13, 2021",
     Authors => {{Name => "Douglas A. Leonard", Email => "leonada@auburn.edu"}},
     Headline => "desingularization of function fields"
     )

export {"negLexMatrix", "arcs"}

----------------------------------------
--exported matrix for input as local ring
----------------------------------------
negLexMatrix = method()
negLexMatrix ZZ := d->matrix(
   for i to d list( 
      for j to d list( 
           if j==d-i then -1 else 0
      )
   )
);
------------------------------------------------------------------------
--Unexported subroutines for arcs---------------------------------------
------------------------------------------------------------------------
--basic list operations
------------------------------------------------------------------------
GE=(uuu,vvv)->min(for i to #uuu-1 list uuu#i-vvv#i)>=0;
------------------------------------------------------------------------
GT=(uuu,vvv)->min(for i to #uuu-1 list uuu#i-vvv#i)>0;
------------------------------------------------------------------------
GC=(L)->( f := for i to #L-1 list( if L#i!=0 then L#i else continue); 
          g := gcd(f); 
          L//g
	);
------list A\ list B----------------------------------------------------
AnotB=(A,B)->(for i in A do( 
	         for j in B do if i==j then A=delete(j,A)
		 ); 
	         A
	     );
-------list A intersect list B------------------------------------------
AcapB=(A,B)-> AnotB(A,AnotB(A,B));
-------elements of list A with zeroth entry negative--------------------
negA=(A)->for i in A list if i#0<0 then i else continue;
-------elements of list A with zeroth entry 0---------------------------
zerA=(A)->for i in A list if i#0==0 then i else continue;
-------cyclic shift of first m entries of elements of list A------------ 
cycshft=(A,m)->(for i to m-1 list A#(i+1))|{A#0}|
               (for j to #A-m-2 list A#(m+j+1));
------------------------------------------------------------------------
------------------------------------------------------------------------
--orthogonal complement of integer list m of length n over N------------
------------------------------------------------------------------------
orth=(m,n)->(
   matrix(
      flatten(
	 for i to n list(
            if m#i==0 then( 
              {for j to n list(
	          if j==i then 1 else 0
	      )}
	      )
              else if m#i<0 then(
                 for k to n list( 
	            if m#k>0 then(
	               for l to n list(
	                  if l==i then m#k else 
	                     if l==k then -m#i else 0
		        )
	            )
	            else continue   
                 )
            )
            else continue
         )
      )
   )
);
------------------------------------------------------------------------
-------reducing b in N by elements of pos-------------------------------
--red=(b,pos)->( 
--    i:=0; 
--    bnew := b;
--    while i<#pos and bnew != list0 do( 
--	if GE(bnew,pos#i) 
--	and bnew != list0 
--	then bnew = bnew-pos#i 
--	else i=i+1
--	);
--    GC(bnew)
--    );
------------------------------------------------------------------------
weights=(M,I,J,N,d)->(
   m:=N+d-2;
   O:=for k2 to N+d-2 list 0;
   ainit:=Ainit(M,I,J,N,d);
------------------------------------------------------------------------
   if #(M#0)!=2 then(  
      neg := negA(ainit);
      zer := zerA(ainit);
      pos := AnotB(AnotB(ainit,neg),zer);
      if #pos>0 then(
         i:=0;
         while  i<N-2  do(
            j:=0;
            while j< #neg   do(
               k:=0;
               while k < #pos   do(
                  v:=GC(for l to m list neg#j#l+pos#k#l);
                  if v#i<0 then(
                     for i1 to #neg-1 do(
                        if neg#i1#i==v#i and GE(v,neg#i1) then(
		           vv:=for j1 to m list v#j1-neg#i1#j1;
			   if vv!=O then( 
			      v=GC(vv); 
			   )    
			   else(
			      v=O;
		           );		
			);	 
		     );	     
		  ); 
                  --reduction
                  l1 := 0; 
                  zp:=zer|pos;
                  while #zp>0 and l1<#zp do( 
	             if GE(v,zp#l1) then( 
	                vv := for k1 to m list v#k1-zp#l1#k1;
		        if vv!=O then v=GC(vv) else v=vv;
			l1 = 0;
		     )
		     else( 
                        l1 = l1+1;
                     );
		  );
                  --appending
                  if v#i<0 then neg=sort unique append(neg,v);
		  if v#i==0 and v!=O then zer=unique append(zer,v);
		  if v#i>0 then pos=unique append(pos,v);
	          k=k+1;
               );
               j=j+1;
            );
	 i=i+1;
         zp=sort unique(zer|pos);
         if #zp>0 then(
            neg=sort(for i2 to #zp-1 list if zp#i2#i<0 then zp#i2 else continue);
	    zer=sort(for j2 to #zp-1 list if zp#j2#i==0 then zp#j2 else continue);
            pos=sort(for k2 to #zp-1 list if zp#k2#i>0 then zp#k2 else continue);
         );
      );
   );
   );
   if #(M#0)==2 then ainit else zer|pos
);
------------------------------------------------------------------------
------------------------------------------------------------------------
printWeights=(R,b,IM)->(
   L1 := {}; 
   iM := flatten entries gens IM;
   N  := #iM;
   MM := unique(for i to #iM-1 list 
                   for j to numgens(R)-1 list 
		      degree(R_j,iM#i));
   M  := for i to #MM-1 list 
           for j to #MM-1 list 
              for k to numgens(R)-1 list MM#j#k-MM#i#k;
         for i to N-2 do(
            for j from i+1 to N-1 do(
               S := weights(M,i,j,N,numgens(R)-1);
	       if S != {} then(
                  L1 = append(L1,S);
	       );
            );
         );
         L4 := {};
         if L1 != {} then(
            L2 := transpose matrix(flatten (L1));
            L3 := unique(for i to numColumns(L2)-1 list L2_i);
            L4  = for i to #L3-1 list if sum(entries L3#i)>1 then 
	       flatten entries transpose matrix(L3#i) else continue;
         );
         L4
   );
------------------------------------------------------------------------
--A Gr\"obner basis should be an ideal, not a matrix--------------------
GB=(I)-> ideal(flatten entries gens gb I);
------------------------------------------------------------------------
redneg=(f)->(
    if #f==0 then(
       f
    )
    else(
       s:=unique flatten (
       for i to #f-1 list( 
          g := factor(f#i); 
          for j to #g-1 list g#j#0
          )
       );
       ss:=for i to #s-1 list( 
              for j to numgens(ring(f#0))-1 list degree((ring(f#0))_j,s#i)
           );
       for i to #ss-1 list( 
           if sum(ss#i)>0 then s#i else continue
       )
    )	       
);
------------------------------------------------------------------------
distinctFactors=(nlist)->(
   n1 := for i to #nlist-1 list factor(nlist#i);
   n2 := for i to #n1-1 list for j to #(n1#i)-1 list n1#i#j#0; 
   unique flatten n2
);
------------------------------------------------------------------------
saturateByList=(nlist,eideal)->(
   e1 := eideal;
   for i to #nlist-1 do (e1 = saturate(e1,ideal(nlist#i)););
   e1
);
------------------------------------------------------------------------
------------------------------------------------------------------------
-- gblist appends an element v to a list, reduces the ideal generated by
-- the appended list and produces a list of gb elements for it
gblist:=(init,newelement)-> 
   ideal flatten entries gens gb (init+newelement);
------------------------------------------------------------------------
redgE:=(g,E,P)->(
    e := ideal(for i to numgens(E)-1 list (E_i)*P_0^0);
    (g*P_0^0)%e
);

redfE:=(f,E,R)->(
    e := ideal(for i to numgens(E)-1 list (E_i)*R_0^0);
    (f*R_0^0)%e 
);
------------------------------------------------------------------------
--initialization function for the i,j weight computation----------------
--run this for 0<=i<j<=N------------------------------------------------
Ainit=(M,i,j,N,d)->( 
    Sinit := (orth(M#i#j,d)*transpose matrix(M#i))
                  _(AnotB(for k to N-1 list k,{i,j}));
    Tinit := matrix entries(orth(M#i#j,d));
    sort(apply(entries(Sinit|Tinit),k->GC(k)))
);
------------------------------------------------------------------------
------------------------------------------------------------------------
possibleInit=(b,initialNEQ,initialEQ)->(
    -- b an element of R, 
    -- initialNEQ a list of elements of P, 
    -- initialEQ an ideal of P
   R            := ring(b);
   P            := coefficientRing(R);
   F            := coefficientRing(P);
   d            := numgens(R)-1;
   localMap     := map(R,R,matrix{for i to d list P_i+R_i});
   constantTerm := map(R,R,matrix{ for i to d list promote(P_i,R)});
------------------------------------------------------------------------
-- local affine polynomial, since the constant term is assumed to be zero
   Bbar        := localMap(b)-constantTerm(b);
-- Vbar, the list of monomials supporting B
   Vbar        := flatten entries (coefficients(Bbar))#0;
-- Cbar, the corresponding list of coefficients
   Cbar        := flatten entries transpose (coefficients(Bbar))#1;
-- genericInit, the minimal monomials with constant coefficients in Bbar
   genericInit := flatten entries gens gb ideal(
      for i to #Cbar-1 list( 
 	 if (degree(Cbar#i))#1==0 then Vbar#i else continue
      ) 
    );
-- standard is the sum of standard terms
   standard    := Bbar%ideal genericInit;
-- V is the list of standard monomials of B
   V           := flatten entries (coefficients(standard))#0;
-- C is the corresponding list of coefficients
   C           := flatten entries transpose (coefficients(standard))#1;
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Start with EQ the ideal generated by the constant term
   NEQ  := {initialNEQ};--list of lists of P elements
   EQ   := {initialEQ};--list of P ideals
   INIT := {ideal(genericInit)};--list of R ideals
   TAIL := {standard};--list of R elements
------------------------------------------------------------------------
------------------------------------------------------------------------
   k := 0;
   while k < #NEQ do(
      tail := TAIL#k;
      eqk1 := GB(EQ#k);
      if tail != 0 then(
         monid    := ideal(leadMonomial(tail));
         newcoeff := redgE(lift(leadCoefficient(tail),P),eqk1,P);
         neqk1    := flatten({NEQ#k}|{newcoeff});
	 neqk1     = flatten(for jj to #neqk1-1 list distinctFactors({neqk1#jj}));
         if neqk1 != {} then neqk1 = unique(neqk1);
         if all(neqk1,i->i!=0) then(
            NEQ  = append(NEQ,neqk1);
            EQ   = append(EQ,eqk1);
            INIT = append(INIT,gblist(INIT#k,monid));
            TAIL = append(TAIL,redfE(redfE(tail,monid,R),eqk1,R));
         );
         dist := product(distinctFactors({newcoeff}));
         eqka := GB(eqk1+ideal(dist));
         genlista := flatten entries gens eqka;
	 eqkb := ideal(flatten(for jj to #genlista-1 list product(distinctFactors({genlista#jj}))));
         eqkc := GB(eqkb);
         if eqkc != ideal(promote(1,ring(eqkc))) then(
	    genlistc := flatten entries gens eqkc;
	    eqkd     := ideal(flatten(for jj to #genlistc-1 list product(distinctFactors({genlistc#jj}))));
            eqk2     := GB(eqkd);
            neqk2    := unique(for i to #(NEQ#k)-1 list ((NEQ#k)#i)%eqk2);
            neqk2 = distinctFactors(neqk2);
            if all(neqk2,i->i!=0) then(
               EQ   = append(EQ,eqk2);
	       NEQ  = append(NEQ,neqk2);
               INIT = append(INIT,INIT#k);
               TAIL = append(TAIL,(tail-leadTerm(tail))%promote(eqk2,R)); 
            );
         );
      ); 
------------------------------------------------------------------------
      k=k+1;
   );
------------------------------------------------------------------------
-- returning sublists NEQ, EQ, INIT
   newinits:=(
      NEQ=for i to #NEQ-1 list
        if #(NEQ#i)>0 then 
                distinctFactors(NEQ#i) 
        else NEQ#i;
        for i to #NEQ-1 list (
           if TAIL#i==0 
	   and unique(NEQ#i) != {0} 
	   and promote(EQ#i,P)!=ideal(promote(1,P)) 
           then (i,unique(NEQ#i),GB(EQ#i),(INIT#i)) 
           else continue)
        );
      newinits
   );
-----------------------------------------------------------------------
----printing results routine ------------------------------------------
--with input the output of possibleInit(F,b,d)-------------------------
printInit=(A,d)->(
   NEQ  := {};
   EQ   := {};
   INIT := {};
   for i to #A-1 do(
      B1 := A#i#1;
      B2 := for j to #(B1)-1 list 
               if degree(B1#j)!= (for k to d list 0) 
	       then B1#j 
               else continue;
      C1 := A#i#2;
      if numgens(C1)>0 then(
         for j to #B2-1 do C1=saturate(C1,ideal(B2#j));
         C1 = GB(C1);
         r  := ring(C1);
         if C1  != ideal((r_0)^0) then(
	    NEQ  = append(NEQ,B2);
	    EQ   = append(EQ,C1);
	    INIT = append(INIT,A#i#3);
         );
      ); 	 
   );    
   {NEQ,EQ,INIT}
);
-------------------------------------------------------------------------
-------------------------------------------------------------------------
--turns a translation and a minWeight into 
-- a unimodularTransformation map on the series ring R with coeffs from P
-------------------------------------------------------------------------
unimodular=(w,P,R,E)->(
   d   := numgens(R)-1;    
   M   := id_(ZZ^(d+1));
   M    = for i to d list M_i;
   i := 0;
   while w#i==0 and i<d do (i=i+1;);
   while sum(w) >1 do(
      for j to d do( if w#j!=0 and w#j<w#i then i=j);
      q := for k to d list w#k//w#i;
      M  = (for l to d list( if l!=i then M_l else sum(q,M,(i,m)->i*m)));
      w  = for n to d list if n!=i then (w#n)%(w#i) else w#i;
   );
   M = rsort(matrix(M));
   map(R,R, matrix{for i1 to d list 
      promote((P_i1)%E,R)+product(for j1 to d list R_(d-j1)^((M_j1)_i1))
	          }
      )
);
--------------------------------------------------------------------------
--promote all the entries of m to R and make it into a matrix
--in case all the entries of m are in the coeff ring P
-- and M2 doesn't know to promote then to R in defining a map from R to R
-- with matrix m with entries all from P
pmatrix = (R,m)-> matrix{for i to #m-1 list promote(m#i,R)};
--------------------------------------------------------------------------
--constant=(R)->map(R,R,matrix{for i to d list 0});
--------------------------------------------------------------------------
rho = (phim,E,b,P,R)->(
    d :=numgens(R)-1;
   fm    := (phim(b))%promote(E,R);
   con   := map(R,R,matrix{for i to d list 0});
   cm    := fm-con(fm);
   tm    := terms(P,cm);
   dgm   := for i to d list
              for j to #tm-1 list degree(R_i,tm#j);
   mdegm := for i to d list min(dgm#i);
   for i to d do cm=cm//(R_i^(mdegm#i));
   cm
);
---------------------------------------------------------------------
--Main exported method
-----------------------------------------------------------------------
arcs = method()
arcs(RingElement,Ideal,List,File):=(polyb,eq,ineq,fout)->(
-----------------------------------------------------------------------
--Initialization
-----------------------------------------------------------------------
   R           := ring(polyb);
   P           := coefficientRing(R);
   d           := numgens(R)-1;
   prevlist    := {-1};
   levellist   := {0};
   Plist       := {P};
   a           := getSymbol "a";
   for i to 12 do Plist=append(Plist,Plist#i[a_{i+1,0}..a_{i+1,d}]);
   a = value a;
   Rlist       := {R};
   x           := getSymbol "x";
   for i to 12 do Rlist=append(Rlist,Plist#(i+1)[x_{i+1,0}..x_{i+1,d},
	           Weights=> entries negLexMatrix(d),Global=>false]);
   x = value x;
   blist       := {polyb};
   clist       := {polyb%promote(eq,R)};
   nlist       := {ineq};
   elist       := {eq};
   philist     := {map(R,R,matrix{gens(R)})};
   Philist     := {map(R,R,matrix{gens(R)})};
   leaflist    := {"root"};
   currentnode := 0;
   lastnode    := 0;
   globalpars  := 0;
   nodenumber  := 0;
   i0          := 1;
   nextlevel   := 1;
   fout         << "===================================" << endl;
-----------------------------------------------------------------------
   while currentnode<=lastnode and globalpars==0 do(
      nextlevel = 1+levellist#currentnode;
      polyb = blist#currentnode;
      ineq  = nlist#currentnode;
      eq    = elist#currentnode;
      R     = ring(polyb);
      P     = coefficientRing(R);
-----------------------------------------------------------------------
--computation of INITs-------------------------------------------------
-----------------------------------------------------------------------  
      PI     := possibleInit(polyb,ineq,eq);
      Parts  := printInit(PI,d);
      NEQ1   := Parts#0;
      EQ1    := Parts#1;
      INIT1  := Parts#2;
-----------------------------------------------------------------------
--computation of weights and unimodular transformations for each case
-----------------------------------------------------------------------
--initialize neq1,eq1,ne1,WT,wt,test,
--taustart,phistar,Pstar,Rstar,bnew,cnew, GBB,linear
      for Partnumber to #EQ1-1 do(
         if globalpars == 0 then(    
            neq1 := redneg(NEQ1#Partnumber);
            eq1  := EQ1#Partnumber;
            ne1  := distinctFactors(for j to #neq1-1 list (neq1#j)%eq1);
            ne1   = for i to #ne1-1 list if max(for j to d list degree(R_j,ne1#i))>0 
	                                 then ne1#i 
					 else continue;
            if eq1 != promote(ideal(1),ring(eq1)) then(
               WT := printWeights(R,polyb,INIT1#Partnumber);
               for i1 to #WT-1 do(
	          if globalpars==0 then(    
                     wt   := for j from #(WT#i1)-d-1 to #(WT#i1)-1 list WT#i1#j;
	             test := true;
                     for j to d do if ((Plist#(levellist#currentnode)_j)%eq1)==0 
		                    and wt#d==0 then test=false 
				    else continue;
                     if sum(wt) !=1 and test==true 
	             then(
                        phi := unimodular(wt,P,R,eq1);
	                fout << "-------------------" << endl;
                        fout << "currentnode=" << currentnode << 
			        ", lastnode=" << lastnode << 
				",  lastvertex=" << i0 << endl;
	                fout << "-------------------" << endl;
                        i0 = i0+1;
                        taustar := map(Plist#(nextlevel),R,
			                 matrix{gens Plist#(nextlevel)});
                        phistar := map(Rlist#(1+levellist#currentnode),R,
 			                 matrix{gens Rlist#(nextlevel)});
	                Pstar   := Plist#(nextlevel);
	                Rstar   := Rlist#(nextlevel);
	                bnew    := (rho(phistar*phi,eq1,polyb,Pstar,Rstar));
	                if degree(Rstar_d,bnew)<2 then globalpars=1;
			cnew    := bnew%ideal(Rstar_d);
			--------print to file--------------------------------
	                fout << endl << "phi=" << endl;
                        for i to d do fout << toString((phistar*phi)(R_i)) << endl;
	                fout << endl;
                        fout << endl << "Phi=" << endl;
                        for i to d do 
	                   fout << toString((phistar*phi*Philist#currentnode)((Rlist#0)_i)) 
			        << endl;
	                   fout << endl;	       
	                   fout << "Part#=" << Partnumber << endl;
	                   fout << endl << "neq=";
                           fout << toString(for i to #ne1-1 list 
			               promote(ne1#i,Plist#(nextlevel))) << endl;
                           fout << endl << "eq=";
	                   GBB := GB (ideal(Pstar_d)
                                 +ideal((map(Pstar,Rstar,matrix{gens Pstar}))(bnew))
		                 +promote(eq1,Pstar)
		                 );
	                   fout << toString(GBB) << endl << endl;
	                   fout << "b=" << toString(bnew) << endl << endl;
			   mindeg:=min(for j to d list degree(promote(x_{nextlevel,j},ring(bnew)),bnew));
			   if mindeg==0 then print("global parameter found");
			   if mindeg==1 then print("global parametrization found");    
			   --------------------------------------------------------- 
	                   linear := "not a leaf";
                           dc := for i to d-1 list degree(R_i,cnew);
                           if max(dc)>0 then(
                              dcplus := for i to d-1 list if dc_i != 0 then dc_i 
		                                          else continue;				  
                              if min(dcplus)==1 then(
		                 linear = "leaf";
                              );	
	                      fout << linear << endl;
		              if linear != "leaf" then(
		                 nodenumber = 1+nodenumber; 
		                 fout << "nodenumber=" << nodenumber << endl; 
		                 lastnode = 1+lastnode;			
                            --updating node lists
		                 prevlist  = append(prevlist,currentnode);        
                                 levellist = append(levellist,nextlevel);
	                         blist     = append(blist, bnew); 
                                 clist     = append(clist, cnew);
                                 nlist     = append(nlist, (for i to #ne1-1 list 
			                            promote(ne1#i,Pstar)));
                                 elist     = append(elist,GBB);
                                 philist   = append(philist, phistar*phi);
		                 leaflist  = append(leaflist,linear);
                                 Philist   = append(Philist, phistar*phi*Philist#(currentnode));
	                   );   
                        ); 
	             ); 
	          );
               );
            );
         ); 
      );
         currentnode     = 1+currentnode;
--         if currentnode <= lastnode then print(currentnode,lastnode);
   ); 
   {prevlist,levellist,clist,blist,nlist,elist,philist,leaflist,Philist}
);

-----------------------------------------------
-- documentation and tests
-----------------------------------------------

beginDocumentation()

document {
     Key => {
	 FunctionFieldDesingularization
	 },
     
     Headline => "Strong desingularization of a function field
     in arbitrary characteristic and dimension",
     
         "Strong desingularization of a function field
     in arbitrary characteristic and dimension,
     using the algorithm in the paper 
     Desingularization of Function Fields, Leonard, arXiv1912.08663, December, 2019.",
     }

document {
     Key => {
	  negLexMatrix
	  },
     Headline => "a local (series) monomial ordering matrix for the input of arcs",
     Usage => "negLexMatrix(d)",
     Inputs => {
	  "d" => "the dimension, a positive integer"
	  },
     Outputs => { 
	  "negLexMatrix" => Matrix => "negative lex d\times d matrix" 
	  },
     EXAMPLE lines ///
          M = negLexMatrix 10;
          ///,
     PARA {
	  "The rings should be poly rings over poly rings with local monomial orerings to emphasize series" 
	  }
     }	

document {
     Key => {
	  arcs
	  },
     Headline => "prints node labels for the desingularization tree",
     Usage => "(prevlist,levellist,clist,blist,nlist,elist,philist,leaflist,Philist) = arcs(b0,n0e0,fout)",
     Inputs => {
	  "polyb" => "irreducible polynomial for the domain A_k",
	  "ineq"  => "list of inequality constraints for the part",
 	  "eq"    => "ideal of equality constraints for the part",
          "fout"  => "output file to which results are written"
	  },
     Outputs => {
	  "prevlist"  => List => "previous node in tree list", 
	  "levellist" => List => "level determines the rings being used",
          "clist"     => List => "irreducible mod x_d",
	  "blist"     => List => "irreducible",
	  "nlist"     => List => "inequality constraints",
	  "elist"     => List => "ideal of equality constraints",
	  "philist"   => List => "birational change of variables maps between node and previous node",
          "leaflist"  => List => "leaf of tree or not",
	  "Philist"   => List => "birational change of variables maps between node and root node"
       	  },
     EXAMPLE lines ///
      fout = openOut "curve_example0";
         F = QQ;
         d = 1;
        P0 = F[a_{0,0}..a_{0,d}];
        R0 = P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
        b0 = x_{0,0}^3+x_{0,0}*x_{0,1}+x_{0,1}^5;
        n0 = {};
        e0 = ideal(a_{0,0}^3+a_{0,0}*a_{0,1}+a_{0,1}^5);
      tree = arcs(b0,e0,n0,fout);
        b1 = x_{0,0}^3+x_{0,0}^2*x_{0,1}^4+x_{0,1}^5;
        n1 = {};
        e1 = ideal(a_{0,0},a_{0,1});
      tree = arcs(b1,e1,n1,fout);     
      fout << close;
          ///,
     PARA {
     	  "Each node is described by an irreducible polynomial bnew,
	  cnew its reduction mod x_d, a list of inequality constraints,
	  an ideal of equality constraints, a birational change-of-variables
	  from its previous node, and one from the root as well. 
	  See the test examples for syntax." 
     	  }
     }	   

TEST ///
   fout = openOut "/dev/null";
         F = QQ;
         d = 1;
        P0 = F[a_{0,0}..a_{0,d}];
        R0 = P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
        b0 = x_{0,0}^3+x_{0,0}*x_{0,1}+x_{0,1}^5;
        n0 = {};
        e0 = ideal(a_{0,0}^3+a_{0,0}*a_{0,1}+a_{0,1}^5);
      tree = arcs(b0,e0,n0,fout);
        b1 = x_{0,0}^3+x_{0,0}^2*x_{0,1}^4+x_{0,1}^5;
        n1 = {};
        e1 = ideal(a_{0,0},a_{0,1});
      tree = arcs(b1,e1,n1,fout);     
      fout << close;
      -- assert(globalpars==0);
///
TEST ///
      fout := openOut "/dev/null" 
         F := ZZ/2
         d := 1
        P0 := F[a_{0,0}..a_{0,d}];
        R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
        b0 := x_{0,0}^17+
              x_{0,0}^7*x_{0,1}^6+
	      x_{0,1}^25;
        n0 := {};
        e0 := ideal(a_{0,0}^17+
	            a_{0,0}^7*a_{0,1}^6+
	            a_{0,1}^25);
time tree1 := arcs(b0,e0,n0,fout);
        b1 := x_{0,0}^17+
              x_{0,0}^10*x_{0,1}^19+
	      x_{0,1}^25;
        n1 := {};
        e1 := ideal(a_{0,0},
	            a_{0,1});
time tree2 := arcs(b1,e1,n1,fout);     
      fout << close
      -- assert(globalpars==0);
///      
TEST ///
     fout := openOut "/dev/null";
        F := QQ
        d := 2
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
       b0 := x_{0,0}^3+
            (x_{0,1}^2-x_{0,2}^6)^2+
	     x_{0,2}^21;
       n0 := {};
       e0 := ideal((a_{0,0}^3+
	           (a_{0,1}^2-a_{0,2}^6)^2+
	            a_{0,2}^21));
time tree := arcs(b0,e0,n0,fout);
     fout << close
     assert(#tree#0==5)
///     
TEST ///
     fout := openOut "/dev/null";
        F := QQ
        d := 2
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
       b0 := x_{0,0}^3+
            (x_{0,1}^2-x_{0,2}^3)^2+
	     x_{0,2}^8;
       n0 := {};
       e0 := ideal((a_{0,0}^3+
	           (a_{0,1}^2-a_{0,2}^3)^2+
	            a_{0,2}^8));
time tree := arcs(b0,e0,n0,fout);
     fout << close
     assert(#tree#0==4)
///     
TEST ///
     fout := openOut "/dev/null"
        F := QQ
        d := 2
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
       b0 := x_{0,0}^4+
             x_{0,0}^2*x_{0,1}*x_{0,2}^9+
	     x_{0,0}*x_{0,1}^9*x_{0,2}+
	     x_{0,1}^5*x_{0,2}^5;
       n0 := {};
       e0 := ideal(a_{0,0}^4+
	           a_{0,0}^2*a_{0,1}*a_{0,2}^9+
	           a_{0,0}*a_{0,1}^9*a_{0,2}+
		   a_{0,1}^5*a_{0,2}^5);
time tree := arcs(b0,e0,n0,fout);
     fout << close
     assert(#tree#0==8)
///
TEST ///
     fout := openOut "/dev/null"
        F := ZZ/2
        d := 3
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	       Weights=> entries negLexMatrix(d),Global=>false];
       b0 := x_{0,0}^2+
             x_{0,1}^3*x_{0,2}+
	     x_{0,1}*x_{0,3}^3+
	     x_{0,3}*x_{0,2}^7;
       n0 := {};
       e0 := ideal(a_{0,0}^2+
	           a_{0,1}^3*a_{0,2}+
	           a_{0,1}*a_{0,3}^3+
		   a_{0,3}*a_{0,2}^7);
time tree := arcs(b0,e0,n0,fout);
        d := 2
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
       B0 := x_{0, 1}^3+
             x_{0, 0}^2*x_{0, 2}+
	     x_{0, 1}*x_{0, 2}+
	     x_{0, 2}^2;
       N0 := {};
       E0 := ideal(a_{0, 1}^3+
	           a_{0, 0}^2*a_{0, 2}+
		   a_{0, 1}*a_{0, 2}+
		   a_{0, 2}^2);
time Tree := arcs(B0,E0,N0,fout);
     fout << close
     assert(#tree#0==2)
///
TEST ///
     fout := openOut "/dev/null"
        F := ZZ/2
        d := 2
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
       b0 := x_{0,0}^2+
             x_{0,1}^4*x_{0,2}+
	     x_{0,1}^2*x_{0,2}^4+
	     x_{0,2}^7;
       n0 := {};
       e0 := ideal(a_{0,0}^2+
	           a_{0,1}^4*a_{0,2}+
		   a_{0,1}^2*a_{0,2}^4+
		   a_{0,2}^7);
time tree := arcs(b0,e0,n0,fout);
        d := 1
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];   
       B0 := x_{0, 0}^4+
             x_{0, 1}+
	     x_{0, 0}^2*x_{0, 1}+
	     x_{0, 1}^2;
       N0 := {};
       E0 := ideal(a_{0, 0}^4+
	           a_{0, 1}+
		   a_{0, 0}^2*a_{0, 1}+
		   a_{0, 1}^2);
time Tree := arcs(B0,E0,N0,fout);
     fout << close
     assert(#tree#0==2)
///
TEST ///
     fout := openOut "/dev/null"
        F := ZZ/2
        d := 2
       P0 := F[a_{0,0}..a_{0,d}];
       R0 := P0[x_{0,0}..x_{0,d},
	        Weights=> entries negLexMatrix(d),Global=>false];
       b0 := x_{0,0}+
	     x_{0,1}^2+
	     x_{0,2}^3+
	     x_{0,0}^2*x_{0,1}*x_{0,2};
       n0 := {};
       e0 := ideal(a_{0,0}+
		   a_{0,1}^2+
		   a_{0,2}^3+
		   a_{0,0}^2*a_{0,1}*a_{0,2});
time tree := arcs(b0,e0,n0,fout);
     fout << close
     assert(#tree#0==1)
///

end

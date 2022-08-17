-- -*- coding: utf-8-unix -*-

newPackage(
	"KustinMiller",
    	Version => "1.4",
    	Date => "May 14, 2012",
    	Authors => {{Name => "Janko Boehm", 
		  Email => "boehm@mathematik.uni-kl.de", 
		  HomePage => "http://www.math.uni-sb.de/ag/schreyer/jb/"},
                  {Name => "Stavros Papadakis", 
		  Email => "papadak@math.ist.utl.pt", 
		  HomePage => "http://www.math.ist.utl.pt/~papadak/"}
                   },
    	Headline => "unprojection and the Kustin-Miller complex construction",
	Keywords => {"Commutative Algebra"},
	PackageExports => {"SimplicialComplexes"},
    	DebuggingMode => true,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Implementing the Kustin-Miller complex construction",
	     "acceptance date" => "2012-05-07",
	     "published article URI" => "http://j-sag.org/Volume4/jsag-2-2012.pdf",
	     "published code URI" => "http://j-sag.org/Volume4/KustinMiller.m2",
	     "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/KustinMiller.m2",
	     "release at publication" => "a611bb9148103fa0c7908595cc979c66d210bb70",
	     "version at publication" => "1.4",
	     "volume number" => "4",
	     "volume URI" => "http://j-sag.org/Volume4/"
	     }
        )


-------------------------------------------------------------------------------

-*
Copyright [2011] [Janko Boehm, Stavros Papadakis]

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, see <http://www.gnu.org/licenses/>
*-


-*

      Installation:

      Put the file KustinMiller.m2 somewhere into the path of Macaulay2      
      (usually into the directory .Macaulay2/code inside your home directory, type
      path in M2 to see the path) and do inside M2

      installPackage "KustinMiller"
      
      This package requires the package SimplicialComplexes.m2 Version 1.2 or higher,
      so install this first.

*-

--------------------------------------------------------------------
-- the commands available to the user:

export {
    "kustinMillerComplex",
    "unprojectionHomomorphism",
    "resBE",
    "delta",
    "isExactRes",
    "Tom",
    "Jerry",
    "Face",
    "face",
    "isSubface",
    "isFaceof"
    }

if version#"VERSION" < "1.4" then error "This package was written for Macaulay2 Version 1.4 or higher.";
if (options SimplicialComplexes).Version < "1.2" then error "This package requires the SimplicialComplexes package Version 1.2 or higher."


--------------------------------------------------------------------
-- Code originally located in 'SimplicialComplexes'

Face = new Type of MutableHashTable

vertices Face := List => F -> F.vertices
dim Face := ZZ => F -> -1 + # vertices F
ring Face := Ring => F -> F.ring

net Face := f -> (
    v := vertices f;
    if #v === 0 then return net({});
    horizontalJoin apply(v,j->net(j)|net(" "))
    )
Face#{Standard,AfterPrint} = m -> (
  n := # vertices m;
  if n === 0 then vstr := "empty face";
  if n === 1 then vstr = "face with " | n | " vertex";
  if n > 1 then vstr = "face with " | n | " vertices";
  << endl;
  << concatenate(interpreterDepth:"o") << lineNumber << " : "
  << vstr|" in "|net(ring m)
  << endl;
  )

face = method()
face List := Face => L -> new Face from {
    symbol vertices => L, 
    symbol ring=> ring L#0
    }
face(List, PolynomialRing) := Face => (L,R) -> new Face from {
    symbol vertices => L, 
    symbol ring => R
    }
face RingElement := m -> face(support m, ring m)


Face == Face := Boolean => (F, G) -> (
    # vertices F === # vertices G and set vertices F === set vertices G
    )

isSubface = method()
isSubface(Face, Face) := Boolean => (F, G) -> (
    isSubset(set vertices F, set vertices G)
    )

isFaceOf = method()
isFaceOf(Face, SimplicialComplex) := Boolean => (F, C) -> (
    fc := facets C / face;    
    #(select(1,fc, G -> isSubface(F,G)))>0
    )

substitute(Face, PolynomialRing) := (F, R) -> (
    v := vertices F;
    face(apply(v, j -> sub(j, R)), R)
    )



------------------------------------------------------------------------
-- Buchsbaum-Eisenbud resolution of the ideal of submaximal Pfaffians of a 
-- skew symmetric matrix, keeping the syzygy matrix skew-symmetric

resBE=method()
resBE Matrix := A -> (
        R:=ring A;
        p:=gens pfaffians(-1+rank source A,A);
        n:=rank source p;
        g:=matrix {apply(n,j-> (-1)^(j)*p_(n-j-1)_0)};
        chainComplex {g,A,transpose g,map(R^1,R^0,0)})



-------------------------------------------------------------------------
-- Boundary complex of a cyclic polytope


-- for more details on how to create the boundary complex
-- of a cyclic polytope see
-- J. Boehm, S. Papadakis: On the structure of Stanley-Reisner rings associated to cyclic polytopes}, http://arxiv.org/abs/0912.2152, to appear in Osaka J. Math.


-- test whether the list of variables X is contiguous
-- used by contiguousSubsets
isContiguous=method()
isContiguous List := X -> (
        if X=={} then return false ;
        X1:=sort X;
        p2:=X1#-1;
        p1:=X1#0;
        abs(index p2 - index p1)==#X1-1)

TEST ///
debug KustinMiller
R=QQ[x_1..x_9]
assert(isContiguous {x_2,x_3,x_6,x_4,x_5,x_8,x_7})
assert(not isContiguous {x_1,x_2,x_5,x_3})
///

-- compute the contiguous subsets
-- used by maximalContiguousSubsets
contiguousSubsets=method()
contiguousSubsets List := W -> 
        select(subsets(W),isContiguous) 

TEST ///
debug KustinMiller
R=QQ[x_1..x_6]
assert(contiguousSubsets {x_4,x_5} == {{x_4}, {x_5}, {x_4, x_5}})
assert(contiguousSubsets {x_1,x_2,x_3} == {{x_1}, {x_2}, {x_1, x_2}, {x_3}, {x_2, x_3}, {x_1, x_2, x_3}})
assert(contiguousSubsets {x_1,x_2,x_4} == {{x_1}, {x_2}, {x_1, x_2}, {x_4}})
///

-- given a list L,  maximalElements L  returns a list containing the maximal
-- elements of L with respect to inclusion
-- used by maximalContiguousSubsets
maximalElements=method()
maximalElements List := L -> (
        M:=L;
        for j from 0 to #L-1 do (
          for jj from j+1 to #L-1 do (
               if isSubset(L#j,L#jj) then (M=remove(L,j);break);
               if isSubset(L#jj,L#j) then (M=remove(L,jj);break);
          );
        );
        if #M==#L then return L;
        maximalElements M)

TEST ///
debug KustinMiller
R=QQ[x_1..x_10]
assert(maximalElements {{x_1,x_2,x_3},{x_1,x_2},{x_3,x_4},{x_2,x_3,x_4,x_7,x_8}} == {{x_1, x_2, x_3}, {x_2, x_3, x_4, x_7, x_8}})
assert(maximalElements {{x_1,x_2,x_3}, {x_2,x_2,x_3}, {x_1,x_2},{x_3,x_9},{x_6}} == {{x_1, x_2, x_3}, {x_3, x_9}, {x_6}})
assert(maximalElements {{x_2,x_1,x_3}, {x_1,x_2,x_3}, {x_1,x_2},{x_3,x_9},{x_6}} == {{x_1, x_2, x_3}, {x_3, x_9}, {x_6}})
///



-- compute the maximal contiguous subsets of a list of variables
-- used by oddContiguousNonEndsets
maximalContiguousSubsets=method()
maximalContiguousSubsets List := 
        maximalElements @@ contiguousSubsets 

TEST ///
debug KustinMiller
R=QQ[x_1..x_10]
assert(maximalContiguousSubsets {x_1,x_2,x_4} == {{x_1, x_2}, {x_4}})
assert(maximalContiguousSubsets {x_1,x_2,x_4,x_5,x_7,x_8,x_9} == {{x_1, x_2}, {x_4, x_5}, {x_7, x_8, x_9}})
///

-- test whether a list of variables is an endset
-- used by removeEndsets
isEndset=method()
isEndset List := L ->(
        v:=gens ring first L;
        member(v#0,L) or member(v#-1,L))

TEST ///
debug KustinMiller
R=QQ[x_1..x_10]
assert(isEndset {x_1,x_2})
assert(isEndset {x_1,x_3})
assert(not isEndset {x_2,x_3})
///

-- remove the endsets from a list of lists of variables
-- used by oddContiguousNonEndsets
removeEndsets=method()
removeEndsets List := L ->
        select(L,j->not isEndset j )


TEST ///
debug KustinMiller
R=QQ[x_1..x_10]
assert(removeEndsets {{x_1,x_2},{x_3,x_4}} == {{x_3,x_4}})
assert(removeEndsets {{x_1,x_3},{x_7,x_8}} == {{x_7,x_8}})
assert(removeEndsets maximalContiguousSubsets {x_1,x_2,x_4} == {{x_4}})
assert(removeEndsets maximalContiguousSubsets {x_1,x_2,x_4,x_5,x_7,x_8} == {{x_4, x_5}, {x_7, x_8}})
///

-- compute the odd contiguous non-endsets contained in a list of variables
-- used by isFaceOfCyclicPolytope
oddContiguousNonEndsets=method()
oddContiguousNonEndsets List := L ->(
        L1:=removeEndsets maximalContiguousSubsets L;
        select(L1,j->odd(#j)))

TEST ///
debug KustinMiller
R=QQ[x_1..x_10]
assert(removeEndsets maximalContiguousSubsets {x_1,x_2,x_4,x_5,x_7,x_8,x_9} == {{x_4, x_5}, {x_7, x_8, x_9}})
assert(oddContiguousNonEndsets {x_1,x_2,x_4,x_5,x_7,x_8,x_9} == {{x_7, x_8, x_9}})
///

-- tests whether W is a face of a cyclic polytope of dimension d
-- used by delta
isFaceOfCyclicPolytope=method()
isFaceOfCyclicPolytope(List,ZZ):=(W,d)->
        W=={} or #oddContiguousNonEndsets(W)<=d-#W

TEST ///
debug KustinMiller
R=QQ[x_1..x_10]
assert(isFaceOfCyclicPolytope({x_2,x_3},3))
assert(isFaceOfCyclicPolytope({x_3,x_4},3))
assert(not isFaceOfCyclicPolytope({x_4,x_9},3))
///

-- boundary complex of a cyclic polytope
-- of dimension d on the vertices corresponding to the variables of R
delta=method()
delta(ZZ,PolynomialRing) := (d, R) -> (
    L := apply(select(subsets(gens R,d), j -> isFaceOfCyclicPolytope(j,d)),face);
    simplicialComplex apply(L, j -> product vertices j)
    )


-----------------------------------------------------------------------------
-- Constructing the Kustin-Miller complex

kustinMillerComplex=method(Options=>{Verbose=>0})

kustinMillerComplex(Ideal,Ideal,PolynomialRing):=opt->(I,J,T0)->(
        if ring I =!= ring J then error "expected the rings of the first and second argument to be the same";
        if not isSubset(I,J) then error "expected first argument to be an ideal contained in the second";
        if codim(I) != -1+codim(J) then error "expected an unprojection locus of codimension 1";
        kustinMillerComplex(res I,res J,T0,opt))


kustinMillerComplex(ChainComplex,ChainComplex,PolynomialRing):=opt->(cI0,cJ0,T0)->(
             if opt.Verbose>1 then (
               <<"------------------------------------------------------------------------------------------------------------------------"<<endl;
               <<"res(I):"<<endl;
               for j from 1 to length(cI0) do (
                   <<"a_"<<j<<" = "<<cI0.dd_j<<" : "<<degrees source cI0.dd_j <<" -> "<<degrees target cI0.dd_j<<endl;
               );
               <<endl<<"------------------------------------------------------------------------------------------------------------------------"<<endl;
               <<"res(J):"<<endl;
               for j from 1 to length(cJ0) do (
                   <<"b_"<<j<<" = "<<cJ0.dd_j<<" : "<<degrees source cJ0.dd_j <<" -> "<<degrees target cJ0.dd_j<<endl;
               );
               <<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        I:=ideal cI0.dd_1;
        J:=ideal cJ0.dd_1;
        phi:=unprojectionHomomorphism(I,J);
        R:=ring I;
        if R =!= ring J then error("expected the rings of the first and second argument to be the same");
        K:=coefficientRing R;
        degT:=degree phi;
        S:=K(monoid[toSequence(prepend(T0_0,gens R)),Degrees=>prepend(degT,degrees R)]);
        cI:=substitute(cI0,S);
        cJ:=substitute(cJ0,S);
        g:=length cJ ;
        dualcI:=(dual cI)[-codim I];
        dualcJ:=(dual cJ)[-codim I];
        degshift:=degree phi;
        if degshift#0<=0 then error "Unprojection variable must have positive degree. (Recall that, if I is the first argument and J is the second, then the degree of the unprojection variable is k_1 - k_2, where R/I(k_1) and R/J(k_2) are the canonical modules of R/I and R/J respectively.)";
             if opt.Verbose>0 then (
                gJ:=gens source phi;
                <<"phi: "<<(entries gJ)#0<<" -> "<<(entries phi)#0<<endl;
                <<"degree phi = "<<degshift<<endl;
                <<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        gJs:=sub(gens source phi,S);
        IJmap:=sub(matrix entries phi,S)*((gens ideal (dualcJ.dd_0))//gJs);
        alphaDual:=extend(dualcI,dualcJ, map(dualcI#0,dualcJ#0,IJmap));
        w:=(alphaDual_(length cI))_0_0;
        alpha:=toList apply(g-1,j->sub(w^(-1),S)*((transpose alphaDual_(g-2-j))**S^{-degshift}));
             if opt.Verbose>1 then (
                <<endl;
                for j from 1 to #alpha do (
                  <<"alpha_"<<j<<" = "<<alpha_(j-1)<<endl;
                );
                <<endl<<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        cJ1:=cJ[1];
        betaMap:=map((cI#0,(cJ1#0)**S^{-degshift},-sub(matrix entries phi,S)*((gens ideal (cJ1.dd_0))//gJs)));
        beta1:=extend(cI,cJ1**S^{-degshift},betaMap);
        beta:=toList apply(g, j-> (-1)^(j + codim I)*beta1_j);
             if opt.Verbose>1 then (
                for j from 1 to #beta-1 do (
                   <<"beta_"<<j<<" = "<<beta_(j-1)<<endl;
                );
                <<endl<<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        u:=(beta1_(length cI))_0_0;
             if opt.Verbose>1 then (
                <<"u = "<<u<<endl;
                <<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        -- construct the homotopy h
        h:={0_S};
        for j from 1 to g-1 do (
          tC1:= chainComplex { id_(S^(rank (cI#j)))};
          tC2:= chainComplex { cI.dd_j };
          hi:= (extend ( tC2, tC1, map (tC2#0, tC1#0,  beta#(j-1)*alpha#(j-1) - h#(j-1)*cI.dd_j  )));
          h=append(h,map(cI#j,cI#j**S^{-degshift},hi_1));
        );
             if opt.Verbose>1 then (
                for j from 1 to #h-1 do (
                   <<"h_"<<j<<" = "<<h_(j-1)<<endl;
                );
                <<endl<<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        cJ=cJ**S^{-degshift};
        -- form the differentials of the Kustin-Miller complex
        L:={};
        for j from 1 to g do (
          if g==2 then (
           if j==1 then (
             inL:={cI.dd_1,beta#0,cJ.dd_1,S_0};
           );
           if j==2 then (
             inL={alpha#(j-2),cJ.dd_j,cI.dd_(j-1),u,S_0};
           );
          );
          if g>=3 then (
           if j==1 then (
             inL={cI.dd_1,beta#0,cJ.dd_1,S_0};
           );
           if j==2 then (
             inL={cI.dd_2,beta#1,h#1,cJ.dd_2,alpha#0,S_0};
           );
           if j>=3 and j<=g-2 then (
             inL={cI.dd_j,beta#(j-1),h#(j-1),cJ.dd_j,alpha#(j-2),cI.dd_(j-1),S_0};
           );
           if j>=3 and j==g-1 then (
             inL={beta#(j-1),h#(j-1),cJ.dd_j,alpha#(j-2),cI.dd_(j-1),S_0};
           );
           if j>=3 and j==g then (
             inL={alpha#(j-2),cJ.dd_j,cI.dd_(j-1),u,S_0};
           );
          );
          L=append(L,differentials(inL,j,g,degshift));
             if opt.Verbose>0 then (
                 <<"f_"<<j<<" = "<<L_(j-1)<<" : "<<degrees source L_(j-1)<<" -> "<<degrees target L_(j-1)<<endl;
                 <<endl<<"------------------------------------------------------------------------------------------------------------------------"<<endl;
             );
        );
        chainComplex L)

--kustinMillerComplex(I,J,QQ[t])


TEST ///
R = QQ[x_1..x_4,z_1..z_4, T]
I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
J = ideal (z_1..z_4)
cc=kustinMillerComplex(I,J,QQ[t]);
assert(isExactRes cc)
///


--kustinMillerComplex(L#0,L#1,QQ[t])



-- some auxiliary procedures


-- make the differentials of the Kustin-Miller complex
-- this command (and its documentation, which you can find below) is not a user level function and hence does not get exported

differentials=method()
differentials(List,ZZ,ZZ,List):=(L,i,g,degshift)->(
        if i<0 or i>g then error("second argument should be between 0 and "|g);
        T:=last L;
        mT:=matrix {{T}};
        R:=ring first L;
        -- codim 2 case
        if g==2 then (
         -- first differential
         if i==1 then (
           b1:=L#0;beta1:=L#1;a1:=L#2;
           return(beta1+ mT**a1)
         );
         -- second differential
         if i==2 then (
          alphaim1:=L#0;ai:=L#1;bim1:=L#2;u:=L#3;
          return(-alphaim1+((-1)^i*u^(-1)*mT**ai));
         );
        );
        -- codim 3 case
        if g==3 then (
         -- first differential
         if i==1 then (
           b1=L#0;beta1=L#1;a1=L#2;
           return matrix {{b1, beta1+ mT**a1}};
         );
         -- second differential
         if i==2 then (
           b2:=L#0;beta2:=L#1;h1:=L#2;a2:=L#3;alpha1:=L#4;
           return matrix {{beta2, h1+ mT**id_(R^(rank target b2))}, 
                          {  -a2, -alpha1                        }};
         );
         -- third differential
         if i==3 then (
          alphaim1=L#0;ai=L#1;bim1=L#2;u=L#3;
          sbim1:=bim1**R^{-degshift};
          return matrix {{-alphaim1+(-1)^i*u^(-1)*mT**ai},
                         {sbim1                         }};
         );
        );
        -- general case
        -- first differential
        if i==1 then (
           b1=L#0;beta1=L#1;a1=L#2;
           return matrix {{b1, beta1+mT**a1}};
        );
        -- second differential
        if i==2 then (
          b2=L#0;beta2=L#1;h1=L#2;a2=L#3;alpha1=L#4;
          return matrix {{b2,     beta2, h1+mT**id_(R^(rank target b2))},
                         { 0,       -a2, -alpha1                       }};
        );
        -- the general differential
        if i>=3 and i<=g-2 then (
          bi:=L#0;betai:=L#1;him1:=L#2;ai=L#3;alphaim1=L#4;bim1=L#5;
          sbim1=bim1**R^{-degshift};
          return matrix {{bi,    betai,  him1+(-1)^i*mT**id_(R^(rank target bi))},
                         { 0,      -ai,  -alphaim1                              },
                         { 0,        0,  sbim1                                  }};
        );
        -- second last differential
        if i>=3 and i==g-1 then (
          betai=L#0;him1=L#1;ai=L#2;alphaim1=L#3;bim1=L#4;
          sbim1=bim1**R^{-degshift};
          return matrix {{betai,  him1+(-1)^i*T*id_(R^(rank source bim1))},
                         {  -ai,  -alphaim1                              },
                         {    0,  sbim1                                  }};
        );
        -- case i>=3 and i==g
        -- last differential
        alphaim1=L#0;ai=L#1;bim1=L#2;u=L#3;
        sbim1=bim1**R^{-degshift};
        matrix {{-alphaim1+(-1)^i*u^(-1)*mT**ai},
                {sbim1                         }} )



------------------------------------------------------------------------------
-- Compute the unprojection homomorphism phi

unprojectionHomomorphism=method()
unprojectionHomomorphism(Ideal,Ideal):=(I,J)->(
        R:=ring I;
        if R =!= ring J then error "expected both ideals to be contained in the same ring";
        if not isSubset(I,J) then error "expected first ideal to be contained in the second";
        if codim(I) != -1 + codim(J) then error "the unprojection locus does not have codimension 1";
        if not isGorenstein(I) or not isGorenstein(J) then error("input ideals should be projectively Gorenstein");
        Q:=R/I;
        M:=Hom(ideal mingens sub(J,Q),Q^1);
        -- give some feedback on wrong input
        if rank source gens M != 2 and rank source gens M != 1 then (
            for j from 0 to -1+rank source gens M do (
               phi:=homomorphism M_{j};
                gJ:=gens source phi;
                <<"phi: "<<(first entries gJ)<<" -> "<<(first entries phi)<<endl;
             );
             error "internal error: the computed module of homomorphisms does not have the number of generators predicted by theory";
        );
        if rank source gens M == 1 then (
          return homomorphism M_{0};
        ) else (
          f1:=homomorphism M_{0};
          f2:=homomorphism M_{1};
          if J==I+sub(ideal (entries f1)#0,R) then (
           return f2
          ) else (
           return f1
          )
        ))


-- test whether I is projectively Gorenstein
isGorenstein=method()
isGorenstein(Ideal):= I->(
          R:= ring I;
          (pdim(R^1 / I) == codim I) and (rank gens Ext^(codim I)(coker gens I, R^1)==1)
        )


TEST ///
debug KustinMiller
R = QQ[x_1..x_4,z_1..z_4, T]
I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
assert(isGorenstein I )
R = QQ[x_1..x_6]
assert(not isGorenstein minors (2, matrix {{x_1..x_3},{x_4..x_6}}))
R = QQ[x_1..x_3]
assert(not isGorenstein ideal (x_1*x_2, x_1*x_3))
///

---------------------------------------------------------------------
-- some useful stuff for chain complexes

-- check whether a chain complex is a resolution
-- that is it is exact everywhere except at the
-- first non-zero module
-- note that this is not the same as isExact in the
-- chain complex extras

-- we first find the first non-zero module
firstNonzero=method()
firstNonzero(ChainComplex):= cc -> (
       for i from min cc to max cc do if cc_i!=0 then return i;
       infinity)

isExactRes=method()
isExactRes(ChainComplex):= cc ->(
        for j from firstNonzero(cc)+1 to max(cc)+1 do (
            if cc.dd_(j)*cc.dd_(j+1) !=0 then return false;
            if (HH_j cc) !=0 then return false;
        );
        true)

-- with this method also the substituted complexes
-- recognize, when printed, if a name is assigned to
-- the ring of the complex
substitute(ChainComplex,Ring):=(cc,S)->(
        dual cc;
        cn:= new ChainComplex;
        cn.ring = S;
        for i from min(cc) to max(cc) do cn#i = S^(degrees (cc#i));
        for i from min(cc)+1 to max(cc) do cn.dd_i = sub(cc.dd_i,S);
        cn)

--------------------------------------------------------------------------
-- Stellar subdivision code
stellarSubdivisionSimplex=method()
stellarSubdivisionSimplex(Face,Face,PolynomialRing,PolynomialRing):=(D,s,n,R)->(
        if isSubface(s,D) then
           facets(subdivideFace (D,s,n,R)) / face
        else
           {substitute(D,R)})

-- stellar subdivision of a simplicial complex with respect to the face
-- introducing a new variable
-- the 'stellarSubdivision' method is defined in 'Polyhedra'
stellarSubdivision(SimplicialComplex,Face,PolynomialRing):= (D,s0,n)  ->  (
        R1:=ring D;
        s:=substitute(s0,R1);
        if not isFaceOf(s,D) then (
           error "second argument is not a face of the first argument";
        );
        fc:=facets(D) / face;
        R1=ring D;
        K:=coefficientRing R1;
        v:=join(gens R1,gens n);
        R:=K(monoid[v]);
        L:=join toSequence for i to #fc-1 list stellarSubdivisionSimplex (fc#i,s,n,R);
        simplicialComplex apply(L, j -> product vertices j)
	)

joinFaces=method()
joinFaces(Face,Face):=(F,G)->(
        v1:=vertices F;
        v2:=vertices G;
        face(v1|v2))


listMinus=method()
listMinus(List,List):=(L1,L2)->(
        for i in L1 list 
           if member(i,L2) then continue else i)


coFace=method()
coFace(Face,Face):=(F,G)->(
        v1:=vertices F;
        v2:=vertices G;
        R:=ring G;
        face(listMinus(v2,v1),R))



subdivideFace=method()
subdivideFace(Face,Face,PolynomialRing,PolynomialRing):= (D,s,n,R) -> (
       comp := substitute(coFace(s,D),R);
       nface:= substitute(face {n_0},R);
       nc:=joinFaces(comp,nface);
       vs:=vertices s;
       L := for i to #vs-1 list joinFaces(nc,substitute(coFace(face {vs#i},s),R));
       simplicialComplex apply(L, j -> product vertices j)
       )



TEST ///
R=QQ[x_1..x_6]
I=monomialIdeal(product(gens R))
D=simplicialComplex I
Dsigma=stellarSubdivision(D,face {x_1,x_2,x_3},QQ[t])
S=ring Dsigma
assert(facets Dsigma == {x_2*x_3*x_5*x_6*t, x_1*x_3*x_5*x_6*t, x_1*x_2*x_5*x_6*t,      x_2*x_3*x_4*x_6*t, x_1*x_3*x_4*x_6*t, x_1*x_2*x_4*x_6*t,      x_2*x_3*x_4*x_5*t, x_1*x_3*x_4*x_5*t, x_1*x_2*x_4*x_5*t,      x_2*x_3*x_4*x_5*x_6, x_1*x_3*x_4*x_5*x_6, x_1*x_2*x_4*x_5*x_6})
///



------------------------------------------------------------------------------------------------------------------
-- documentation

beginDocumentation()

doc ///
  Key
    KustinMiller
  Headline
    Unprojection and the Kustin-Miller complex construction
  Description
    Text
      This package implements the construction of the Kustin-Miller complex [1]. This is the
      fundamental construction of resolutions in unprojection theory [2]. For details on the
      computation of the Kustin-Miller complex see [3].

      Gorenstein rings with an embedding codimension at most 2 are known to be
      complete intersections, and those with embedding codimension 3 are described
      by the theorem of Buchsbaum and Eisenbud as Pfaffians of a skew-symmetric matrix; 
      general structure theorems in higher codimension are lacking and the main goal of unprojection theory 
      is to provide a substitute for a structure theorem.

      Unprojection theory has been applied in various cases to construct new varieties, for example, in [4] in the case of Campedelli surfaces and [5] in the case of Calabi-Yau varieties.
      
      We provide a general command @TO kustinMillerComplex@ for the Kustin-Miller complex construction and demonstrate it on several examples connecting unprojection theory
      and combinatorics such as stellar subdivisions of simplicial complexes [6],
      minimal resolutions of Stanley-Reisner rings of boundary complexes $\Delta(d,m)$
      of cyclic polytopes of dimension d on m vertices [7], and the classical 
      (non-monomial) Tom example of unprojection [2].
      
      This package requires the package
      @HREF{"http://www.math.uni-sb.de/ag/schreyer/jb/Macaulay2/SimplicialComplexes/SimplicialComplexes.m2","SimplicialComplexes.m2"}@
      version 1.2 or higher, so install this first.

      {\bf References:}

      For the Kustin-Miller complex see:

      [1] {\it A. Kustin and M. Miller, Constructing big Gorenstein ideals from small ones, J. Algebra 85 (1983), 303-322}.

      [2] {\it S. Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268}, @HREF"http://arxiv.org/abs/math/0111195"@

      [3] {\it J. Boehm, S. Papadakis: Implementing the Kustin-Miller complex construction}, @HREF"http://arxiv.org/abs/1103.2314"@

      For constructing new varieties see for example:

      [4] {\it J. Neves and S. Papadakis, A construction of numerical Campedelli surfaces with ZZ/6 torsion, Trans. Amer. Math. Soc. 361 (2009), 4999-5021}.

      [5] {\it J. Neves and S. Papadakis, Parallel Kustin-Miller unprojection with an application to Calabi-Yau geometry, preprint, 2009, 23 pp}, @HREF"http://arxiv.org/abs/0903.1335"@

      For the stellar subdivision case see:

      [6] {\it J. Boehm, S. Papadakis: Stellar subdivisions and Stanley-Reisner rings of Gorenstein complexes}, @HREF"http://arxiv.org/abs/0912.2151"@

      For the case of cyclic polytopes see:

      [7] {\it J. Boehm, S. Papadakis: On the structure of Stanley-Reisner rings associated to cyclic polytopes}, @HREF"http://arxiv.org/abs/0912.2152"@, to appear in Osaka J. Math.


      {\bf Examples:}

      @TO "Cyclic Polytopes"@  -- Minimal resolutions of Stanley-Reisner rings of boundary complexes of cyclic polytopes

      @TO "Stellar Subdivisions"@  -- Stellar subdivisions and unprojection

      @TO "Tom"@  -- The Tom example of unprojection

      @TO "Jerry"@  -- The Jerry example of unprojection

      
      {\bf Key user functions:}

      {\it The central function of the package is:}

      @TO kustinMillerComplex@  -- The Kustin-Miller complex construction


      {\it Also important is the function to represent the unprojection data as a homomorphism:}

      @TO unprojectionHomomorphism@ -- Compute the homomorphism associated to an unprojection pair


      {\it Functions used in the examples to compare with the combinatorics:}

      @TO delta@  --  The boundary complex of a cyclic polytope

      @TO stellarSubdivision@  -- Compute the stellar subdivision of a simplicial complex


///



doc ///
  Key
    kustinMillerComplex
    (kustinMillerComplex,Ideal,Ideal,PolynomialRing)
    (kustinMillerComplex,ChainComplex,ChainComplex,PolynomialRing)
  Headline
    Compute Kustin-Miller resolution of the unprojection of I in J
  Usage
    kustinMillerComplex(I,J,W)
    kustinMillerComplex(cI,cJ,W)
  Inputs
    J:Ideal
        in a positively graded polynomial ring R
    I:Ideal
        contained in J
    cI:ChainComplex
        resolution of I
    cJ:ChainComplex
        resolution of J
    W:PolynomialRing
        over the the same @TO coefficientRing@ as R
        with one variable T.
  Outputs
    :ChainComplex
  Description
   Text
    Compute Kustin-Miller resolution of the unprojection of I in J (or
    equivalently of the image J' of J in R/I) with unprojection variable T.

    We have the following setup:

    Assume R is a @TO PolynomialRing@ over a field, the degrees of all
    variables positive and $I \subset J \subset R$ two homogeneous ideals of R
    such that R/I and R/J are Gorenstein and dim(R/J)=dim(R/I)-1.

    Let R/I(k_1) and R/J(k_2) be the canonical modules of R/I and R/J respectively. We require k_1 - k_2, that is, the degree of the unprojection variable, to be positive.

    For a description of this resolution and how it is computed see
    
    J. Boehm, S. Papadakis: Implementing the Kustin-Miller complex construction, @HREF"http://arxiv.org/abs/1103.2314"@


    It is also possible to specify minimal resolutions of I and J.

    The function @TO kustinMillerComplex@ returns a chain complex over a new polynomial ring S
    with the same @TO coefficientRing@ as R and the variables of R and W, where
    degree(T) = @TO degree@ @TO unprojectionHomomorphism@(I,J) = k_1-k_2.
    
    To avoid printing the variables of this ring when printing the chain complex
    just give a name to the ring (e.g., do S = @TO ring@ cc  to call it S).

    We illustrate the Kustin-Miller complex construction at the example described in Section 5.5 of 

    Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268, @HREF"http://arxiv.org/abs/math/0111195"@


   Example
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     betti res I
     J = ideal (z_1..z_4)
     betti res J
     cc=kustinMillerComplex(I,J,QQ[T]);
     S=ring cc
     cc
     betti cc
     isExactRes cc
     print cc.dd_1
     print cc.dd_2
     print cc.dd_3
     print cc.dd_4
  SeeAlso
    unprojectionHomomorphism
///



doc ///
  Key
    unprojectionHomomorphism
    (unprojectionHomomorphism,Ideal,Ideal)
  Headline
    Compute the homomorphism associated to an unprojection pair
  Usage
    unprojectionHomomorphism(I,J)
  Inputs
    J:Ideal
        with R/J Gorenstein
    I:Ideal
        contained in J with R/I Gorenstein and dim(R/J)=dim(R/I)-1.
  Outputs
    f:Matrix
  Description
   Text
    Compute the deformation associated to the unprojection of $I \subset J$ (or
    equivalently of $J'\subset R/I$ where $R$ = @TO ring@ $I$ and $J'=$@TO sub@$(J,R/I)$), 
    i.e., a homomorphism 
    
    $\phi : J' \to R/I$
    
    such that the unprojected ideal $U\subset R[T]$ is the inverse image of

    $U' = (T*u - \phi(u)  |  u \in J' ) \subset (R/I)[T]$

    under the natural map $R[T]\to(R/I)[T]$.

    The result is represented by a matrix $f$ with @TO source@ $f$ = J'
    and @TO target@ $f$ = (R/I)^1.

   Example
     R = QQ[x_1..x_4,z_1..z_4, T]
     I = ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     J = ideal (z_1..z_4)
     phi = unprojectionHomomorphism(I,J)
     S = ring target phi;
     I == ideal S
     source phi
     target phi
  SeeAlso
    kustinMillerComplex
///


doc ///
  Key
    delta
    (delta,ZZ,PolynomialRing)
  Headline
    Boundary complex of cyclic polytope.
  Usage
    delta(d,R)
  Inputs
    d:ZZ
       positive
    R:PolynomialRing
  Outputs
    :SimplicialComplex
  Description
   Text
      Boundary complex of a cyclic polytope of dimension d on the variables of R as vertices, i.e., $\Delta(d,m)$ if m is the number of variables of R.

   Example
     K=QQ;
     R=K[x_0..x_6];
     C=delta(4,R)
     fVector C
     I=ideal C
     betti res I
///



doc ///
  Key
    [kustinMillerComplex,Verbose]
  Headline
    Option to print intermediate data
  Description
   Text
    @TO Option@ to print the intermediate results.

    It takes @TO ZZ@ values (standard is 0), increasing the amount of output with the value.
///


-*
doc ///
  Key
    differentials
    (differentials,List,ZZ,ZZ,List)
  Headline
    Generate the differentials of the Kustin-Miller resolution
  Usage
    differentials(L,j,g,s)
  Inputs
    L:List
       with entries of type @TO Matrix@ and the last entry of type @TO RingElement@,
       all of them defined over the same ring.
    g:ZZ
       positive
    j:ZZ
       from 1 to g
    s:ZZ
  Outputs
    :Matrix
  Description
   Text
    Generate the j-th differential of a Kustin-Miller resolution
    of length g. So, e.g., for j=1 we obtain the relations of the 
    ring resolved and for j=2 the first syzygies of those.

    We use the notation of

    J. Boehm, S. Papadakis: Implementing the Kustin-Miller complex construction, @HREF"http://arxiv.org/abs/1103.2314"@

    For any j the @TO last@ entry of L should be the variable T.

    For j=1 we assume L = \{ b_1, beta_1, a_1, T \}.

    For j=2 we assume L = \{ b_2, beta_2, h_1, a_2, alpha_1, T \}.

    For j=3,...,g-1 we assume L = \{ b_j, beta_j, h_{j-1}, a_j, alpha_{j-1}, b_{j-1}, T \}.

    For j=g-1 we assume L = \{ beta_{g-1}, h_{g-1}, a_{g-1}, alpha_{g-2}, b_{g-2}, T \}.

    For j=g we assume L = \{ alpha_{g-1}, a_g, b_{g-1}, u, T \}.
    
    Finally s equals k_1-k_2.

  SeeAlso
    kustinMillerComplex
  Caveat
    This is not a user level function.
///
*-


doc ///
  Key
    resBE
    (resBE,Matrix)
  Headline
    Buchsbaum-Eisenbud resolution
  Usage
    resBE(A)
  Inputs
    A:Matrix
        skew-symmetric
  Outputs
    :ChainComplex
  Description
   Text
      Returns the Buchsbaum-Eisenbud resolution of the ideal of submaximal @TO pfaffians@ 
      of a skew-symmetric matrix A. The syzygy matrix will be A.
      
   Example
      R=QQ[x_1..x_4,z_1..z_4];
      A=matrix {{0,x_1,x_2,x_3,x_4},{-x_1,0,0,z_1,z_2},{-x_2,0,0,z_3,z_4},{-x_3,-z_1,-z_3,0,0},{-x_4,-z_2,-z_4,0,0}}
      resBE A
  SeeAlso
     res
///



doc ///
  Key
    isExactRes
    (isExactRes,ChainComplex)
  Headline
    Test whether a chain complex is an exact resolution.
  Usage
    isExactRes(cc)
  Inputs
    cc:ChainComplex
  Outputs
    :Boolean
  Description
   Text
    Test whether a chain complex is an exact resolution, that is,
    it is exact everywhere except at the first non-zero module.

    We consider cc as a doubly infinite complex extending it by adding
    trivial modules and homomorphisms.

   Example
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cc= res I
     isExactRes cc
     isExactRes(cc[1])
     isExactRes(cc[-1])
  SeeAlso
    res
///


doc ///
  Key
    (substitute,ChainComplex,Ring)
  Headline
    Substitute a chain complex to a new ring.
  Usage
    substitute(cc,R)
  Inputs
    cc:ChainComplex
    R:Ring
  Outputs
    :ChainComplex
  Description
   Text
    Substitute a chain complex cc to a new ring R.

   Example
     R=QQ[x_1..x_4,z_1];
     cc=res ideal(x_4*x_3, -x_1*x_2+x_4*z_1);
     cs=substitute(cc,QQ[x_1..x_4])
     cs.dd_1
  SeeAlso
    substitute
///


doc ///
  Key
    stellarSubdivision
    (stellarSubdivision,SimplicialComplex,Face,PolynomialRing)
  Headline
    Compute the stellar subdivision of a simplicial complex.
  Usage
    stellarSubdivision(D,F,S)
  Inputs
    D:SimplicialComplex 
        a simplicial complex on the variables of the polynomial ring R.
    F:Face
        a face of D
    S:PolynomialRing
        a polynomial ring in one variable corresponding to the new vertex
  Outputs
    :SimplicialComplex
        the stellar subdivision of D with respect to F and S
  Description
   Text
        Computes the stellar subdivision of a simplicial complex D by subdividing the face F with a new vertex
        corresponding to the variable of S.
        The result is a complex on the variables of R \otimes S. It is a subcomplex of the simplex on the variables of R \otimes S.
        
   Example
     R=QQ[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     betti res I
     D=simplicialComplex I 
     fc=facets(D) / face
     S=QQ[x_5]
     D5=stellarSubdivision(D,fc#0,S)
     I5=ideal D5
     betti res I5
   Text

   Example
     R=QQ[x_1..x_6]
     I=monomialIdeal product gens R
     D=simplicialComplex I
     S=QQ[x_7]
     Dsigma=stellarSubdivision(D,face {x_1,x_2,x_3},S)
     betti res ideal Dsigma
  SeeAlso
     simplicialComplex
     facets
     ideal
///




-----------------------------------------------------------------
-- Tests

-- test unprojectionHomomorphism
///TEST
     R = QQ[x_1..x_4,z_1..z_4, T];
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3);
     J = ideal (z_1..z_4);
     phi=unprojectionHomomorphism(I,J);
assert(phi==map(coker gens I,image gens J,matrix {{x_1*x_3, x_1*x_4, x_2*x_3, x_2*x_4}}));
///


-- test cyclic polytope command
///TEST
     K=QQ;
     R=K[x_0..x_6];
     C=delta(4,R);
     fVector C;
assert(ideal C==ideal(x_0*x_2*x_4,x_0*x_2*x_5,x_0*x_3*x_5,x_1*x_3*x_5,x_1*x_3*x_6,x_1*x_4*x_6,x_2*x_4*x_6))
///

-- test Buchsbaum-Eisenbud resolution command
///TEST
      R=QQ[x_1..x_4,z_1..z_4];
      A=matrix {{0,x_1,x_2,x_3,x_4},{-x_1,0,0,z_1,z_2},{-x_2,0,0,z_3,z_4},{-x_3,-z_1,-z_3,0,0},{-x_4,-z_2,-z_4,0,0}};
      cc=resBE A;
assert(matrix entries cc.dd_2==A);
assert(pfaffians(4,A)==ideal cc.dd_1);
///


-- test isExactRes
///TEST
     R = QQ[x_1..x_4,z_1..z_4];
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3);
     cc= res I;
assert(isExactRes cc);
C=chainComplex presentation QQ^1;
assert(isExactRes C);
assert(isExactRes(C[1]));
assert(isExactRes(C[-1]));
assert(isExactRes(dual C));
assert(isExactRes(dual C[1]));
assert(isExactRes(dual C[-1] ));
///

-- test stellar subdivision code
TEST ///
     K=QQ;
     R=K[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     D=simplicialComplex I
     S=K[x_5]
     D5=stellarSubdivision(D,face {x_0,x_2},S)
     I5=ideal D5
     use ring I5
     assert(I5==ideal(x_4*x_5,x_3*x_5,x_1*x_5,x_3*x_4,x_0*x_4,x_2*x_3,x_1*x_2,x_0*x_2,x_0*x_1));
///


-- test Kustin-Miller complex command using C47 example
TEST ///
     K=QQ;
     C26=delta(2,K[z,x_2..x_6])
     R=K[z,x_1..x_7]
     J=sub(ideal C26,R)
     c26=res J;
     C47=delta(4,K[x_1..x_7])
     I=sub(ideal C47,R)
     c47=res I;
     cc=kustinMillerComplex(c47,c26,K[x_8]);
assert(rank(cc#1)==16);
assert(rank(cc#2)==30);
assert(isExactRes(cc));
///

-- test whether the result of unprojection is C48
TEST ///
     K=QQ;
     C26=delta(2,K[z,x_2..x_6])
     R=K[z,x_1..x_7]
     J=sub(ideal C26,R)
     c26=res J;
     C47=delta(4,K[x_1..x_7])
     I=sub(ideal C47,R)
     c47=res I;
     cc=kustinMillerComplex(c47,c26,K[x_8]);
     R'=K[x_1..x_8];
     C48=delta(4,R');
     I48=ideal C48;
assert(I48==sub(ideal cc.dd_1,R'))
///


-- test Kustin-Miller complex command using Tom example
TEST ///
     R = QQ[x_1..x_4,z_1..z_4]
     I =  ideal(z_2*z_3-z_1*z_4,x_4*z_3-x_3*z_4,x_2*z_2-x_1*z_4,x_4*z_1-x_3*z_2,x_2*z_1-x_1*z_3)
     cI=res I
     betti cI
     J = ideal (z_1..z_4)
     cJ=res J
     betti cJ
     cc=kustinMillerComplex(cI,cJ,QQ[T]);
assert(rank(cc#1)==9);
assert(rank(cc#2)==16);
assert(isExactRes cc);
///


-----------------------------------------------------------------
-- Examples

		  
doc ///
    Key
        Face
    Headline
        The class of faces of simplicial complexes.
    Description
        Text
            The class of faces of simplicial complexes on the variables of a
            polynomial ring.  The faces are @TO MutableHashTable@s F with two
            @TO keys@ F.vertices is a @TO List@ of vertices in the @TO
            PolynomialRing@ F.ring
	
        Example
            R=QQ[x_0..x_4];
            F=face {x_0,x_2}
            vertices F
            I = monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
            D = simplicialComplex I
            fc = faces(1, D)
    SeeAlso
        SimplicialComplex
        faces
        facets
///

doc ///
  Key
    (symbol ==,Face,Face)
  Headline
   Compare two faces.
  Usage
    F==G
  Inputs
    F:Face
    G:Face
  Outputs
    :Boolean
  Description
   Text
        Checks whether F and G are equal.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1}
     G1=face {x_1,x_0}
     G2=face {x_1,x_2}
     F==G1
     F==G2
  SeeAlso
     Face
     face
///


doc ///
  Key
    face
    (face,List)
    (face,List,PolynomialRing)
    (face,RingElement)
  Headline
    Generate a face.
  Usage
    face(L)
    face(L,R)
    face(m)
  Inputs
    L:List
    R:PolynomialRing
    m:RingElement
        a monomial
  Outputs
    :Face
  Description
   Text
        Generates a face out of a list L or a squarefree monomial.
        If L is not empty or a monomial the argument R is not required.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1}
  SeeAlso
     SimplicialComplex
     faces
     facets
///

doc ///
    Key
        (dim, Face)
    Headline
        The dimension of a face.
    Usage
        dim F
    Inputs
        F : Face
    Outputs
        : ZZ
            bigger or equal to -1
    Description
       Text
            Returns the dimension of a @TO Face@, i.e., the number of 
	    @TO vertices@ F minus 1.
       Example
            K = QQ;
            R = K[x_0..x_4];
            I = monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
            D = simplicialComplex I
            fc = faces(D)
            -- apply(-1..1, j->apply(fc#j,dim))
    SeeAlso
         face
         (facets, SimplicialComplex)
         (faces, SimplicialComplex)
///

doc ///
  Key
    (vertices, Face)
  Headline
    The vertices of a face of a simplicial complex.
  Usage
    vertices(F)
  Inputs
    F:Face
  Outputs
    :List
  Description
   Text
        Returns a @TO List@ with the vertices of a @TO Face@ of a simplicial complex.
   Example
     R = QQ[x_0..x_4];
     I = monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     D = simplicialComplex I
     fc = facets(D)
     (faces D)#(1)
     --vertices fc#1
  SeeAlso
     face
     (facets,SimplicialComplex)
     (faces, SimplicialComplex)
///

doc ///
  Key
    isSubface
    (isSubface,Face,Face)
  Headline
    Test whether a face is a subface of another face.
  Usage
    isSubface(F,G)
  Inputs
    F:Face
    G:Face
  Outputs
    :Boolean
  Description
   Text
        Test whether F is a subface of G.

   Example
     K=QQ;
     R=K[x_0..x_4];
     G=face {x_0,x_1,x_2}
     F1=face {x_0,x_2}
     F2=face {x_0,x_3}
     isSubface(F1,G)
     isSubface(F2,G)
///

doc ///
  Key
    (substitute,Face,PolynomialRing)
  Headline
    Substitute a face to a different ring.
  Usage
    substituteFace(F,R)
  Inputs
    F:Face
    R:PolynomialRing
  Outputs
    :Face
  Description
   Text
        Substitute a face to a different ring.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1,x_2}
     S=R**K[y]
     substitute(F,S)
///

doc ///
  Key
    (ring,Face)
  Headline
    Ring of a face.
  Usage
    ring(F)
  Inputs
    F:Face
  Outputs
    :Ring
  Description
   Text
        Ring of a face.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1,x_2}
     ring F
///


doc ///
  Key
    isFaceOf
    (isFaceOf,Face,SimplicialComplex)
  Headline
    Substitute a face to a different ring.
  Usage
    substitute(F,R)
  Inputs
    F:Face
    R:PolynomialRing
  Outputs
    :Face
  Description
   Text
        Substitute a face to a different ring.

   Example
     R = QQ[x_1..x_5];
     C = simplicialComplex monomialIdeal (x_1*x_2,x_3*x_4*x_5)
     F1 = face {x_1,x_2}
     F2 = face {x_1,x_3}
     -- isFaceOf(F1,C)
     -- isFaceOf(F2,C)
///

doc ///
  Key
    (net,Face)
  Headline
    Printing a face.
  Usage
    net(F)
  Inputs
    F:Face
  Outputs
    :Net
  Description
   Text
        Prints a face. The vertices are printed without any brackets and with one space between them. Also prints the polynomial ring which contains the vertices.

   Example
     K=QQ;
     R=K[x_0..x_4];
     face {x_0,x_1}
///

///
  Key
    useFaceClass
    [faces,useFaceClass]
    [facets,useFaceClass]
  Headline
    Option to return faces in the class Face
  Description
   Text
    @TO Boolean@ @TO Option@ to return in the methods @TO faces@ and @TO facets@ a @TO List@ of @TO Face@s instead of a @TO Matrix@.
///


doc ///
  Key
    "Stellar Subdivisions"
  Headline
    The Kustin-Miller complex for stellar subdivisions
  Description
   Text
    We consider a Gorenstein* simplicial complex C and the complex C' obtained by
    stellar subdivision (see @TO stellarSubdivision@) of a face F of C,
    and the corresponding Stanley-Reisner ideals I and I'.

    We construct a resolution of I' from a resolution of I and from a resolution of the
    Stanley-Reisner ideal of the link of F using the Kustin-Miller complex construction 
    implemented in @TO kustinMillerComplex@. Note that this resolution
    is not necessarily minimal (for facets it is).

    For details see

    {\it J. Boehm, S. Papadakis: Stellar subdivisions and Stanley-Reisner rings of Gorenstein complexes}, @HREF"http://arxiv.org/abs/0912.2151"@


    (1) The simplest example:

    Consider the stellar subdivision of the edge \{x_1,x_2\}\  of the triangle with vertices x_1,x_2,x_3.
    The new vertex is x_4 and z_1 is the base of the unprojection deformation.

   Example
    K=QQ;
    R=K[x_1..x_3,z_1];
    I=ideal(x_1*x_2*x_3)
    Ilink=I:ideal(x_1*x_2)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_4]);
    S=ring cc
    cc
    betti cc
    isExactRes cc
    cc.dd_1
    cc.dd_2
   Text

    Obviously the ideal resolved by the Kustin-Miller complex at the special fiber z_1=0
    is the Stanley-Reisner ideal of the stellar subdivision (i.e., of a 4-gon).


    (2) Stellar subdivision of the facet \{x_1,x_2,x_4,x_6\}\  of the simplicial complex associated to the complete intersection (x_1*x_2*x_3, x_4*x_5*x_6).
    The result is a Pfaffian:

   Example
    R=K[x_1..x_6,z_1..z_3];
    I=ideal(x_1*x_2*x_3,x_4*x_5*x_6)
    Ilink=I:ideal(x_1*x_2*x_4*x_6)
    J=Ilink+ideal(z_1*z_2*z_3)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_7]);
    S=ring cc
    cc
    betti cc
    isExactRes cc
    cc.dd_1
    cc.dd_2
    cc.dd_3
   Text

    We compare with the combinatorics, i.e., check that the ideal
    resolved by the Kustin Miller complex at the special fiber is the
    Stanley-Reisner ideal of the stellar subdivision:

   Example
    R=K[x_1..x_6];
    C=simplicialComplex monomialIdeal(x_1*x_2*x_3,x_4*x_5*x_6)
    fVector C
    F=face {x_1,x_2,x_4,x_6}
    R'=K[x_1..x_7];
    C'=substitute(stellarSubdivision(C,F,K[x_7]),R')
    fVector C'
    I'=monomialIdeal(sub(cc.dd_1,R'))
    C'===simplicialComplex I'
   Text

    One observes that in this case the resulting complex is minimal
    This is always true for stellars of facets.


    (3) Stellar subdivision of an edge:

   Example
    R=K[x_1..x_5,z_1];
    I=monomialIdeal(x_1*x_2*x_3,x_4*x_5)
    C=simplicialComplex I
    fVector C
    F=face {x_1,x_2}
    Ilink=I:ideal(product vertices F)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_6]);
    S=ring cc
    cc
    betti cc
    isExactRes cc
    cc.dd_1
    cc.dd_2
    cc.dd_3
   Text

    (4) Starting out with the Pfaffian elliptic curve:

   Example
    R=K[x_1..x_5,z_1];
    I=ideal(x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_1)
    Ilink=I:ideal(x_1*x_3)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_10]);
    betti cc
   Text

    (5) One more example of a stellar subdivision of an edge starting with a codimension 4 complete intersection:
 
   Example
    R=K[x_1..x_9,z_1];
    I=monomialIdeal(x_1*x_2,x_3*x_4,x_5*x_6,x_7*x_8*x_9)
    Ilink=I:ideal(x_1*x_3)
    J=Ilink+ideal(z_1)
    cI=res I
    betti cI
    cJ=res J
    betti cJ
    cc=kustinMillerComplex(cI,cJ,K[x_10]);
    S=ring cc;
    cc
    betti cc
   Text

    We compare again with the combinatorics:

   Example
    R=K[x_1..x_9];
    C=simplicialComplex monomialIdeal(sub(I,R))
    fVector C
    F=face {x_1,x_3}
    R'=K[x_1..x_10];
    C'=substitute(stellarSubdivision(C,F,K[x_10]),R')
    fVector C'
    I'=monomialIdeal(sub(cc.dd_1,R'))
    C'===simplicialComplex I'
  SeeAlso
    kustinMillerComplex
    res
    betti
///




doc ///
  Key
    "Cyclic Polytopes"
  Headline
    Constructing minimal resolutions for Stanley-Reisner rings of boundary complexes of cyclic polytopes
  Description
   Text
    In the following example we construct the minimal resolution of the Stanley-Reisner ring of
    the cyclic polytope $\Delta(4,8)$ of embedding codimension 4 (as a subcomplex of the simplex on 8 vertices)
    from those of the cyclic polytopes $\Delta(2,6)$ and $\Delta(4,7)$ (the last one being Pfaffian).

    This process can be iterated to give a recursive construction of the
    resolutions of all cyclic polytopes, for details see

    {\it J. Boehm, S. Papadakis: On the structure of Stanley-Reisner rings associated to cyclic polytopes}, @HREF"http://arxiv.org/abs/0912.2152"@, to appear in Osaka J. Math.

   Example
     K=QQ;
     C26=delta(2,K[z,x_2..x_6])
     R=K[z,x_1..x_7]
     J=sub(ideal C26,R)
     c26=res J;
     betti c26
     C47=delta(4,K[x_1..x_7])
     I=sub(ideal C47,R)
     c47=res I;
     betti c47
     cc=kustinMillerComplex(c47,c26,K[x_8]);
     betti cc
   Text

     We compare with the combinatorics, that is, check that
     the Kustin-Miller complex at the special fiber z=0 indeed resolves 
     the Stanley-Reisner ring of $\Delta(4,8)$.

   Example
     R'=K[x_1..x_8];
     C48=delta(4,R')
     I48=ideal C48
     betti res I48
     I48==sub(ideal cc.dd_1,R')
   Text

     We finish the example by printing the differentials of the Kustin-Miller complex:

   Example
     print cc.dd_1
     print cc.dd_2
     print cc.dd_3
  SeeAlso
    kustinMillerComplex
    res
    betti
///

-- remark: print command avoids matrices to be broken to the next line in the html


doc ///
  Key
    "Tom"
  Headline
    The Kustin-Miller complex for Tom
  Description
   Text
    The Kustin-Miller complex construction for the Tom example which can be found in Section 5.5 of 

    Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268, @HREF"http://arxiv.org/abs/math/0111195"@

    Here we pass from a Pfaffian to a codimension 4 variety.

   Example
     R = QQ[x_1..x_4,z_1..z_4]
     b2 = matrix {{0,x_1,x_2,x_3,x_4},{-x_1,0,0,z_1,z_2},{-x_2,0,0,z_3,z_4},{-x_3,-z_1,-z_3,0,0},{-x_4,-z_2,-z_4,0,0}}
     betti(cI=resBE b2)
     b1 = cI.dd_1
     J = ideal (z_1..z_4);
     betti(cJ=res J)
     betti(cU=kustinMillerComplex(cI,cJ,QQ[T]))
     S=ring cU
     isExactRes cU
     print cU.dd_1
     print cU.dd_2
     print cU.dd_3
     print cU.dd_4
  SeeAlso
    kustinMillerComplex
    res
    betti
    "Jerry"
///

-- we use print to avoid line breaking inside the matrices

doc ///
  Key
    "Jerry"
  Headline
    The Kustin-Miller complex for Jerry
  Description
   Text
    The Kustin-Miller complex construction for the Jerry example which can be found in Section 5.7 of 

    Papadakis, Kustin-Miller unprojection with complexes,  J. Algebraic Geometry 13 (2004) 249-268, @HREF"http://arxiv.org/abs/math/0111195"@

    Here we pass from a Pfaffian to a codimension 4 variety.

   Example
     R = QQ [x_1..x_3, z_1..z_4]
     I = ideal(-z_2*z_3+z_1*x_1,-z_2*z_4+z_1*x_2,-z_3*z_4+z_1*x_3,-z_3*x_2+z_2*x_3,z_4*x_1-z_3*x_2)
     cI=res I
     betti cI
     J = ideal (z_1..z_4)
     cJ=res J
     betti cJ
     cc=kustinMillerComplex(cI,cJ,QQ[T]);
     S=ring cc
     cc
     betti cc
     isExactRes cc
     print cc.dd_1
     print cc.dd_2
     print cc.dd_3
     print cc.dd_4
  SeeAlso
    kustinMillerComplex
    res
    betti
    "Tom"
///



-*
check "KustinMiller"
uninstallPackage("KustinMiller")
installPackage("KustinMiller")
installPackage("KustinMiller",RerunExamples=>true)
viewHelp("KustinMiller")
*-

end

restart
uninstallPackage "KustinMiller"
installPackage "KustinMiller"
check KustinMiller


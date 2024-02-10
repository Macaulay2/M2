newPackage("ToricCohomology",
    Headline => "toric and multiproj cohomology",
    Version => "0.1",
    Date => "June 10, 2009",
    PackageImports => { "LLLBases", "FourierMotzkin" },
    Authors => {
         {Name => "Alexandra Seceleanu", Email => "asecele2@illinois.edu"}}
    )

export {"picard","toricIdeal","toricVariety","toricHH","irrelevantIdeal"}



-- =======================================================================================
--Algorithm for producing toricIdeal taken from "Computations with Macaulay2" book
-- chapter "Toric Hilbert Scheme"

toBinomial = (b,R) -> (
     top := 1_R; bottom := 1_R;
     scan(#b, i -> if b_i > 0 then top = top * R_i^(b_i)
          else if b_i < 0 then bottom = bottom * R_i^(-b_i));
     top - bottom);


-- makes a toric ideal from a matrix (lattice)
toricIdeal = method()
toricIdeal (Matrix) := (A) -> (
    n := numcols A;
    R = QQ[vars(0..n-1),Degrees=>entries transpose A,MonomialSize=>16];
    B := transpose LLL syz matrix A;
    J := ideal apply(entries B, b -> toBinomial(b,R));
    scan(gens ring J, f -> J = saturate(J,f));
    J
    );

 toricVariety = method()
 toricVariety (Matrix) := (A)->(
      I := toricIdeal(A);
      Proj(ring I/I)
      )

-- =========================================================================================== 
 
-- makes a cone over a polytope by placing it at height 1 in a higher dim space
 
 homogenizePolytope = V -> (                                             
               R := ring V;                                                       
               n := numgens source V;                                             
               map(R^1, R^n, {toList(n:1)}) || V); 

-- computes the irrelevant ideal of a toric with from the ray generators of its fan

irrelevantIdeal = method ()
irrelevantIdeal (Matrix,Ring) := (M,R) -> (
     rays := homogenizePolytope M;
     H := fourierMotzkin rays;
     n := numcols H#0;
     inequalityMatrix :=  transpose submatrix(H#0,{1..n-1},);
     inequalityVector := transpose submatrix(H#0,{0},);
     ones := matrix{for i to numcols M-1 list 1};
     B := inequalityMatrix * M +inequalityVector ** ones;
     --x := global x;
     --R := QQ[x_1..x_(numrows B)];
     incidence = matrix table(numrows B,numcols B, (i,j) -> if B_(i,j) == 0 then 1 else 0)**R;
     ideal apply (flatten entries ( incidence*transpose vars R), a -> product support a)
     )
-- ==========================================================================================



      

 

picard = method()
picard (Matrix):= (M)-> (
     return ker (M)
     )



-- auxiliary functions needed below
convert := l-> apply(l,i->apply (i, j->if j==0 then 0 else 1))
diagonal:= l-> for i to n list( apply(select(l,j->j_0==i),k->k_1))

-- INPUTS: a monomial ideal, the irrelevant ideal of the toric variety
-- OUTPUT: the ZZ^n support of the module Ext^{i}(B,S)
     
extSupport = method()
extSupport (Ideal):= (B)-> (
     R = ring B;
     B1 = dual monomialIdeal B;
     supp={};
     for i to n do (
	  T = Tor_i(module R/B1, module  R/ideal gens R);
	  if T!= 0 then  supp=append(supp,apply(unique convert(degrees T),j->((sum j)-i+1,j)));
	  );
     supp=flatten supp;
     return  diagonal(supp)
)


-- INPUTS: a lattice M and a coarse degree p in Pic(X)
-- OUTPUT: the least integer l such that Ext^i(R/B^l,R)_p=H^i_B(R)_p (that is the first integer after which the Ext modules stabilize)

bound = method()
bound(Matrix,Sequence):= (M,p)-> (
     d := numrows M;
     m := max for i in p list abs(i);
     Q1 := max flatten entries M;
     Qdminus1 :=max flatten entries gens minors(d-1,M);
     qd := min apply(flatten entries gens minors(d,M),abs);
     return d^2*m*Q1*Qdminus1/qd
     )

-- INPUTS:  a lattice M, a sequence of coarse degrees P in Pic(X) 
-- OUTPUT: the least integer l such that Ext^i(R/B^l,F)_d=H^i_B(F)_d (that is the first integer after which the Ext modules stabilize)
   
bound(Module,Sequence):= (d,P)-> (
     if isFreeModule P then  return max apply(degrees P,a->bound(M,d-a))
     else (
	  resol := res P;
	  return max for i to pdim P list bound(resol_i,d)
	  )
     )


-- INPUTS: an integer i and a coarse degree d in Pic(X)
-- OUTPUT: the least integer l such that Ext^i(R/B^l,R)_d=H^i_B(R)_d (that is the first integer after which the Ext modules stabilize)

toricHH = method ()
toricHH (ZZ,Matrix,Ideal,Sequence):= (i,M,B,d)-> (
     l := bound(M,d);
     Bl := ideal apply(flatten entries gens B, j->j^l);
     Ext^i(module ring B/Bl,module ring B)
)
	  



beginDocumentation()

document {
	Key =>ToricCohomology,
	Headline => "the cohomology of toric varieties",
	PARA{"This package uses an explicit description of the cohomology of toric varieties based on:"},
	PARA{"Cohomology on Toric varieties and Local Cohomology with Monomial Supports, by D. Eisenbud, M. Mustata, M. Stillman"},
	PARA{""}
}

document {
	Key =>picard,
	Headline => "computation of the Picard group of a toric variety",
	Inputs => {{"M, a ", TO Matrix, "representing the toric variety"}},
	Outputs =>{"the Picard group, as a cokernel"},
	Usage => "picard (M)",
	Caveat => {},
EXAMPLE lines ///
	n = 4;
	R = QQ[x_1..x_n,Degrees=>apply(n,i->flatten entries(id_(ZZ^n)^{i}))];
	e = 2;
	M = transpose matrix {{1,0},{0,1},{-1,e},{0,-1}}
///,
     PARA{"Consider the toric variety given by the fan whose rays are columns of M:"},
     PARA{"The toric variety is a Hirzebruch surface (rational normal scroll)"},
EXAMPLE lines ///
	pic = picard M
///,
	PARA{"is the Picard group of this variety. Note that it is generated by two divisors of multidegrees (0,1,0,1) and (1,0,1,e)."},
	PARA{"This function is part of the package ToricCohomology."}
}



document {
	Key =>toricHH,
	Headline => "computation of the i^th cohomology module of a toric variety in specified multidegree",
	Inputs => {{"i, an ", TO ZZ},{"M, a ", TO Matrix, "representing the toric variety"},{"B, the irrelevant",TO Ideal},{"d, a multidegree"}},
	Outputs =>{"the ith cohomology module of the toric variety in multidegree d"},
	Usage => "toricHH(i,M,B,d)",
	Caveat => {"the multigrading is given by the Picard group of the toric variety"},
EXAMPLE lines ///
	n = 4;
	R = QQ[x_1..x_n,Degrees=>apply(n,i->flatten entries(id_(ZZ^n)^{i}))];
	e = 2;
	M = transpose matrix {{1,0},{0,1},{-1,e},{0,-1}}
///,
     PARA{"Consider the toric variety given by the fan whose rays are columns of M:"},
EXAMPLE lines ///
	X = toricVariety (M)
	pic = picard M
///,
	PARA{"is the Picard group of this variety. Note that it is generated by two divisors of multidegrees (0,1,0,1) and (1,0,1,e)."},
	PARA{"The toric variety is a Hirzebruch surface (rational normal scroll) with irrelevant ideal"},
EXAMPLE lines ///
        B = intersect(ideal(x_1,x_3),ideal (x_2,x_4));
///,
        PARA{"The nonzero cohomology occurs in"},
EXAMPLE lines ///
     	toricHH(2,M,B,(-1,0,-1,0))  
	toricHH(3,M,B,(-1,-1,-1,-1))
///,
	PARA{"This function is part of the package ToricCohomology."}
}

document {
	Key =>irrelevantIdeal,
	Headline => "computation of the irrelevant ideal in the Cox ring of a toric variety",
	Inputs => {{"M, a ", TO Matrix, "representing the toric variety"},{"R, a",TO Ring}},
	Outputs =>{"the irrelevant ideal"},
	Usage => "irrelevantIdeal(M,R)",
	Caveat => {"the multigrading is given by the Picard group of the toric variety"},
	PARA{"This function is part of the package ToricCohomology."}
}

document {
	Key =>{toricIdeal,(toricIdeal,Matrix)},
	Headline => "computation of the irrelevantdefining ideal of a toric variety",
	Inputs => {{"M, a ", TO Matrix, "representing the toric variety"}},
	Outputs =>{"the defining equations"},
	Usage => "toricIdeal(M)",
	Caveat => {},
	EXAMPLE lines ///
	n = 4;
	R = QQ[x_1..x_n,Degrees=>apply(n,i->flatten entries(id_(ZZ^n)^{i}))];
	e = 2;
	M = transpose matrix {{1,0},{0,1},{-1,e},{0,-1}}
///,
     PARA{"Consider the toric variety given by the fan whose rays are columns of M:"},
EXAMPLE lines ///
	X = toricVariety (M)
///,
	PARA{"The code for this function can be found in chapter 5 of Computations in Algebraic Geometry with Macaulay 2 "},
	PARA{"This function is part of the package ToricCohomology."}
}

document {
	Key =>toricVariety,
	Headline => "gives the toric variety associated to a set of ray generators",
	Inputs => {{"M, a ", TO Matrix, "representing the toric variety"}},
	Outputs =>{"the toric variety"},
	Usage => "toricVariety(M)",
	Caveat => {},
	PARA{"This function is part of the package ToricCohomology."}
}



end
	  
n=3;
R = QQ[x_1..x_n,Degrees=>apply(n,i->flatten entries(id_(ZZ^n)^{i}))];
--n = # gens ring B;
--S = newRing(ring B,Degrees=>apply(n,i->flatten entries(id_(ZZ^n)^{i})));
--f = map(B,S);
--B = f B;

--===========================================================================
-- INPUTS: a matrix M representing the  toric variety X and a degree d in the Picard group of X
-- OUTPUT: a representative of d in the finer Z^(numRows M) grading
representative =method()
representative (Matrix,List):= (M,d)-> (
     m = map(M);
     return (inverse(M))(d)
     )

--============================================================================
--trying to get the bound by linear programming


bound = method()
bound (Matrix,ZZ,List):= (M,i,d)-> (
     dplusM := transpose matrix for l to numcols M-1 list toList d;
     si := (extSupport B)_i;
     return max apply si (j-> max (linearprogram(dplusM,j)))     
     )

linearprogram := (M,l)-> (
     	  S := ZZ[t_1..t_(numcols M)];
	  equations := M* transpose vars S;
	  for i in l do (
	       if i!= 0 then 

--=====================================================================================

     X = {};
     R1 = R;
     for i to n-1 do (
	  X = append (X,gens R1);
	  R1=coefficientRing R1;
	  );
     X = flatten X;
     A =(R1)[X];
     f =map(A,R);
     g =map(R,A);
     B1 = f B;
     B2 = dual monomialIdeal B1;
     B3 =g B2;
     for i to n do (
	  T = Tor_i(module R/B3, module  R/ideal X);
	  p = multidegree T;
	  
=================================================================================
--my version

toricIdeal = (S, a) -> (
           -- check that S is a polynomial ring over a field
           n := numrows a;
	   m := numcols a;
           if not all(flatten entries a, i -> instance(i,ZZ) )
             then error "expected a matrix of integers";
	   topa := max for i to numcols a -1  list sum flatten entries a_i;
           s := symbol s;
	   t := symbol t;
           k := coefficientRing S;
           M1 := monoid [t,s_1..s_n];
           R1 := k M1;
	   R2 := k [Variables=> m ];
	   t := (flatten entries vars R1)_0;
           s = flatten entries vars R1-set {t};
           mm := matrix {for j to numcols a-1 list ( product for i to n-1 list s_i^(a_(i,j))* t^(topa - sum flatten entries a_j))};
           j := generators kernel map(R1, R2, mm);
           ideal substitute(j, submatrix(vars S, {0..n}))
           )



-- makes a scripted functor i.e toricHH^i will now mean toricHH(i,....)
toricHH = new ScriptedFunctor from {
     supscript => (
	  i -> new ScriptedFunctor from {
	       argument =>  (M,B,P,d) -> (
		      	      f := lookup (toricHH,class i,class M,class B,class d);
		    	      if f === null then error "no method available"
		    	      else f(i,M,B,P,d)
			      )
		    }
		    )
	       }
-- gives the grading group for the toric (which is Pic of the toric variety)
-- INPUTS: a matrix representing the  toric variety X
-- OUTPUT: the Picard group Pic(X)
    
	  

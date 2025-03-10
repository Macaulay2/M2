-* 
restart

peek loadedFiles
uninstallPackage "HilbertSchemeStrata"

--restart
installPackage "HilbertSchemeStrata"
viewHelp HilbertSchemeStrata

*-

newPackage(
        "HilbertSchemeStrata",
        Version => "0.5", 
        Date => "July 5th 2022",
        Authors => {{Name => "Frank Schreyer", 
                  Email => "schreyer@math.uni-sb.de", 
                  HomePage => "https://www.math.uni-sb.de/ag/schreyer/"}
	      },
        Headline => "Computation of strata of a Hilbert Scheme",
        DebuggingMode => true
        )

export{
    "unfolding",
    "flatteningRelations",
    "removeVariables",
    "substituteFamily",
    "passToGB"   
    }



extraTerms=method()
extraTerms(Ideal,ZZ) := (I,d) ->(
    gens trim ideal ((basis(d,ring I))%I))

extraTerms(Ideal,ZZ,RingElement) := (I,d,m) ->(
    if not (#terms m ==1 and leadCoefficient m ==1) then error "expected a monomial";
    L:=(entries gens trim ideal ((basis(d,ring I))%I))_0;
    matrix{select(L,t-> t<m)})

extraTerms(Ideal,ZZ,Ideal) := (I,d,qperp) ->(
    q1:=ideal image basis(d,module qperp);
    mingens trim ideal (gens q1%I))

TEST /// 
S=QQ[x_0..x_3]
q=x_0*x_3+x_1*x_2
s2=basis(2,S)
qperp=ideal(s2*syz diff(s2,q))
I0=ideal(x_0,x_1,x_2^4)
I=intersect(I0,qperp)
betti res I
q1:=basis(2,module qperp)
truncate(2,qperp)
truncate(3,qperp)
extraTerms(I,2,qperp)
///    

unfolding=method()
unfolding(Ideal,Ring) := (I,R) -> (
    kk:= coefficientRing R;
    lTs:=apply(numgens I,i-> leadTerm I_i);
    tailTerms:=apply(lTs,m->extraTerms(I,(degree m)_0,m));
    ind :=flatten apply(#lTs,i->apply(rank source  tailTerms_i,j->(i,j)));   
    a :=symbol a;
    Ra:=kk[apply(ind,ij->a_ij)];
    RA :=R**Ra;
    tails:=apply(#lTs,i->sum(rank source tailTerms_i,
	    j->(sub(a_(i,j),RA)*sub((tailTerms_i)_(0,j),RA))));
    F:=RA^(apply(degrees source gens I,d->{-d_0,0}));
    unfold:=map(RA^1,F,matrix{apply(#lTs,i->sub(lTs_i,RA)+tails_i)});
    return (unfold,Ra))

unfolding(Ideal,Ideal) := (I,qperp) -> (
    R:= ring I;
    kk:= coefficientRing R;
    lTs:=apply(numgens I,i-> leadTerm I_i);
    tailTerms:=apply(lTs,m->extraTerms(I,(degree m)_0,qperp));
    ind :=flatten apply(#lTs,i->apply(rank source  tailTerms_i,j->(i,j)));   
    a :=symbol a;
    Ra:=kk[apply(ind,ij->a_ij)];
    RA :=R**Ra;
    tails:=apply(#lTs,i->sum(rank source tailTerms_i,
	    j->(sub(a_(i,j),RA)*sub((tailTerms_i)_(0,j),RA))));
--tails
    F:=RA^(apply(degrees source gens I,d->{-d_0,0}));
    unfold:=map(RA^1,F,matrix{apply(#lTs,i->sub(I_i,RA)+tails_i)});
    return (unfold,Ra))


unfolding(Ideal) := I -> (
    R:=ring I;
    kk:= coefficientRing R;
    degs:=degrees source gens I; 
    degs1:=unique degs;
    tailTerms:=apply(degs1,d->extraTerms(I,d_0));
    T1 :=tally degs;
    T2 :=apply(#degs1,i->rank source tailTerms_i);
    a := symbol a; d:= null;
    ind :=flatten apply(#degs1,k-> (d=degs1_k;
	    flatten apply(T2_k,j->apply(T1_d,i->(k,i,j)))));   
    Ra:=kk[apply(ind,kij->a_kij)];
    As :=apply(#degs1,k->(d=degs1_k;matrix apply(T2_k,j->apply(T1_d,i->a_(k,i,j)))));   
    RA :=R**Ra;
    tail:=apply(#degs1,k->sub(tailTerms_k,RA)*sub(As_k,RA));
    pos:=apply(degs1,d->positions(degs,e->d==e));
    F:=RA^(apply(degrees source gens I,d->{-d_0,0}));
    unfold:=map(RA^1,F,gens sum(#degs1,k->
	    ideal(sub((gens I)_(pos_k),RA)+tail_k)));
    return (unfold,Ra))
TEST ///    

kk=ZZ/nextPrime 10^3
S=kk[x_0..x_3]
I=ideal(x_0*x_3,x_1*x_2,x_0*x_2-x_1*x_3,x_1^2,x_0*x_1,x_0^2,x_2^4)
(unf,R)=unfolding(I)
I1=(ideal(x_0,x_1))^2
(unf1,R1)=unfolding(I1)
I2=ideal(x_0^2,x_0*x_1,x_0*x_2,x_1^3)
(unf,R)=unfolding(I2)
gens R
I0=I2
///
 
passToGB=method()
passToGB(Ideal,Matrix) := (I,unf) -> (
    kk:= coefficientRing ring I;
    I0:=gens gb I;
    n:= numgens I;
    b:=(syz(gens I| I0));
    b0:=sub(b,kk);
    cols:=select(rank source b,i->(sub(b,kk))_{i}!=0);
    b1:=b^{0..n-1}_cols;
    unf1:=unf*sub(b1,ring unf);
    unf1=unf1*inverse contract(transpose sub(leadTerm I0,ring unf),unf1);
    (ideal I0,unf1)    
    )

TEST ///
--- vsp4
kk=QQ
--kk=ZZ/101
S=kk[x_0..x_3]
q=x_0*x_2+x_1*x_3
s2=basis(2,S)
qperp=ideal(s2*syz diff(s2,q))
betti (fperp=res qperp)

line=ideal (x_0,x_1)
badScheme=intersect (line, qperp)
betti res badScheme
I=ideal (gens badScheme)_{5,4,3,2,1,0}+ideal x_2^4
betti res I
gens gb I

(unf,R)=unfolding(I,qperp)  

(I1,unf1)=passToGB(I,unf)
elapsedTime betti( J=flatteningRelations(I1,unf1,R))



///

flatteningRelations=method()
flatteningRelations(Ideal,Matrix,Ring) := (I0,unf,R) -> (
    assert(
	 rank source gens gb I0== rank source gens I0
	 );
    s0 :=syz gens I0;
    Ra :=ring unf;
    F1:= Ra^(apply(degrees target s0,d->{-d_0,0}));
    F2:= Ra^(
	apply(degrees source s0,d->{-d_0,0})
	);
    s0 =map(F1,F2,sub(s0,Ra));
    rel1 := unf*s0;
    I0g := sub(gens I0,Ra);
    rel12:=rel1;
    E:=id_(target s0);
    ss :=s0; s':=null; s1:= null;
    while (rel12=unf*ss;
	s1=sum(numgens I0, i->(s'=contract(I0g_i,rel12);
	    rel12=rel12-I0g_(0,i)*s';E_{i}*s'));
    	ss=ss-s1; not s1==0) do ();
    base1:=unf*ss;
    degs:=degrees source base1;	d:= null;   
    trim sum(#degs,i->(d=degs_i_0;
	ideal sub(contract(sub(extraTerms(I0,d),Ra),base1_{i}),R)))
) 
   
TEST /// 
kk=QQ
S=kk[x_0..x_3]
I0=ideal(x_0^2,x_0*x_1,x_0*x_2,x_1^3)
I0=(ideal(x_0,x_1))^2
(unf,R)=unfolding(I0)
base=flatteningRelations(I0,unf,R)
betti unf
betti base
linearPart1=(gens base%(ideal vars R)^2)
leadTerm linearPart1

dim ideal linearPart1
dim R
dim base


I0=ideal(x_0^2,x_0*x_1,x_0*x_2,x_1^3)
(unf,R)=unfolding(I0)
base=flatteningRelations(I0,unf,R)
betti unf
betti base
linearPart1=(gens base%(ideal vars R)^2)

betti trim ideal linearPart1
dim ideal linearPart1
dim R
m42x27=syz(map(R^1,R^{42:-1},linearPart1),DegreeLimit=>1)

m42x15=mingens(image(id_(R^42)%image map(R^42,R^27,m42x27)))
lTm=matrix{apply(15,i->leadTerm (linearPart1*m42x15)_(0,i))}
tailTerm=(gens base)*m42x15-lTm
D=diagonalMatrix apply(15,i->leadCoefficient lTm_(0,i))
ltm1=lTm*D
 tailTerm1=tailTerm*D
subst=flatten apply(15,i->{ltm1_(0,i) => -tailTerm1_(0,i)})
base1=base
while(base2=trim sub(base1,subst);
    #support base2 < #support base1) do (base1=base2)
betti base2
Rbase=kk[support base2]
Ibase=sub(base2,Rbase)
cb=decompose ideal Ibase_0

cb1=apply(cb,i->trim (i+Ibase))
cb2=sum((entries gens cb1_0)_0,t->ideal t:cb1_1)
betti (lincb2=trim ideal(gens cb2%(ideal gens Rbase)^2))
betti cb2
dim cb2, dim lincb2
dim cb1_1
inter=trim (cb_1+cb2)
dim(inter)
dim ideal (gens inter% (ideal gens Rbase)^2)
///    
removeVariables=method()
-- Input: an inhomogeneous ideal J in a polynomial ring R
-- Output:(J1,h), an ideal J1 in a smaller polynomial ring R1 
--        of a subset of the variables
--        such that R1->R induces an isomorphism R1/J1 -> R/J
--        and a substitution h:R -> R1, which induces the inverse isomorphism
-- Assumption: No generator has a constant term
-- Method: look for linear part of a generator and try to remove 
--         the lead term variable via a substitution by 
--         the tail of the equation
removeVariables(Ideal) := J -> (
    R:= ring J;
    if not isPolynomialRing R then 
         error "expected an ideal in a polynomial ring";
    mIdeal:=ideal gens R;    
    if not (gens J%mIdeal==0) then 
        error "expected an ideal whose generators have no constant part";    
    gJ:=gens J;
    F1 := R^{rank source gJ:-1};
    linPart:= map(R^1, F1,gJ%(mIdeal)^2);
    slP:=syz(linPart,DegreeLimit=>1);
    m1 :=mingens image(id_(F1)%image slP);
    r:=rank source m1;
    linPart1 :=linPart*m1;
    m2:=gens gb linPart1//linPart1;
    linPart2 := linPart1*m2;
    assert(linPart2==gens gb linPart1);
    LlTm:=apply(r,i->leadTerm linPart2_(0,i));
    assert(#unique LlTm== r);
    lTm:=matrix{apply(r,i->leadTerm linPart2_(0,i))};
    D := inverse diagonalMatrix apply(r,i-> leadCoefficient lTm_(0,i));
    lTm=lTm*D;
    tailTerm:=gJ*m1*m2*D-lTm; t:=null;
    pos:=positions(toList(0..r-1),i->(t=lTm_(0,i);
	   not member(t,support tailTerm_(0,i))));
    assert(#pos==r);
    subst:=flatten apply(pos,i->{lTm_(0,i) => -tailTerm_(0,i)});
    assert all(r,i-> sub(gJ*m1*m2*D_{i},subst_i)==0);
    graph:=flatten apply(r,i->apply(
	   select(r,j-> member(lTm_(0,i),support tailTerm_{j})),j->(i,j)));
    isDirected :=isDirectedGraph graph;
--graph
--isDirected = true;
    if isDirected then (
    -- why is the order as it is?
    -- it is in general not!
    gi :=graph; gi2:=gi;
    maxPathLength :=1;
    while(gi2=flatten apply(graph,ij->
	    apply(select(gi,jk->ij_1==jk_0),jk->(ij_0,jk_1)));
	#gi2>0) do (maxPathLength=maxPathLength+1; gi=gi2);
    subst1 := if maxPathLength==0 then subst else (
    	--tailTerm1:=tailTerm;
        scan(maxPathLength,i->tailTerm=sub(tailTerm,subst));
        flatten apply(r,i->{lTm_(0,i) => -tailTerm_(0,i)}));
    J1 :=trim sub(J,subst1);
    kk :=coefficientRing R;R1:=null;
    h:=sub(vars R,subst1);
    if numgens J1 == 0 then (R1=kk[support h]; return (ideal 0_R1,sub(h,R1)));
    R1 =kk[support h];
    return (sub(J1,R1),sub(h,R1)));
    )
TEST /// 
 kk=QQ
S=kk[x_0..x_3]
I0=ideal(x_0^2,x_0*x_1,x_0*x_2,x_1^3)

(unf,R)=unfolding(I0)
base=flatteningRelations(I0,unf,R)
betti(J=base)
ring J
J1=removeVariables J
  ///

substituteFamily=method()
substituteFamily(Matrix, Ring, Matrix) := (fam,S,h) -> (
    SR := S**ring h;
    sub(fam,sub(vars S, SR)|sub(h,SR))
    )

isDirectedGraph=method()
isDirectedGraph(List) := L -> (
    -- Input: L a list of directed edges of a graph
    -- Output: Boolean
    -- true if the Graph contains no loops
    all(L,p->(L3:=successors(p_0,L);
	   not member(p_0,L3)))
)   

successors=method()
successors(ZZ,List) := (i,L) -> (
    pos:=positions(L,p->p_0==i);
    L1:= unique apply(L_pos,i->i_1); 
    l1:=#L1; L2:=null;
    while ( pos=positions(L,p->member(p_0,L1));
	    L2= unique (L1|apply(L_pos,i->i_1));
	    #L2>l1) do (L1=L2;l1=#L1);
    L2)		     
TEST ///
L={(1,2),(2,3)}
i=1
successors(i,L)
isDirectedGraph L
L={(1,2),(2,3),(3,1)}
successors(i,L)
isDirectedGraph L
///
    

beginDocumentation()

document {
  Key => HilbertSchemeStrata,
  Headline => "Computation of strata of an Hilbert scheme",
  "This package contains the implementation of an Groebner basis approach 
  to the Hilbert scheme. The basic set-up is to compute an unfolding of the 
  generators of an ideal corresponding to a point in the Hilbert scheme, and then to compute the 
  flattening relations via Buchberger tests." 
  ,

   PARA{},
    SUBSECTION "Setup for the construction",
    UL{
        TO unfolding,
	TO flatteningRelations,
	TO passToGB
        },
    SUBSECTION "Clean up the presentation",
    UL{
        TO removeVariables,
	TO substituteFamily
        }   
}

doc ///
  Key 
   unfolding
   (unfolding,Ideal)
   (unfolding,Ideal,Ring)
   (unfolding,Ideal,Ideal)
  Headline 
   computes the unfolding of an ideal
  Usage
   (unf,R) = unfolding(I)
   (unf,R) = unfolding(I0,S)
   (unf,R) = unfolding(I0,S)
  Inputs
   I0: Ideal
           an ideal of a subscheme of P^n 
   S: Ring
       needed to specify a global monom order
   I1: Ideal
         if present, only unfolding terms in this ideal are added    
  Outputs
   unf: Matrix
       the matrix of generators of the unfolding in R**S
   R: Ring
        the coefficient ring of the unfolding
  Description
    Text
     The unfolding of the ideal I0 has generators
     which are the sum of a generator f of I0 plus a sum of monomials a_i*m_i
     where the a_i are in in coefficient ring R and m_i monomials of the same 
     degree than f, which 
     are not in the leading term ideal of I0. 
     
     In the presence of S the m_i are 
     smaller with respect to the monom order of S than the leadTerm of f.
     If an ideal I1 is present then the m_i are required to be in I1, and they 
     might be polynomials in S.
     
     Below we treat the example of Piene-Schlessinger which computes
     the nature of the Hilbert scheme of the twisted cubic curve in P^3
     at its worst degeneration: A triple line in a plane with an embedded point.
    Example
     kk=QQ
     S=kk[x_0..x_3]--,MonomialOrder=>Lex]
     I=ideal(x_0^2,x_0*x_1,x_0*x_2,x_1^3)
     betti res I, degree I, dim I
     (unf,R) =unfolding(I);
     SR=ring unf;
     transpose unf
     gens R
    Text
     We analyze this example further.
    Example     
     betti (base=flatteningRelations(I,unf,R))
     (J,h)=removeVariables(base);
     betti base, betti J
     dim ring J
     dim J
     J_0 
     unf_{2}, support J_0
     J1= trim( J+ideal ((support J_0)_0))
     betti(J2=trim (J:J1))
     dim J1, dim J2
     (removeVariables J2)_0
     J12=J1+J2;
     J12r=(removeVariables J12)_0
     dim J12r
    Text
     So there are two smooth rational components of dimension 15 and 12 which intersect
     along a smooth rational subvariety of dimension 11.
    Example
     fam0=substituteFamily(unf,S,h);
     hs={(removeVariables J1)_1,(removeVariables J2)_1,(removeVariables J12)_1};
     fams=apply(hs,h->substituteFamily(fam0,S,h));
    Text 
     Next we compute the scheme to a random points in each family:
    Example
     Xs=apply(#hs,i->ideal sub(fams_i,vars S|random(S^1,S^(numgens ring hs_i))));
     netList apply(Xs, X-> (betti res X, apply(decompose X, c-> betti c)))
     Y=Xs_2;
     betti (Yr=radical Y)
     betti(pt=Y:Yr)
     singYr=saturate (Yr+minors(2,jacobian Yr))
     singYr==pt
    Text
     We conclude that the the general scheme corresponding to a point in the intersection
     of the two components is a nodal plane curve with an embedded point at the node.
     Notice that the plane curve of the random example cannot 
     have a cusps, because its singular loci is reduced.      
/// 



doc ///
  Key 
   passToGB
   (passToGB,Ideal,Matrix)
  Headline 
   computes the unfolding of the GB induced by an unfolding of the generators   
  Usage
   (I0,unf1) = passToGB(I,unf)
  Inputs
   I: Ideal
           an ideal of a subscheme of P^n 
   unf: Matrix
       	   a matrix descibing an unfulding of I   
  Outputs
   I0: Ideal
       generated by the GB of I
   unf1: Matrix
        the induced unfolding of the GB
  Description
    Text
      Given an unfolding of an ideal I,
      the corresponding unfolding of the GB I0 of I
      is computed.
    Example
      kk=QQ
      S=kk[x_0..x_3]
      q=x_0*x_2+x_1*x_3
      s2=basis(2,S)
      qperp=ideal(s2*syz diff(s2,q))
      betti (fperp=res qperp)
      line=ideal (x_0,x_1)
      badScheme=intersect (line, qperp)
      betti res badScheme
      I=ideal (gens badScheme)_{5,4,3,2,1,0}+ideal x_2^4
      betti res I
      betti gb I
      (unf,R)=unfolding(I,qperp);
      SR=ring unf
      transpose unf      
      (I0,unf1)=passToGB(I,unf);
      I0
      transpose unf1          
///

 






doc ///
  Key 
   flatteningRelations
   (flatteningRelations,Ideal, Matrix, Ring)
  Headline 
    Compute the flattening relation of an unfolding in the coefficient ring R
  Usage
    base = flatteningRelations(I, unf, R)
  Inputs
    I: Ideal
        generated by a GB of a subscheme of PP^n
    unf: Matrix
          of generators of an unfolding of I with coefficients in R   
  Outputs
    base: Ideal
          in the coefficient ring R
  Description
    Text
      Given an unfolding of an ideal I, the functions computes the ideal base of the relations in R 
      obtained from the Buchberger test syzygies.
      Thus $$R/J -> k[PP^n]**(R/J)/ideal(unf) $$ is flat.

    Example
      kk=ZZ/101
      S=kk[x_0..x_3]
      I=intersect( ideal(x_0,x_1^2), ideal(x_0^2,x_1,x_2))
      (unf,R)=unfolding I;
      base=flatteningRelations(I,unf,R);
      betti base

    Text
      We analyse the base further.
    Example
     (J,h)=removeVariables base;
     fam=substituteFamily(unf,S,h)
     betti J 
     R'=ring J   
     J
     dim J
     cJ=decompose J;
     L=apply(cJ,c->removeVariables c);
     Lfam=apply(L,c->(hc=c_1;(substituteFamily(fam,S,sub(vars ring J,hc)),ring hc)));
     netList apply(Lfam,famR ->(flatteningRelations(I,famR_0,famR_1),dim famR_1))
     LX = apply(Lfam,famR->
	     ideal sub(famR_0, vars S|random(kk^1,kk^(#gens famR_1))));
     netList apply(LX,X-> (betti res X, tally apply(decompose X,c-> (dim c, degree c, betti res c))))
    Text
      We conclude that there are two components of the Hilbert scheme of P^3 for 2t+2.
      An eight-dimensional consisting of two skew lines and a 11-dimensional 
      consisting of a plane conic union a point.
  Caveat 
    The ideal I has to be generated by a minimal Gröbner basis.
  SeeAlso
    passToGB      
///

 
doc ///
  Key 
   removeVariables
   (removeVariables,Ideal)
  Headline 
   Try to remove variables
  Usage
    (J,h) = removeVariables I
  Inputs
    I: Ideal
        the ideal of a subscheme of AA^n  
  Outputs
    J: Ideal
          of a subscheme of AA^m
    h: Matrix
        substitution matrix of a surjective map
	$ k[AA^n]\to k[AA^m]$
        which induces the isomorphism 
	$ k[AA^n]/I \to k[AA^m]/J$
  Description
    Text
      Given ideal I, whose zero loci contains the origin,
      the function remove some of the variables x_i by using 
      generators of the form f_i=x_i+f_i' and the substitution $\phi$
      of the form $\{ x_i => -f_i' \}$
      provided this $\phi$ stabilizes, i.e., $\phi^N=\phi^{N+1}$
      for $N$ sufficiently large.  

    Example
      kk=ZZ/101
      S=kk[x_1..x_3]
      m=matrix{{1,x_1,x_2,x_3},{x_1,x_2,x_3,0}}
      I=minors(2,m)
      (J,h)=removeVariables I
      ring J
///

doc ///
  Key 
   substituteFamily
   (substituteFamily,Matrix,Ring,Matrix)
  Headline 
   substitute a family of ideal using h
  Usage
    fam1 = substitute(fam,S,h)
  Inputs
    fam: Matrix
        of generators of ideal depending on some parameter in R
    S: Ring
       the embient ring of of the special fiber
    h: Matrix 
       describing a substitution R to ring h   	 
  Outputs
    fam1: Matrix
        describing the generators of the pullback family
  Description
    Text
      Given a family fam over Spec R, the function returns the 
      pullback family under the map R to ring h.
      We illustate this with the Piene-Schlessinger example.     
    Example
     kk=QQ
     S=kk[x_0..x_3]--,MonomialOrder=>Lex]
     I=ideal(x_0^2,x_0*x_1,x_0*x_2,x_1^3)
     betti res I, degree I, dim I
     (unf,R) =unfolding(I);
     SR=ring unf;
     transpose unf
     numgens R
   Text
     We analyze this example further.
   Example     
     betti (base=flatteningRelations(I,unf,R))
     (J,h)=removeVariables(base);
     fam=substituteFamily(unf,S,h);
     Sh=ring fam
     #support fam, #support unf
     betti base, betti J
     dim ring J
     dim J
     J_0 
     unf_{2}, support J_0
     J1= trim( J+ideal ((support J_0)_0))
     betti(J2=trim (J:J1))
     dim J1, dim J2
     (J1s,h1)=removeVariables J1
   Text
     Now we compute the pullback family
   Example
     betti(fam1=substituteFamily(fam,S,h1))
     SR1=ring fam1; gens SR1
     transpose fam1
   Text
     The family depends on 15 parameters.
   Example
     #gens ring h1, support h1
   Text
     Similarly for the other component we get a 12 dimensional family
   Example  
     (J2s,h2)=removeVariables J2
     betti(fam2=substituteFamily(fam,S,h2))
     SR2=ring fam2; gens SR2
     #gens ring h2, support h2
     support fam2      
///

end
---%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

restart
installPackage "HilbertSchemeStrata"
loadPackage "HilbertSchemeStrata"




-- -*- coding: utf-8 -*-

newPackage(
    "SCMAlgebras",
    Version => "1.0",
    Date => "October 2, 2024",
    Authors => {{Name => "Ernesto Lax", Email => "erlax@unime.it", HomePage => "https://www.researchgate.net/profile/Ernesto-Lax"}},
    Headline => "a package to work with sequentially Cohen-Macaulay modules or ideals",
    Keywords => {"Commutative Algebra"},
    PackageExports => {"Depth","MinimalPrimes"},
    DebuggingMode => false
)

export{
    -- Methods
    "deficiencyModule",
    "canonicalModule",
    "filterIdeal",
    "isSCM",
    -- Service
    "minDim"
};


MyDoc := ""; 
MyDoc = concatenate(MyDoc,///
Node
  Key
    SCMAlgebras
  Headline
    a package to work with sequentially Cohen-Macaulay modules or ideals
  Description
    Text
      SCMAlgebras is a package to check whether a module or an ideal is sequentially Cohen-Macaulay,
      by computing the modules of deficiency and the filter ideals.
///);



-------------
-- FUNCTIONS |
---------------------------------------------------------------------------------------------
--=======================================================================
-- computes the ith module of deficiency of an ideal/module
--=======================================================================
MyDoc=concatenate(MyDoc,///
Node
  Key
    deficiencyModule
    (deficiencyModule,Module,ZZ)
    (deficiencyModule,Ideal,ZZ)
  Headline
    computes the $i$th module of deficiency of a module $M$ or an ideal $I$.
  Usage
    deficiencyModule(M,i)
    deficiencyModule(I,i)
  Inputs
    M:Module
      module over the polynomial ring $S=K[x_1,\ldots,x_n]$, with $K$ a field or an ideal $I\subset S$
    i:ZZ
      an integer
  Outputs
    D:Module
      the ith module of deficiency of $M$
  Description
    Text
      Let $S=K[x_1,\ldots,x_n]$ be the polynomial ring.
      Given a $S$-module $M$ (or an ideal $I\subset S$), it returns the $i$th module of deficiency $\omega^i(M)$ (or $\omega^i(S/I)$), defined as $\mathrm{Ext}_S^{n-i}(M,S(-n))$.
    Example
      S=QQ[x_1..x_5];
      I=(x_1^2*x_3,x_2*x_3^2*x_4,x_1*x_3^3*x_5);
      M=S^1/I;
      deficiencyModule(M,3)
  SeeAlso
    canonicalModule
    isSCM
///);
-------------------------------------------------------------------------
deficiencyModule = method(TypicalValue=>Module);
-------------------------------------------------------------------------
deficiencyModule(Module,ZZ) := (M,i) -> (
    S:=ring M;
    n:=dim S;
    t:=depth M;
    d:=dim M;
    if i<t or i>d then (
        Di:=Ext^(n+1)(M,S^{-n})
    ) else Di=Ext^(n-i)(M,S^{-n});
    return Di
)
-------------------------------------------------------------------------
deficiencyModule(Ideal,ZZ) := (I,i) -> (
    S:=ring I;
    return deficiencyModule((S^1/I),i)
)
--=======================================================================


--=======================================================================
-- computes the canonical module of an module
--=======================================================================
MyDoc=concatenate(MyDoc,///
Node
  Key
    canonicalModule
    (canonicalModule,Module)
    (canonicalModule,Ideal)
  Headline
    computes the canonical module of a module $M$ or an ideal $I$.
  Usage
    canonicalModule M
    canonicalModule I
  Inputs
    M:Module
      module over the polynomial ring $S=K[x_1,\ldots,x_n]$, with $K$ a field or an ideal $I\subset S$
  Outputs
    K:Module
      the canonical module
  Description
    Text
      Let $S=K[x_1,\ldots,x_n]$ be a polynomial ring. Given a $S$-module $M$ (or an ideal $I\subset S$), it returns the canonical module $\omega(M)$ (or $\omega(S/I)$),
      as $\mathrm{Ext}_S^{n-d}(M,S(-n))$, where $d=\dim M$ (or $d=\dim S/I$).
    Example
      S=QQ[x_1..x_5];
      I=(x_1^2*x_3,x_2*x_3^2*x_4,x_1*x_3^3*x_5);
      M=S^1/I;
      canonicalModule M
  SeeAlso
    deficiencyModule
///);
-------------------------------------------------------------------------
canonicalModule = method(TypicalValue=>Module);
-------------------------------------------------------------------------
canonicalModule(Module) := M -> (
    S:=ring M;
    d:=dim M;
    r:=dim S;
    K:=Ext^(r-d)(M,S^{-r});
    return K
)
-------------------------------------------------------------------------
canonicalModule(Ideal) := I -> (
    S:=ring I;
    return canonicalModule(S^1/I)
)
--=======================================================================


--=======================================================================
-- compute the minimum value of i such that the filter ideal
-- I^<i> differs from> I
--=======================================================================
MyDoc=concatenate(MyDoc,///
Node
  Key
    minDim
    (minDim,Ideal)
  Headline
    computes the minimum dimension of $I$.
  Usage
    minDim I
  Inputs
    I:Ideal
      a homogeneous ideal of the polynomial ring $S=K[x_1,\ldots,x_n]$, with $K$ a field
  Outputs
    k:ZZ
      the minimum dimension of $I$
  Description
    Text
      Let $S=K[x_1,\ldots,x_n]$ be a polynomial ring. Given a homogeneous ideal $I\subset S$, it returns the minimum dimension,
      defined as the minimum integer $k$ such that the filter ideal $I^{<k>}$ differs from $I$.
    Example
      S = QQ[x_1..x_10,y_1..y_10];
      E = {{1,2},{1,3},{1,4},{1,5},{1,6},{1,7},{1,8},{1,9},{1,10},{6,7},{8,9},{8,10},{9,10}};
      J=ideal(for e in E list x_(e#0)*y_(e#1)-x_(e#1)*y_(e#0));
      minDim J
  SeeAlso
    filterIdeal
    isSCM
///);
-------------------------------------------------------------------------
minDim = method(TypicalValue=>ZZ);
minDim(Ideal) := I -> (
    T:=decompose I;
    D:=for Q in T list dim(radical Q);
    return min(D)
)
--=======================================================================


--=======================================================================
-- computes the filter ideal I^<i> according to Goodarzi's definition
--=======================================================================
MyDoc=concatenate(MyDoc,///
Node
  Key
    filterIdeal
    (filterIdeal,Ideal,ZZ)
  Headline
    computes the $i$th filter ideal of $I$.
  Usage
    filterIdeal(I,i)
  Inputs
    I:Ideal
      a homogeneous ideal of the polynomial ring $S=K[x_1,\ldots,x_n]$, with $K$ a field
    i:ZZ
      an integer greater or equal than -1
  Outputs
    J:Ideal
      the $i$th filter ideal of $I$
  Description
    Text
      Let $I\subset S$ be a homogeneous ideal, with $d=\dim S/I$, and let $I=\displaystyle\bigcap_{j=1}^r Q_j$ be the minimal primary decomposition of $I$.
      For all $1\leq j\leq r$, let $P_j = \sqrt{Q_j}$ be the radical of $Q_j$. For all $-1\leq i\leq d$, the $i$th filter ideal of $I$ is $$I^{<i>} = \bigcap_{\dim S/{P_j}>i} Q_{j},$$
      where $I^{<-1>}=I$ and $I^{<d>}=S$.
    Example
      S = QQ[x_1..x_10,y_1..y_10];
      E = {{1,2},{1,3},{1,4},{1,5},{1,6},{1,7},{1,8},{1,9},{1,10},{6,7},{8,9},{8,10},{9,10}};
      J=ideal(for e in E list x_(e#0)*y_(e#1)-x_(e#1)*y_(e#0));
      filterIdeal(J,5)
  SeeAlso
    minDim
    isSCM
///);
-------------------------------------------------------------------------
filterIdeal = method(TypicalValue=>Ideal);
-------------------------------------------------------------------------
filterIdeal(Ideal,ZZ) := Ideal => (I,i) -> (
    S:=ring I;
    d:=dim I;
    d0:=minDim I;
    if i>-2 and i<d0 then (
        Ii:=I;
    ) else if i>=d0 and i<d then ( 
        Q:=decompose I;
        L:={};
        for k from 0 to #Q-1 do (
            Qk:=Q#k;
            Pk:=radical Qk;
            dk:=dim Pk;
            L=L|{{dk,Qk}};
        );
        Pi:={};
        for k from 0 to #L-1 do (
            if (L#k)#0 > i then (
                Pi=Pi|{(L#k)#1};
            );
        );
        Ii=intersect(Pi);
    ) else if i==d then (
        Ii=ideal(S^1);
    ) else error("Expected an integer greater than -2 and at most " | toString(d) | ".");
    return Ii
)
--=======================================================================


--=======================================================================
-- checks whether an ideal/module is sequentially Cohen-Macaulay
-- -- for a module the Schenzel's characterization is used by default
-- -- for an ideal the Goodarzi's characterization is used by default
--=======================================================================
MyDoc=concatenate(MyDoc,///
Node
  Key
    isSCM
    (isSCM,Module)
    (isSCM,Ideal)
  Headline
    checks whether a module or an ideal is sequentially Cohen-Macaulay
  Usage
    isSCM M
    isSCM I
  Inputs
    M:Module
      a finitely generated graded $S$-module, $S=K[x_1,\ldots,x_n]$, with $K$ a field or an ideal $I\subset S$
  Outputs
    b:Boolean
      whether the module $M$ or the graded algebra $S/I$ is sequentially Cohen-Macaulay
  Description
    Text
      Given a finitely generated graded $S$-module $M$, this method checks if the $i$th module of deficiency of $M$, $\omega^{i}(M)$, if non-zero, is Cohen-Macaulay of dimension $i$.
      For a homogeneous ideal $I\subset S$, the function checks if $\mathrm{depth} S/{I^{<i>}} \geq i+1$, where $I^{<i>}$ is the $i$th filter ideal.
    Example
      S=QQ[x_1..x_5];
      M=coker matrix{{x_1*x_2,x_3*x_4,0,0},{0,x_1*x_5,x_2*x_4,0}};
      isSCM M
    Example
      S = QQ[x_1..x_10,y_1..y_10];
      E = {{1,2},{1,3},{1,4},{1,5},{1,6},{1,7},{1,8},{1,9},{1,10},{6,7},{8,9},{8,10},{9,10}};
      J=ideal(for e in E list x_(e#0)*y_(e#1)-x_(e#1)*y_(e#0));
      isSCM J
///);
-------------------------------------------------------------------------
isSCM = method(TypicalValue=>Boolean);
-------------------------------------------------------------------------
isSCM(Module) := M -> (
    d:=dim M;
    for i from 0 to d-1 do ( 
        Oi:=deficiencyModule(M,i);
        if  Oi != 0 then (
            if (isCM(Oi) == false) or (dim(Oi)!=i) then (
                return false;
            );
        );
    );
    return true;
)
------------------------------------------------------------------------- 
isSCM(Ideal) := I -> (
    S:=ring I;
    d:=dim I;
    d0:=minDim I;
    for i from 0 to d-1 do (
        Ii:=filterIdeal(I,i);
        Si:=(S^1/Ii);
        if depth Si < i+1 then
            return false;	
    );
    return true;
)
--=======================================================================



-----------------
-- DOCUMENTATION |
--------------------------------------------------------------------------------------------
beginDocumentation();
multidoc(MyDoc);
-------------------------------------------------------------------------



---------
-- TESTS |
--------------------------------------------------------------------------------------------

--========================
-- deficiencyModule test
--========================
TEST ///
S = QQ[x_1..x_5]
M = coker matrix {{x_1*x_2,0,0},{x_1*x_4,0,x_3*x_5}}
assert(deficiencyModule(M,3)==0)
///


--========================
-- canonicalModule test
--========================
TEST ///
S = QQ[x_1..x_5]
M = coker matrix {{x_1*x_2,0,0},{x_1*x_4,0,x_3*x_5}}
assert(isCM canonicalModule(M))
///


--========================
-- minDim test
--========================
TEST ///
S = QQ[x_1..x_4,y_1..y_4]
E = {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {3, 4}}
J = ideal(for e in E list x_(e#0)*y_(e#1)-x_(e#1)*y_(e#0));
assert(minDim(J)==4)
///


--========================
-- filterIdeal test
--========================
TEST ///
S = QQ[x_1..x_4,y_1..y_4]
E = {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {3, 4}}
J = ideal(for e in E list x_(e#0)*y_(e#1)-x_(e#1)*y_(e#0));
--I = ideal (x_4*y_3-x_3*y_4,x_2*y_4-x_4*y_2,x_3*y_2-x_2*y_3,x_4*y_1-x_1*y_4,x_3*y_1-x_1*y_3,x_2*y_1-x_1*y_2)
assert(filterIdeal(J,4)!=J)
///


--========================
-- isSCM test
--========================
TEST ///
S = QQ[x_1..x_4,y_1..y_4]
E = {{1, 2}, {1, 3}, {1, 4}, {2, 3}, {3, 4}}
J = ideal(for e in E list x_(e#0)*y_(e#1)-x_(e#1)*y_(e#0));
assert(isSCM(J)==true)
///


-------------------------------------------------------------------------


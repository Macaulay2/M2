-- -*- coding: utf-8 -*-

newPackage(
        "MatchingPowers",
        Version => "1.0", 
        Date => "March 11, 2024",
        Authors => {{Name => "Antonino Ficarra", Email => "antficarra@unime.it", HomePage => "https://www.researchgate.net/profile/Antonino-Ficarra"}},
        Headline => "A Macaulay2 package for computing the matching powers of monomial ideals",
        Keywords => {"Documentation"},
        DebuggingMode => false,
        Reload => true
        )

export {"toMultidegree",
        "toMonomial",
        "boundingMultidegree",
        "matchingProduct",
        "matchingPower",
        "monomialGrade",
        "gFunction",
        "isLinearlyRelated"
};

mydoc := "";
mydoc = concatenate(mydoc,///
Node
  Key
    MatchingPowers
  Headline
    a package for computing the the matching powers of monomial ideals
  Description
    Text
      MatchingPowers is a package for computing the the matching powers of monomial ideals
///);


-------------------------------------------------------------------------------------------
--  Compute the monomial with a given multidegree
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    toMonomial
    (toMonomial,Ring,List)
  Headline
    compute the monomial with a given multidegree
  Usage
    toMonomial(S,l)
  Inputs
    S:Ring
      a polynomial ring $S = K[x_1,\ldots,x_n]$, $K$ a field
    l:List
      the multidegree $(a_1,\dots,a_n)$
  Outputs
    u:RingElement
      the monomial $x_1^{a_1}\cdots x_n^{a_n}\in S$
  Description
    Text
      The multidegree of a monomial $u\in S=K[x_1,\ldots,x_n]$ is the vector $(a_1,a_2,\dots,a_n)$ where $a_i=\max\{j:x_i^j\ \text{divides}\ u\}$.
    Example
      S = QQ[x_1..x_4];
      l = {0,2,1,2};
      toMonomial(S,l)
///);

toMonomial = method(TypicalValue=>RingElement)
toMonomial (Ring,List) := (S,l) -> (
	n:=numgens S-1;
	u:=1;
        j:=0;
        if not(#l==n+1) then error "the list does not have the right length";
        product(n + 1, i -> S_i^(l#i))
    );

-------------------------------------------------------------------------------------------
--  Compute the multidegree of a given monomial
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    toMultidegree
    (toMultidegree,RingElement)
  Headline
    compute the multidegree of a monomial
  Usage
    toMultidegree u
  Inputs
    u:RingElement
      a monomial $u=x_1^{a_1}\cdots x_n^{a_n}$
  Outputs
    l:List
      the multidegree $(a_1,\dots,a_n)$ of $u$
  Description
    Text
      The multidegree of a monomial $u\in S=K[x_1,\ldots,x_n]$ is the vector $(a_1,a_2,\dots,a_n)$ where $a_i=\max\{j:x_i^j\ \text{divides}\ u\}$.
    Example
      S = QQ[x_1..x_4];
      u = x_2^2*x_3*x_4^2;
      toMultidegree(u)
///);

toMultidegree = method(TypicalValue=>List)
toMultidegree (RingElement) := u -> (
	S:=ring u;
        if not isMonomialIdeal(ideal(u)) then error "expected a monomial";
        n:=numgens S-1;
        L := for i to n list for j to n list if i == j then 1 else 0;
        R:=newRing(S,Degrees=>L);
        f:=map(R,S);
        degree f u
    );

-------------------------------------------------------------------------------------------
--  Compute the bounding multidegree of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    boundingMultidegree
    (boundingMultidegree,Ideal)
  Headline
    compute the bounding multidegree of a monomial ideal
  Usage
    boundingMultidegree I
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    l:List
      the bounding multidegree of $I$: $\textbf{deg}(I)$
  Description
    Text
      The bounding multidegree of a monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ is the vector $\textbf{deg}(I)=(\text{deg}_{x_1}(I),\text{deg}_{x_2}(I),\dots,\text{deg}_{x_n}(I))$ with $$\text{deg}_{x_i}(I)=\max_{u\in G(I)}\max\{j:x_i^j\ \text{divides}\ u\},$$ where $G(I)$ is the minimal monomial generating set of $I$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      boundingMultidegree(I)
///);

boundingMultidegree = method(TypicalValue=>List)
boundingMultidegree (Ideal) := I -> (
	if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        n:=numgens S-1;
        L:=apply(flatten entries mingens I,x->toMultidegree(x));
        max \ transpose L
    );

-------------------------------------------------------------------------------------------
--  Compute the matching product of two monomial ideals
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    matchingProduct
    (matchingProduct,Ideal,Ideal)
  Headline
    compute the matching product of two monomial ideals
  Usage
    matchingProduct(I,J)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
    J:Ideal
      a monomial ideal $J\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    L:Ideal
      the matching product $I*J$
  Description
    Text
      For a monomial $u\in S$, we define its support as the set $\text{supp}(u)=\{i:x_i\ \text{divides}\ u\}$. The matching product of two monomial ideals $I,J\subset S=K[x_1,\dots,x_n]$ is the monomial ideal defined as $$I*J=(uv:u\in I,v\in J,\text{supp}(u)\cap\text{supp}(v)=\emptyset).$$
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1*x_2,x_3^2,x_4^2);
      J = ideal(x_1*x_3,x_2);
      matchingProduct(I,J)
///);


matchingProduct = method(TypicalValue=>Ideal)
matchingProduct (Ideal,Ideal) := (I,J) -> (
        if ring I=!=ring J then error "expected monomial ideals in the same polynomial ring";
        S:=ring I;
        gI:=apply(apply(flatten entries mingens I,x->toMultidegree x),y->toMonomial(S,y));
        gJ:=apply(apply(flatten entries mingens J,x->toMultidegree x),y->toMonomial(S,y));
        l:={};
        for u in gI do (
              for v in gJ do (
                   if (set support u)*(set support v)===set{} then l=append(l,u*v);
              );
        );
        if ideal(l)==ideal(0_ZZ) then return ideal(0_S);
        trim ideal(l)
);

-------------------------------------------------------------------------------------------
--  Compute a matching power of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    matchingPower
    (matchingPower,Ideal,ZZ)
  Headline
    compute the kth matching power of a monomial ideal
  Usage
    matchingProduct(I,k)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
    k:ZZ
      a positive integer
  Outputs
    L:Ideal
      the $k$th matching power $I^{[k]}$
  Description
    Text
      For a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, we define $I^{[1]}=I$, and recursively $I^{[k]}=I^{[k-1]}*I$ for $k\ge2$. The monomial ideal $I^{[k]]$ is called the $k$th matching power of $I$.
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1*x_2,x_3^2,x_4^2);
      matchingPower(I,2)
      matchingPower(I,3)
      matchingPower(I,4)
///);

matchingPower = method(TypicalValue=>Ideal)
matchingPower (Ideal,ZZ) := (I,k) -> (
        J:=I;
        for i from 1 to k-1 do (
            J=matchingProduct(J,I);
        );
        J
);

-------------------------------------------------------------------------------------------
--  Compute the monomialGrade of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    monomialGrade
    (monomialGrade,Ideal)
  Headline
    compute the monomial grade of a monomial ideal
  Usage
    monomialGrade(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    k:ZZ
      the monomial grade of $I$: $\nu(I)$
  Description
    Text
      For a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, the monomial grade of $I$, denoted by $\nu(I)$, is the biggest integer $k$ such that $I^{[k]}\ne0$.
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1*x_2,x_3^2,x_4^2);
      monomialGrade I
///);

monomialGrade = method(TypicalValue=>ZZ)
monomialGrade (Ideal) := I -> (
        k:=1;
        while matchingPower(I,k+1)=!=ideal() do (
              k=k+1;
        );       
        k
);

-------------------------------------------------------------------------------------------
--  Compute the normalized depth function of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    gFunction
    (gFunction,Ideal)
  Headline
    compute the normalized depth function of a monomial ideal
  Usage
    gFunction(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    l:List
      the normalized depth function of $I$
  Description
    Text
      For a monomial ideal $I\subset S=K[x_1,\dots,x_n]$ and a positive integer $k$ such that $I^{[k]}\ne0$, we define $$g_I(k)=\text{depth}(S/I^{[k]})+|\textbf{deg}(I)|-n-(\text{indeg}(I)-1)=|\textbf{deg}(I)|-\text{projdim}(I^{[k]})-\text{indeg}(I^{[k]}).$$ Here $|\textbf{deg}(I)|=\deg_{x_1}(I)+\dots+\deg_{x_n}(I)$ and $\text{indeg}(I)$ is the initial degree of $I$, that is the minimum degree of a monomial generator of $I$. The normalized depth function of $I$ is $$g_I=(g_I(1),\dots,g_I(\nu(I))).$$
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1*x_2,x_3^2,x_4^2);
      gFunction I
///);

gFunction = method(TypicalValue=>List)
gFunction (Ideal) := I -> (
	k:=monomialGrade I;
        G:={};
        m:=sum boundingMultidegree I;
        for p from 1 to k do (
            G=append(G,m-pdim(module matchingPower(I,p))-(min apply(flatten entries mingens matchingPower(I,p),x->degree x))#0);
        );
        G
    );

-------------------------------------------------------------------------------------------
--  Whether a monomial ideal is linearly related
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    isLinearlyRelated
    (isLinearlyRelated,Ideal)
  Headline
    checks if a monomial ideal is linearly related
  Usage
    isLinearlyRelated(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ is linearly related
  Description
    Text
      A monomial ideal $I\subset S=K[x_1,\dots,x_n]$ is linearly related if $I$ is generated in a single degree $d$ and has linear relations in degree $d+1$, that is $\beta_1(I)=\beta_{1,d+1}(I)$.
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1*x_2,x_3^2,x_4^2);
      isLinearlyRelated I
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1*x_2,x_1*x_3,x_3^2);
      isLinearlyRelated I
///);

isLinearlyRelated = method(TypicalValue=>Boolean)
isLinearlyRelated (Ideal) := I -> (
        genDeg:=flatten apply(flatten entries mingens I,x->degree x);
        if min(genDeg)=!=max(genDeg) then error "expected an ideal generated in a single degree";
        S:=ring I;
        E:=toList(set flatten entries (res I).dd_2 - set{0_S});
        max(flatten apply(E,x->degree x))==1
    );
	
-------------------------------------------------------
--DOCUMENTATION MatchingPowers
-------------------------------------------------------
beginDocumentation();
multidoc(mydoc);


------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------

----------------------------
-- Test toMonomial
----------------------------
TEST ///
S = QQ[x_1..x_4]
l = {0,2,1,2}
assert(toMonomial(S,l) == x_2^2*x_3*x_4^2)
///

----------------------------
-- Test toMultidegree
----------------------------
TEST ///
S = QQ[x_1..x_4]
u = x_2^2*x_3*x_4^2
assert(toMultidegree(u) == {0,2,1,2})
///

----------------------------
-- Test boundingMultidegree
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(boundingMultidegree(I) == {1,1,2,2,1})
///

----------------------------
-- Test matchingProduct
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_3^2,x_4^2)
J = ideal(x_1*x_3,x_2)
assert(matchingProduct(I,J) == ideal(x_1*x_3*x_4^2,x_2*x_3^2,x_2*x_4^2))
///

----------------------------
-- Test matchingPower
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_3^2,x_4^2)
assert(matchingPower(I,1) == I)
///

----------------------------
-- Test matchingPower
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_3^2,x_4^2)
assert(matchingPower(I,3) == ideal(x_1*x_2*x_3^2*x_4^2))
///

----------------------------
-- Test monomialGrade
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_3^2,x_4^2)
assert(monomialGrade I == 3)
///

----------------------------
-- Test gFunction
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_3^2,x_4^2)
assert(gFunction I == {2,1,0})
///

----------------------------
-- Test isLinearlyRelated
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_3^2,x_4^2)
assert(isLinearlyRelated I == false)
///

----------------------------
-- Test isLinearlyRelated
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1*x_2,x_1*x_3,x_3^2)
assert(isLinearlyRelated I == true)
///

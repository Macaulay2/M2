-- -*- coding: utf-8 -*-
newPackage(
        "HomologicalShiftIdeals",
        Version => "1.0", 
        Date => "May 9, 2022",
        Authors => {{Name => "Antonino Ficarra", Email => "antficarra@unime.it"}},
        Headline => "compute the homological shift ideals of a monomial ideal",
        DebuggingMode => false,
        PackageImports => {"SimplicialComplexes", "SimplicialDecomposability"},
        Reload => true
        )

export {"supportIdeal",
        "isFullySupported",
        "toMonomial",
        "toMultidegree",
        "boundingMultidegree",
        "multigradedShifts",
        "HS",
        "socle",
        "hasLinearResolution",
        "hasHomologicalLinearResolution",
        "hasLinearQuotients",
        "hasHomologicalLinearQuotients",
        "admissibleOrder",
        "isAdmissibleOrder",
        "isPolymatroidal",
        "isHomologicalPolymatroidal"
};

mydoc := "";
mydoc = concatenate(mydoc,///
Node
  Key
    HomologicalShiftIdeals
  Headline
    a package for computing the homological shift ideals of a monomial ideal of a polynomial ring
  Description
    Text
      HomologicalShiftIdeals is a package for computing the homological shift ideals of a monomial ideal of a polynomial ring
///);

-------------------------------------------------------------------------------------------
-- Compute the support of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    supportIdeal
    (supportIdeal,Ideal)
  Headline
    compute the support of a monomial ideal
  Usage
    supportIdeal(I)
  Inputs
    I:Ideal
      a monomial ideal $I$ of a polynomial ring $S = K[x_1,\ldots,x_n]$, $K$ a field
  Outputs
    l:List
      the support of $I$
  Description
    Text
      The support of a monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ is the set $$\text{supp}(I)=\bigcup_{u\in G(I)}\text{supp}(u),$$ where $G(I)$ is the minimal monomial generating set of $I$ and $\text{supp}(u)=\{x_j:x_j\ \text{divides}\ u\}$ for a monomial $u$.
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^2,x_2,x_3,x_1^2*x_4);
      supportIdeal(I)
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^2,x_2^5,x_3,x_1*x_4);
      supportIdeal(I)
///);

supportIdeal = method(TypicalValue=>List)
supportIdeal (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        GI:=flatten entries mingens I;
        suppI:=set{};
        sum(GI, w -> set support w)
    );

-------------------------------------------------------------------------------------------
-- Whether an ideal is fully supported
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    isFullySupported
    (isFullySupported,Ideal)
  Headline
    whether a monomial ideal is fully supported
  Usage
    isFullySupported(I)
  Inputs
    I:Ideal
      a monomial ideal $I$ of a polynomial ring $S = K[x_1,\ldots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ is fully supported
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ is fully supported if $\text{supp}(I)=\{x_1,\dots,x_n\}$.
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^2,x_2,x_3,x_1^2*x_4);
      isFullySupported(I)
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^2,x_3,x_2*x_4);
      isFullySupported(I)
///);

isFullySupported = method(TypicalValue=>Boolean)
isFullySupported (Ideal) := I -> (
        n:=numgens ring I;
        suppI:=supportIdeal I;
        #suppI==n
    );

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
        if #l != n+1 then error "the list does not have the right length";
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
        L:=apply(flatten entries mingens I,toMultidegree);
        max \ transpose L
    );

-------------------------------------------------------------------------------------------
--  Compute the kth multigraded shifts of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    multigradedShifts
    (multigradedShifts,Ideal,ZZ)
  Headline
    compute the kth multigraded shifts of a monomial ideal
  Usage
    multigradedShifts(I,k)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
    k:ZZ
      an integer $k$
  Outputs
    l:List
      the $k$th multigraded shifts of $I$
  Description
    Text
      Given a monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ with minimal multigraded free resolution $$\mathbb{F}:\cdots\rightarrow\bigoplus_{j=1}^{\beta_k(I)}S(-\textbf{a}_{k,j})\rightarrow\bigoplus_{j=1}^{\beta_{k-1}(I)}S(-\textbf{a}_{k-1,j})\rightarrow\cdots\rightarrow\bigoplus_{j=1}^{\beta_0(I)}S(-\textbf{a}_{0,j})\rightarrow I\rightarrow 0,$$ the set of the $k\text{th}$ multigraded shifts of $I$ is $$\{\textbf{x}^{\textbf{a}_{k,j}}:j=1,\dots,\beta_k(I)\}.$$
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      multigradedShifts(I,0)
      multigradedShifts(I,1)
      multigradedShifts(I,2)
      multigradedShifts(I,3)
      multigradedShifts(I,4)
      multigradedShifts(I,5)
///);

multigradedShifts = method(TypicalValue=>List)
multigradedShifts (Ideal,ZZ) := (I,k) -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        n:=numgens S-1;
        r:=k+1;
        L:={};
        for i from 0 to n do (
              mdeg:={};
              for j from 0 to n do (
                     if j==i then mdeg=append(mdeg,1)
                     else mdeg=append(mdeg,0);
              );
              L=append(L,mdeg);
        );
        R:=newRing(S,Degrees=>L);
        f:=map(R,S);
        J:=f I;
        Res:=resolution J;
        d:=degrees Res_r;
        unique for w in d list toMonomial(S,w)
    );

-------------------------------------------------------------------------------------------
--  Compute the kth homological shift ideal of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    HS
    (HS,Ideal,ZZ)
  Headline
    compute the kth homological shift ideal of a monomial ideal
  Usage
    HS(I,k)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
    k:ZZ
      an integer $k$
  Outputs
    l:List
      the $k$th homological shift ideal of $I$
  Description
    Text
      Given a monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ with minimal multigraded free resolution $$\mathbb{F}:\cdots\rightarrow\bigoplus_{j=1}^{\beta_k(I)}S(-\textbf{a}_{k,j})\rightarrow\bigoplus_{j=1}^{\beta_{k-1}(I)}S(-\textbf{a}_{k-1,j})\rightarrow\cdots\rightarrow\bigoplus_{j=1}^{\beta_0(I)}S(-\textbf{a}_{0,j})\rightarrow I\rightarrow 0,$$ the $k\text{th}$ homolofical shift ideal of $I$ is $$\text{HS}_k(I)=(\textbf{x}^{\textbf{a}_{k,j}}:j=1,\dots,\beta_k(I)).$$
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      HS(I,0)
      HS(I,1)
      HS(I,2)
      HS(I,3)
      HS(I,4)
      HS(I,5)
///);

HS = method(TypicalValue=>Ideal)
HS (Ideal,ZZ) := (I,k) -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        M:=multigradedShifts(I,k);
        H:=trim ideal(M);
        if H==(0) or H==(1) then return ideal(0_S)
        else return H
    );

-------------------------------------------------------------------------------------------
--  Compute the socle elements of a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    socle
    (socle,Ideal)
  Headline
    compute the socle of a monomial ideal
  Usage
    socle(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    l:List
      the socle elements of $I$
  Description
    Text
      Given a monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$, the socle set of $I$ is the set given by all monomials $v$ such that $v\notin I$ and $x_iv\in I$ for all $i=1,\dots,n$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      socle I
///);

socle = method(TypicalValue=>List)
socle (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        n:=numgens S-1;
        soc:=multigradedShifts(I,n);
        if not isFullySupported(I) then error "expected a fully supported monomial ideal";
        P:=product gens S;
        L:=for w in soc list w/P;
        f:=map(S,frac S);
        L / f
    );

-------------------------------------------------------------------------------------------
-- Whether an ideal has a linear resolution
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    hasLinearResolution
    (hasLinearResolution,Ideal)
  Headline
    checks if a monomial ideal has linear resolution
  Usage
    hasLinearResolution(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ has linear resolution
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ has linear resolution if $\text{indeg}(I)=\text{reg}(I),$ where $\text{indeg}(I)=\min\{\text{deg}(u):u\in G(I)\}$ is the initial degree of $I$ and $\text{reg}(I)=\max\{j:\beta_{i,i+j}(I)\ne0\}$ is the Castelnuovo-Mumford regularity of $I$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      hasLinearResolution I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      hasLinearResolution I
///);

hasLinearResolution = method(TypicalValue=>Boolean)
hasLinearResolution (Ideal) := I -> (
        S:=ring I;
        min flatten apply(flatten entries mingens I,x->degree x)==regularity I
    );

-------------------------------------------------------------------------------------------
-- Whether a monomial ideal and its homological shift ideals have a linear resolution
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    hasHomologicalLinearResolution
    (hasHomologicalLinearResolution,Ideal)
  Headline
    checks if a monomial ideal has homological linear resolution
  Usage
    hasLinearResolution(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ has homological linear resolution
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ has homological linear resolution if $\text{HS}_k(I)$ has linear resolution for all $k$.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1,x_2,x_3)*ideal(x_1,x_2);
      hasHomologicalLinearResolution I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      hasHomologicalLinearResolution I
///);

hasHomologicalLinearResolution = method(TypicalValue=>Boolean)
hasHomologicalLinearResolution (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        p:=pdim module I;
        i:=0;
        while i<p and hasLinearResolution(HS(I,i)) do (
             i=i+1;
        );
        hasLinearResolution(HS(I,i))
    );

-------------------------------------------------------------------------------------------
-- Whether a monomial ideal has linear quotients
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    hasLinearQuotients
    (hasLinearQuotients,Ideal)
  Headline
    checks if a monomial ideal has linear quotients
  Usage
    hasLinearQuotients(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ has linear quotients
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ has linear quotients if there exists an order $u_1,\dots,u_m$ of the minimal monomial generating set $G(I)$ of $I$ such that the colon ideals $$(u_1,\dots,u_{j-1}):u_j$$ are generated by variables, for all $j=2,\dots,m$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      hasLinearQuotients I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      hasLinearQuotients I
///);

hasLinearQuotients = method(TypicalValue=>Boolean)
hasLinearQuotients (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        DualI:=dual simplicialComplex polarize(monomialIdeal I);
        isShellable(DualI)
    );

-------------------------------------------------------------------------------------------
-- Whether a monomial ideal and its homological shift ideals have linear quotients
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    hasHomologicalLinearQuotients
    (hasHomologicalLinearQuotients,Ideal)
  Headline
    checks if a monomial ideal has homological linear quotients
  Usage
    hasLinearQuotients(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ has homological linear quotients
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ has homological linear quotients if $\text{HS}_k(I)$ has linear quotients for all $k$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      hasHomologicalLinearQuotients I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      hasHomologicalLinearQuotients I
///);

hasHomologicalLinearQuotients = method(TypicalValue=>Boolean)
hasHomologicalLinearQuotients (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        p:=pdim module I;
        i:=0;
        while i<p and hasLinearQuotients(HS(I,i)) do (
             i=i+1;
        );
        hasLinearQuotients(HS(I,i))
    );

-------------------------------------------------------------------------------------------
--  Compute an admissible order of a monomial ideal with linear quotients
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    admissibleOrder
    (admissibleOrder,Ideal)
  Headline
    gives an admissible order of a monomial ideal having linear quotients
  Usage
    admissibleOrder(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    l:List
      an admissible order
  Description
    Text
      Let $I$ be a monomial ideal of a polynomial ring $S=K[x_1,\ldots,x_n]$ having linear quotients. Any linear quotients order $u_1,\dots,u_m$ is called an admissible order. This method determines if a given order of $G(I)$ is an admissible order of $I$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      admissibleOrder I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      admissibleOrder I
///);

admissibleOrder = method(TypicalValue=>List)
admissibleOrder (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        if not hasLinearQuotients(I) then error "the ideal does not have linear quotients";
        S:=ring I;
        n:=numgens S-1;
        bDeg:=boundingMultidegree I;
        L:=for i to n list for j to bDeg#i-1 list S_i;
        J:=polarize (monomialIdeal I);
        DualI:=dual simplicialComplex J;
        Ord:=shellingOrder DualI;
        R:=ring J;
        P:=product gens R;
        M:=for w in Ord list P/w;
        f:=map(S,R,L)*map(R,frac R);
        M / f
    );

-------------------------------------------------------------------------------------------
-- Whether a list of monomials is an admissible order for a monomial ideal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    isAdmissibleOrder
    (isAdmissibleOrder,Ideal,List)
  Headline
    checks whether a list of monomials is an admissible order of a monomial ideal
  Usage
    isAdmissibleOrder(I,l)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
    l:List
      a list of monomials
  Outputs
    b:Boolean
      whether the list is an admissible order of $I$
  Description
    Text
      Let $I$ be a monomial ideal of a polynomial ring $S=K[x_1,\ldots,x_n]$ having linear quotients. Any linear quotients order $u_1,\dots,u_m$ is called an admissible order. This method determines an admissible order of $I$, if $I$ has linear quotients.
    Example
      S = QQ[x_1..x_5,MonomialOrder=>GLex];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      l = sort flatten entries mingens I
      isAdmissibleOrder(I,l)
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      l = {x^2,y^2};
      isAdmissibleOrder(I,l)
///);

isAdmissibleOrder = method(TypicalValue=>Boolean)
isAdmissibleOrder (Ideal,List) := (I,L) -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        if not (set L)===(set flatten entries mingens I) or #L!=#(flatten entries mingens I) then error "incorrect list";
        mu:=#(flatten entries mingens I);
        if mu==1 then return true
        else j:=0;
        -- checks if the colon ideals are generated by a subset of the variables
        while max(flatten apply(flatten entries mingens quotient(ideal(take(L,{0,j})),ideal(L#(j+1))),x->degree x))==1 and j<mu-2 do (
              j=j+1;
        );
        max(flatten apply(flatten entries mingens quotient(ideal(take(L,{0,j})),ideal(L#(j+1))),x->degree x))==1
    );

-------------------------------------------------------------------------------------------
-- Whether a monomial ideal is polymatroidal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    isPolymatroidal
    (isPolymatroidal,Ideal)
  Headline
    checks if a monomial ideal is polymatroidal
  Usage
    isPolymatroidal(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ is polymatroidal
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ is called polymatroidal if for all $u,v\in G(I)$, $u=x_1^{a_1}\cdots x_n^{a_n}$, $v=x_1^{b_1}\cdots x_n^{b_n}$, and all $i$ such that $a_i>b_i$, there exists $j$ with $a_j<b_j$ such that $x_j(u/x_i)\in G(I)$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      isPolymatroidal I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      isPolymatroidal I
///);

isPolymatroidal = method(TypicalValue=>Boolean)
isPolymatroidal (Ideal) := (I) -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        if #(unique apply(flatten entries mingens I,x->degree x))!=1 then error "expected an equigenerated monomial ideal";
        S:=ring I;
        n:=numgens S-1;
        Gens:=flatten entries mingens I;        
        mDegs:=apply(Gens,toMultidegree);
        f:=map(S,frac S);
        A:={};
        for u in mDegs do (
             mDegs1:=delete(u,mDegs);
             for v in mDegs1 do (
                   for i from 0 to n when u#i>v#i do (
                        for j from 0 to n do (
                             if u#j<v#j and member(f (S_j*(toMonomial(S,u))/S_i),Gens) then A=append(A,j);
                        );
                        if A=={} then return false
                        else A={};
                   );
             );
        );
        true
    );

-------------------------------------------------------------------------------------------
-- Whether a monomial ideal and its homological shift ideals are polymatroidal
-------------------------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    isHomologicalPolymatroidal
    (isHomologicalPolymatroidal,Ideal)
  Headline
    checks if a monomial ideal is homological polymatroidal
  Usage
    isHomologicalPolymatroidal(I)
  Inputs
    I:Ideal
      a monomial ideal $I\subset S=K[x_1,\dots,x_n]$, $K$ a field
  Outputs
    b:Boolean
      whether $I$ is homological polymatroidal
  Description
    Text
      A monomial ideal $I$ of a polynomial ring $S=K[x_1,\ldots,x_n]$ is called homological polymatroidal if $\text{HS}_k(I)$ is polymatroidal for all $k$.
    Example
      S = QQ[x_1..x_5];
      I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5);
      isHomologicalPolymatroidal I
    Example
      S = QQ[x,y];
      I = ideal(x^2,y^2);
      isHomologicalPolymatroidal I
///);

isHomologicalPolymatroidal = method(TypicalValue=>Boolean)
isHomologicalPolymatroidal (Ideal) := I -> (
        if not isMonomialIdeal(I) then error "expected a monomial ideal";
        S:=ring I;
        p:=pdim module I;
        i:=0;
        while i<p and isPolymatroidal(HS(I,i)) do (
             i=i+1;
        );
        isPolymatroidal(HS(I,i))
    );

------------------------------------------------------
--DOCUMENTATION HomologicalShiftIdeals
-------------------------------------------------------
beginDocumentation();
multidoc(mydoc);


------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------

----------------------------
-- Test supportIdeal
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1^2,x_2,x_3,x_1^2*x_4)
assert(supportIdeal(I) === set {x_1,x_2,x_3})
///

----------------------------
-- Test isFullySupported
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1^2,x_2,x_3,x_1^2*x_4)
assert(isFullySupported(I) == false)
///

----------------------------
-- Test isFullySupported
----------------------------
TEST ///
S = QQ[x_1..x_4];
I = ideal(x_1^2,x_3,x_2*x_4);
assert(isFullySupported(I) == true)
///

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
-- Test multigradedShifts
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(set multigradedShifts(I,4) === set {x_1*x_2*x_3^2*x_4*x_5,x_1*x_2*x_3*x_4^2*x_5})
///

----------------------------
-- Test HS
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(HS(I,4) == ideal(x_1*x_2*x_3^2*x_4*x_5,x_1*x_2*x_3*x_4^2*x_5))
///

----------------------------
-- Test socle
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(set socle I === set {x_3,x_4})
///

----------------------------
-- Test hasLinearResolution
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(hasLinearResolution I == true)
///

----------------------------
-- Test hasLinearResolution
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^2,y^2)
assert(hasLinearResolution I == false)
///

----------------------------
-- Test hasHomologicalLinearResolution
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(hasHomologicalLinearResolution I == true)
///


----------------------------
-- Test hasLinearQuotients
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(hasLinearQuotients I == true)
///

----------------------------
-- Test hasLinearQuotients
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^2,y^2)
assert(hasLinearQuotients I == false)
///

----------------------------
-- Test hasHomologicalLinearQuotients
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(hasHomologicalLinearQuotients I == true)
///

----------------------------
-- Test hasHomologicalLinearQuotients
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^2,y^2)
assert(hasHomologicalLinearQuotients I == false)
///

----------------------------
-- Test admissibleOrder
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^3,x*y)
assert(admissibleOrder I == {x*y,x^3})
///

----------------------------
-- Test isAdmissibleOrder
----------------------------
TEST ///
S = QQ[x_1..x_5,MonomialOrder=>GLex]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(isAdmissibleOrder(I,rsort flatten entries mingens I) == true)
///

----------------------------
-- Test isAdmissibleOrder
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^3,x*y)
assert(isAdmissibleOrder(I,{x^3,x*y}) == false)
///

----------------------------
-- Test isPolymatroidal
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(isPolymatroidal I == true)
///

----------------------------
-- Test isPolymatroidal
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^2,y^2)
assert(isPolymatroidal I == false)
///

----------------------------
-- Test isHomologicalPolymatroidal
----------------------------
TEST ///
S = QQ[x_1..x_5]
I = ideal(x_1,x_2,x_3,x_4)*ideal(x_3,x_4,x_5)
assert(isHomologicalPolymatroidal I == true)
///

----------------------------
-- Test isHomologicalPolymatroidal
----------------------------
TEST ///
S = QQ[x,y]
I = ideal(x^2,y^2)
assert(isHomologicalPolymatroidal I == false)
///

-- -*- coding: utf-8 -*-

newPackage(
    "VNumber",
    Version => "1.0",
    Date => "September 12, 2024",
    Authors => {{Name => "Antonino Ficarra", Email => "antficarra@unime.it", HomePage => "https://www.researchgate.net/profile/Antonino-Ficarra"},
        {Name => "Emanuele Sgroi", Email => "emasgroi@unime.it", HomePage => "https://www.researchgate.net/profile/Emanuele-Sgroi"}},
    Headline => "compute v-number of homogeneous ideals and v-function of monomial ideals",
    Keywords => {"Documentation"},
    PackageExports => {"PrimaryDecomposition","ReesAlgebra"},
    DebuggingMode => false
)

export {"reesMap",
        "stablePrimes",
        "stableMax",
        "isStablePrime",
        "vNumberP",
        "vNumber",
        "soc",
        "vFunctionP",
        "vFunction",
        --options
        "control"
};

mydoc := "";
mydoc = concatenate(mydoc,///
Node
  Key
    VNumber
  Headline
    a package for computing the v-number and v-function
  Description
    Text
      VNumber is a package for computing the v-number of homogeneous ideals and v-function of monomial ideals
///);

------------------------------------------------------------------------
-- Compute the Rees map of I
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    reesMap
    (reesMap,Ideal)
  Headline
    compute the Rees map of $I$
  Usage
    reesMap I
  Inputs
    I:Ideal
      a homogeneous ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
  Outputs
    g:RingMap
      the Rees map of $I$
  Description
    Text
      Given a homogeneous ideal $I$, with minimal homogeneous generating set $\{f_1,\dots,f_m\}$, this method yields the bigraded map $g: \mathbb{K}[x_1,\dots,x_n,y_1,\dots,y_m] \to \mathbb{K}[x_1,\dots,x_n,t]$ defined by setting $g(x_i) = x_i$ for $1 \le i \le n$ and $g(y_j) = f_j t$ for $1 \le j \le m$ and with $\text{bideg}(x_i) = (1,0)$ and $\text{bideg}(y_j) = (\text{deg}(f_j), 1)$.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3);
      reesMap I
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^3*x_2+x_3^4,x_1+x_2+x_4,x_3^3);
      reesMap I
///);

reesMap = method(TypicalValue => RingMap);
reesMap (Ideal) := (I) -> (
  y := symbol y;
  R := ring I;
  variables := gens R;
  degs := toList(#variables : {1,0}) | {{0,1}};
  S := newRing(R, Variables => variables | {"t"}, Degrees => degs);
  variables = variables / (v -> sub(v,S));
  degs = take(degs, #degs-1) | for d in degrees I list {d#0,1};
  geners := (flatten entries gens I)/(g -> sub(g,S));
  T := newRing(R, Variables => variables | {y_1..y_#geners}, Degrees=>degs);
  rule := variables | (S_#variables)*geners;
  map(S, T, rule)
);

------------------------------------------------------------------------
-- Find Ass^inf(I) when I is a monomial ideal 
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    stablePrimes
    (stablePrimes,Ideal)
  Headline
    compute the set of stable primes of a monomial ideal
  Usage
    stablePrimes I
  Inputs
    I:Ideal
      a monomial ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
  Outputs
    l:List
      all stable primes of $I$
  Description
    Text
      By a classical result of Brodmann, $\text{Ass}(I^k)=\text{Ass}(I^{k+1})$ for all $k\gg0$. We denote the common set $\text{Ass}(I^k)$ for $k\gg0$ by $\text{Ass}^\infty(I)$. A prime ideal $\mathfrak{p}\subset S$ such that $\mathfrak{p}\in \text{Ass}(I^k)$ for all $k \gg 0$ is called a {\tt stable prime} of $I$. This method computes $\text{Ass}^\infty(I)$, the set of the stable primes of $I$.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3);
      stablePrimes I
    Example
      S = QQ[a..e];
      I = ideal(a*d,a*e,b*d,c*d,c*e);
      stablePrimes I
  SeeAlso
    isStablePrime
    stableMax
///);

stablePrimes = method(TypicalValue => List);
stablePrimes (Ideal) := (I) -> (
  if isMonomialIdeal(I)==false then error "the argument is not a monomial ideal";
  S := ring I;
  P := symbol P;
  variables := flatten toList entries vars S;
  minprimes := minimalPrimes I;
  stabprimes := minprimes;
  checks := minprimes / (x->flatten entries gens x);
  cc := {};
  for x in checks do (
    cc = append(cc,for i from 0 to #variables-1 list if isSubset({variables#i}, x) then 1 else 0);
  );
  b := {};
  ok := 0;
  for i from 1 to 2^#variables - 1 do (
    b = toList changeBase(i,2) / (d -> (ascii(d))#0-48);
    b = 0*toList(1..#variables-#b) | b;
    ok = 0;
    for c in cc when ok==0 do (
      if b!=c then
        ok=product apply(c,b, (i,j) -> i^j)
      else
        ok=0
    );
    if ok==0 then (
      P = ideal for p in positions(b,i->i==1) list variables#p;
      if not(isMember(true, minprimes / (m->m==P))) and isStablePrime(I,P) then
        stabprimes = append(stabprimes, ideal for p in positions(b,i->i==1) list variables#p)
    )
  );
  stabprimes
);

------------------------------------------------------------------------
-- Find Max^inf(I) when I is a monomial ideal 
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    stableMax
    (stableMax,Ideal)
  Headline
    compute the set of stable primes of a monomial ideal that are maximal with respect to the inclusion.
  Usage
    stableMax I
  Inputs
    I:Ideal
      a monomial ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
  Outputs
    l:List
      all maximal stable primes of $I$
  Description
    Text
      This method computes $\text{Max}^\infty(I)$, the set of the stable primes of $I$ that are maximal with respect to the inclusion.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
      stableMax I
    Example
      S = QQ[a..e];
      I = ideal(a*d,a*e,b*d,c*d,c*e);
      stableMax I
  SeeAlso
    stablePrimes
///);

stableMax = method(TypicalValue => List);
stableMax (Ideal) := (I) -> (
  if isMonomialIdeal(I)==false then error "the argument is not a monomial ideal";
  stab := stablePrimes(I);
  select(stab, x -> all(stab, p -> if x == p then true else not(isSubset(x,p))))
);

------------------------------------------------------------------------
-- Checking whether a prime ideal is stable for an ideal I 
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    isStablePrime
    (isStablePrime,Ideal,Ideal)
  Headline
    test whether a prime ideal is a stable prime of a ideal $I$
  Usage
    isStablePrime(I,p)
  Inputs
    I:Ideal
      a monomial ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
    p:Ideal
      a prime monomial ideal of $S$
  Outputs
    b:Boolean
      whether $\mathfrak{p}$ is a stable prime of $I$
  Description
    Text
      This method returns {\tt true} if $\mathfrak{p}\in\text{Ass}^\infty(I)$, otherwise {\tt false}.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
      p = ideal(x_1,x_2)
      isStablePrime(I,p)
    Example
      S = QQ[a..e];
      I = ideal(a*d,a*e,b*d,c*d,c*e);
      p = ideal(a,c);
      isStablePrime(I,p)
  SeeAlso
    stablePrimes
    stableMax
///);

isStablePrime = method(TypicalValue => Boolean);
isStablePrime (Ideal, Ideal) := (I,P) -> (
  if isMonomialIdeal(I)==false or isMonomialIdeal(P)==false then error "one of the two ideals is not a monomial ideal";
  if isPrime(P)==false then error "the second argument is not a prime ideal";
  S := ring I;
  Ip := localize(I,P);
  if Ip==S then return false;
  R := reesAlgebra(localize(I,P));
  Q := entries mingens P;
  m := #(flatten Q)-1;
  C := (koszul (matrix Q))**R;
  if dim(HH_m C) <= 0 then return false;
  true
);

------------------------------------------------------------------------
-- Compute vNumberP(I,P);
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    vNumberP
    (vNumberP,Ideal,Ideal)
  Headline
    compute the $\text{v}_\mathfrak{p}$-number of homogeneous ideal $I$
  Usage
    vNumberP(I,p)
  Inputs
    I:Ideal
      a homogeneous ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
    p:Ideal
      a associated prime of $I$
  Outputs
    k:ZZ
  Description
    Text
      This method computes the $\text{v}_{\mathfrak{p}}$-number of $I$: $$\text{v}_{\mathfrak{p}}(I)=\min\{\text{deg}(f):f\in S\ \text{is homogeneous, and}\ (I:f)=\mathfrak{p}\}.$$
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
      p = ideal(x_1,x_2)
      vNumberP(I,p)
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^3*x_2+x_3^4,x_1+x_2+x_4,x_3^3);
      p = (ass I)#0;
      vNumberP(I,p)
  SeeAlso
    vNumber
    vFunctionP
    vFunction
///);

vNumberP = method(TypicalValue => ZZ);
vNumberP (Ideal, Ideal) := (I, P) -> (
  if not(isMember(true, (ass(I)/(Q->P==Q)))) then error "the prime ideal is not associated prime";
  XP := ring I;
  B := I;
  primes := select(ass(I), Q -> Q!=P and isSubset(P,Q)==true);
  if primes != {} then (
    XP = product primes;
    B = quotient(I,P + saturate(XP));
  );
  A := quotient(I, P);
  min flatten((flatten entries mingens (A/B)) / degree)
);

------------------------------------------------------------------------
-- Compute vNumber(I);
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    vNumber
    (vNumber,Ideal)
  Headline
    compute the $\text{v}$-number of homogeneous ideal $I$
  Usage
    vNumberP(I,p)
  Inputs
    I:Ideal
      a homogeneous ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
  Outputs
    k:ZZ
  Description
    Text
      This method computes the $\text{v}$-number of $I$: $$\text{v}(I)=\min\{\text{deg}(f):f\in S\ \text{is homogeneous, and}\ (I:f)\in\text{Ass}(I)\}.$$
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3);
      vNumber(I)
    Example
      S = QQ[x_1..x_4];
      I = ideal(x_1^3*x_2+x_3^4,x_1+x_2+x_4,x_3^3);
      vNumber(I)
  SeeAlso
    vNumberP
    vFunctionP
    vFunction
///);

vNumber = method(TypicalValue => ZZ);
vNumber (Ideal) := (I) -> (
  assI := ass(I);
  min(for P in assI list vNumberP(I,P))
);

------------------------------------------------------------------------
-- Compute soc_P(I)
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    soc
    (soc,Ideal,Ideal)
  Headline
    compute the $\text{Soc}_\mathfrak{p}^*(I)$, where $I$ is a monomial ideal
  Usage
    soc(I,p)
  Inputs
    I:Ideal
      a monomial ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
    p:Ideal
       a stable prime of $I$
  Outputs
    M:Module
  Description
    Text
      This method computes the module $\text{Soc}_\mathfrak{p}^*(I)$. Let $\mathcal{R}(I)=\bigoplus_{k\ge0}I^k$ be the Rees algebra of $I$. For the prime $\mathfrak{p}\in\text{Ass}^\infty(I)$, we set $$X_\mathfrak{p}=\begin{cases}S&\text{if}\ \mathfrak{p}\in\text{Max}^\infty(I),\\ \prod\{\mathfrak{q}\in\text{Ass}^\infty(I):\mathfrak{p}\subsetneq\mathfrak{q}\}&\text{otherwise}.\end{cases}$$ Let $\mathcal{R}'=\bigoplus_{k\ge0}I^{k+1}$. We set $$\text{Soc}_\mathfrak{p}^*(I)=\frac{(\mathcal{R}':_{\mathcal{R}(I)}\mathfrak{p}\mathcal{R}(I))}{(\mathcal{R}':_{\mathcal{R}(I)}(\mathfrak{p}+X_\mathfrak{p}^\infty)\mathcal{R}(I))}.$$ Here $X_\mathfrak{p}^\infty=\bigcup_{k\ge0}(\mathfrak{p}:\mathfrak{m}^k)$ is the saturation of $\mathfrak{p}$ with respect to the maximal ideal $\mathfrak{m}=(x_1,\dots,x_n)$. As proved by Conca, for all $k\gg0$ we have $$\text{Soc}_\mathfrak{p}^*(I)_{(*,k)}=\frac{(I^{k+1}:\mathfrak{p})}{I^{k+1}:(\mathfrak{p}+X_\mathfrak{p}^\infty)},$$ and the initial degree of this module is the $\text{v}_\mathfrak{p}$-number of $I^{k+1}$.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
      p = ideal(x_1,x_2)
      soc(I,p)
  SeeAlso
    vFunctionP
    vFunction
///);

soc = method(TypicalValue => Module);
soc (Ideal, Ideal) := (I, P) -> (
  if isMonomialIdeal(I)==false or isMonomialIdeal(P)==false then error "one of the two ideals is not a monomial ideal";
  if not(isMember(true, (stablePrimes(I)/(Q->P==Q)))) then error "the second argument is not a stable prime";
  XP := ring I;
  if not(isMember(true, (stableMax(I)/(Q->P==Q)))) then XP = product(select(stablePrimes(I), Q -> (Q!=P and isSubset(P,Q)==true)));
  f := reesMap(I);
  R := source f/ker f;
  IR := sub(I,R);
  PR := sub(P,R);
  QR := if not(XP===ring I) then sub(XP, R) else ideal(1_R);
  A := quotient(IR, PR);
  B := intersect(quotient(IR, PR), saturate(IR, QR));
  A/B
);

------------------------------------------------------------------------
-- Compute vFunctionP(I, P); return {m,q}
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    vFunctionP
    (vFunctionP,Ideal,Ideal)
  Headline
    compute the $\text{v}_\mathfrak{p}$-function of monomial ideal $I$
  Usage
    vFunctionP(I,p)
  Inputs
    I:Ideal
      a monomial ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
    p:Ideal
      a stable prime of $I$
    control=>ZZ
      can be changed to increase reliability
  Outputs
    l:List
      the first element represents the slope of the line, while the second represents the $y$-intercept
  Description
    Text
      For $k\gg0$, the numerical function $k\mapsto\text{v}_\mathfrak{p}(I^k)$ is a linear function of the form $mk+q$, with $m$ and $q$ integers. This method computes the pair $(m,q)$.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3);
      p = ideal(x_1,x_2);
      vFunctionP(I,p)
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1,x_2)*ideal(x_1,x_3)*ideal(x_2,x_3);
      p = ideal(x_1,x_2,x_3);
      vFunctionP(I,p,control=>3)
  SeeAlso
    vNumber
    vNumberP
    vFunction
///);

vFunctionP = method(TypicalValue => List, Options=>{control=>2});
vFunctionP (Ideal, Ideal) := opts -> (I, P) -> (
  if isPrime(P) == false then error "the second argument is not a prime ideal";
  f := reesMap(I);
  socP := soc(I,P);
  geners := flatten entries mingens socP;
  T := source f;
  S := target f;
  R := ring socP;
  ngens := length flatten entries gens I;
  M := ideal(for i from #(gens S)-1 to #(gens T)-1 list R_i);
  kk := unique(geners / (g->(degree g)#1));
  gensplit := kk / (k->select(geners, g->(degree g)#1==k));
  l := length gensplit;
  J := symbol J;
  for i from 0 to l-2 do (J = M^(kk#(i+1)-kk#i)*ideal(gensplit#i)+ideal(gensplit#(i+1)));
  if l == 1 then J = ideal(gensplit#0);
  k := kk#(l-1);
  vals := {};
  i := 0;
  count := 0;
  C := opts.control;
  while i < 10*C do (
    geners = flatten entries mingens J;
    vals = append(vals, reverse(min(geners/(g->degree g)))+{1,0});
    if i >= 1 and #vals >= 2 then (
      if vals#i#1 - vals#(i-1)#1 == vals#(i-1)#1 - vals#(i-2)#1 then (
        count = count + 1
      ) else (
        count = 0
      )
    );
    if count == C then (
      break;
    );
    J = M*J;
    i = i+1;
  );
  l = length vals -1;
  k = vals#l#0;
  m := vals#l#1 - vals#(l-1)#1;
  q := vals#l#1 - m*k;
  {m,q}
);


------------------------------------------------------------------------
-- Compute vFunction(I); return {m,q}
------------------------------------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    vFunction
    (vFunction,Ideal)
  Headline
    compute the $\text{v}$-function of monomial ideal $I$
  Usage
    vFunction I
  Inputs
    I:Ideal
      a monomial ideal of polynomial ring $S = \mathbb{K}[x_1,\ldots,x_n]$, $\mathbb{K}$ a field
    control=>ZZ
      can be changed to increase reliability
  Outputs
    l:List
      the first element represents the slope of the line, while the second represents the $y$-intercept
  Description
    Text
      For $k\gg0$, the numerical function $k\mapsto\text{v}(I^k)$ is a linear function of the form $mk+q$, with $m=\alpha(I)$ the initial degree of $I$ and $q\ge-1$ an integer. This method computes the pair $(\alpha(I),q)$.
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1*x_2,x_1*x_3,x_2*x_3);
      vFunction I
      vFunction(I,control=>3)
    Example
      S = QQ[x_1..x_3];
      I = ideal(x_1,x_2)*ideal(x_1,x_3)*ideal(x_2,x_3);
      vFunction I
  SeeAlso
    vNumber
    vNumberP
    vFunctionP
///);

vFunction = method(TypicalValue => List, Options=>{control=>2});
vFunction (Ideal)  := opts -> (I) -> (
  C := opts.control;
  alpha := (min(degrees I))#0;
  stabP := stablePrimes(I);
  min(select(stabP / (P -> vFunctionP(I,P,control=>C)), m->m#0==alpha))
);

------------------------------------------------------------------------
-- DOCUMENTATION FOR OPTION
------------------------------------------------------------------------

------------------------------------------
-- control (for vFunctionP and vFunction)
------------------------------------------
mydoc = concatenate(mydoc,///
Node
  Key
    control
    [vFunctionP,control]
    [vFunction,control]
  Headline
    optional argument for vFunctionP and vFunction
  Description
    Text
      By default this argument is set to 2. Whether it is equal to 2 the vFunctionP and vFunction methods terminate after finding three "aligned" values $\text{v}_\mathfrak{p}(I^k),\text{v}_\mathfrak{p}(I^{k+1}),\text{v}_\mathfrak{p}(I^{k+2})$ of the $\text{v}_\mathfrak{p}$-function. That means that $\text{v}_\mathfrak{p}(I^{k+1})-\text{v}_\mathfrak{p}(I^k)=\text{v}_\mathfrak{p}(I^{k+2})-\text{v}_\mathfrak{p}(I^{k+1})$. By increasing its value, the methods terminate after finding $\texttt{control}+1$ aligned values.
  SeeAlso
    vFunctionP
    vFunction
///);

------------------------------------------------------------------------
-- DOCUMENTATION VNumber
------------------------------------------------------------------------
beginDocumentation();
multidoc(mydoc);

------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------

----------------------------
-- Test stablePrimes
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
assert(stablePrimes I == {ideal(x_1,x_2),ideal(x_1,x_3),ideal(x_2,x_3),ideal(x_1,x_2,x_3)})
///

----------------------------
-- Test stablePrimes
----------------------------
TEST ///
S = QQ[a..e]
I = ideal(a*d,a*e,b*d,c*d,c*e)
assert(stablePrimes I == {ideal(c,b,a),ideal(d,c,a),ideal(e,d)})
///

----------------------------
-- Test stableMax
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
assert(stableMax I == {ideal(x_1,x_2,x_3)})
///

----------------------------
-- Test stableMax
----------------------------
TEST ///
S = QQ[a..e]
I = ideal(a*d,a*e,b*d,c*d,c*e)
assert(stableMax I == {ideal(c,b,a),ideal(d,c,a),ideal(e,d)})
///

----------------------------
-- Test isStablePrime
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
p = ideal(x_1,x_2)
assert(isStablePrime(I,p) == true)
///

----------------------------
-- Test isStablePrime
----------------------------
TEST ///
S = QQ[a..e];
I = ideal(a*d,a*e,b*d,c*d,c*e);
p = ideal(a,c);
assert(isStablePrime(I,p) == false)
///

----------------------------
-- Test vNumberP
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
assert(vNumberP(I, ideal(x_1,x_2)) == 1)
///

----------------------------
-- Test vNumberP
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1^3*x_2+x_3^4,x_1+x_2+x_4,x_3^3)
p = (ass I)#0
assert(vNumberP(I,p) == 5)
///

----------------------------
-- Test vNumber
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
assert(vNumber I == 1)
///

----------------------------
-- Test vNumber
----------------------------
TEST ///
S = QQ[x_1..x_4]
I = ideal(x_1^3*x_2+x_3^4,x_1+x_2+x_4,x_3^3)
assert(vNumber I == 5)
///

----------------------------
-- Test vFunctionP
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
assert(vFunctionP(I, ideal(x_1,x_2)) == {2,-1})
///

----------------------------
-- Test vFunctionP
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1,x_2)*ideal(x_1,x_3)*ideal(x_2,x_3)
p = ideal(x_1,x_2,x_3)
assert(vFunctionP(I,p) == {3,-1})
///

----------------------------
-- Test vFunction
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1*x_2,x_1*x_3,x_2*x_3)
assert(vFunction I == {2,-1})
///

----------------------------
-- Test vFunction
----------------------------
TEST ///
S = QQ[x_1..x_3]
I = ideal(x_1,x_2)*ideal(x_1,x_3)*ideal(x_2,x_3)
assert(vFunction I == {3,-1})
///

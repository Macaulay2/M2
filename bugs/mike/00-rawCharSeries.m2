-- Bugs involving rawCharSeries (16 Jan 2013)
-- These are taken from 
--    00-kahle-isPrime.m2  (crashing bug)
--    test/decompose4.m2 (crashing bug)
--    00-bug-radical (gives WRONG answer)

kk = QQ
R = kk[x,y]
I = ideal"3x2-5"
I = ideal"1/2x2-5/3"
debug Core
rawCharSeries raw gens I
-----------------
-- example from: 00-kahle-isPrime.m2
-----------------
--kk = ZZ/32003 -- this one just goes off into never never land, doesn't return
kk = QQ -- this one crashes very early
R = kk[x1,x2,x3,x4,x5,x6, MonomialOrder=>Lex] -- crashes with this ring and QQ
I = ideal(x1^2*x2^2*x5^8*x6^8-x3^4*x4^4,x2^8*x3^2*x4^8*x5^2-x1^4*x6^4,x1^8*x3^8*x4^2*x6^2-x2^4*x5^4)
debug Core
m = gens I
rawCharSeries raw m

-----------------
-- example from: test/decompose4.m2
-----------------
kk = QQ
S = kk[a,b,c,d, MonomialOrder=>Lex]
J = ideal(4*c^2+a*d-4*b*d-4*c*d+3*d^2-48*c+16*d+144,
    2*a*c-4*c^2+3*b*d+4*c*d-3*d^2-12*a-4*b+48*c-16*d-144,
    -b*c^2+(1/4)*b^2*d+(1/2)*b*c*d+c^2*d-(1/2)*b*d^2-(1/2)*c*d^2+(1/4)*d^3+10*b*c-4*b*d-10*c*d-24*b+28*d)
J1 = ideal(J_0, J_1, 4*J_2)
  -- the only difference between J and J1 is the factor of 4 on the last generator of J
debug Core
rawCharSeries raw gens J1 -- works
rawCharSeries raw gens J -- CRASHES

R = QQ[v_1]
f= -153750943071/2*v_1^3-635215450586*v_1^2-3631862913640*v_1+334197355680
g=1898159791*v_1-171912220
-----------------
-- example from: 00-bug-radical
-----------------
-- new attempt to boil it down:
-- Dan, I think that rawCharSeries is giving an incorrect answer here.
-- Could you run this on your machine and see if you have a problem?
restart
errorDepth=0
debug Core
Istring = "ideal((v2-1)*(v3-1),
  (v4+1)*(v7+1),
  (v2-1)*(v5-1)*(v6+1),
  (v2+1)*(v6+1)*(v7-1),
  (v1-1)*(v5-1)*(v7+v4),
  v7*v6*v1+v7*v6+v7*v2*v1+v7*v2+2*v6*v4-v6*v1+v6-v4*v3*v1+v4*v3+2*v4*v2-v4*v1+v4-v3*v1+v3-v2*v1+v2-v1+1,
  (v1-1)*(v3+v2)*(v4+1),
  (v1+1)*(v3-1)*(v5-1)*(v6+1),
  (v1-1)*(v3+1)*(v4+1)*(v6+1),
  (v1-1)*(v3+1)*(v4+1)*(v5-1),
  (v1-1)*(v2-1)*(v5-1)*(v7-1))"
R1 = QQ[v7,v6,v5,v4,v3,v2,v1, MonomialOrder=>Lex]
I1 = value Istring
P1 = ideal(v1+1,v2-1,v4+1,v7-1)
assert((gens I1) % P1 == 0)
assert(degree I1 == 9)
assert(codim I1 == 4)

R2 = ZZ/101[v7,v6,v5,v4,v3,v2,v1, MonomialOrder=>Lex]
I2 = value Istring
P2 = ideal(v1+1,v2-1,v4+1,v7-1)
assert((gens I2) % P2 == 0)
assert(degree I1 == 9)
assert(codim I1 == 4)

C1 = rawCharSeries raw gens I1 -- this is WRONG (seems to me to be wrong, at least)
C2 = rawCharSeries raw gens I2 -- also seems wrong?  But different from C1!
-- there should be 9 codim 4 components, all degree 1.
-- one should be this one:

-- answer for C1, before "contract":
-- note: there are only: 
( ( v3-1, v4+1, v6+1, v1*v2*v5*v7-1*v2*v5*v7-v1*v5*v7+1*v5*v7-v1*v2*v7+1*v2*v7+v1*v7-1*v7-v1*v2*v5+1*v2*v5+v1*v5-1*v5+v1*v2-1*v2-v1+1 ),
 ( v3-1, v4+1, v5-1, v2*v6*v7+1*v6*v7+v2*v7+1*v7-v2*v6-1*v6-v2-1 ),
 ( v3-1, v4+1, v5-1, v6+1, v1*v2*v7+1*v2*v7-v1*v7-1*v7-v1*v2-1*v2+v1+1 ),
 ( v2-1, v4+1, v1*v3*v5*v6+1*v3*v5*v6-v1*v5*v6-1*v5*v6-v1*v3*v6-1*v3*v6+v1*v6+1*v6+v1*v3*v5+1*v3*v5-v1*v5-1*v5-v1*v3-1*v3+v1+1, v1*v5*v7-1*v5*v7-v1*v7+1*v7-v1*v5+1*v5+v1-1 ),
 ( v2-1, v4+1, v5-1, v6+1 ),
 ( v1-1, v2-1, v3*v5-1*v5-v3+1, v6+1, v7+1 ),
 ( v1-1, v2-1, v3-1, v6+1, v7+1 ),
 ( v1-1, v2-1, v3-1, v4+1, v6+1 ),
 ( v1-1, v2-1, v4+1, v3*v5-1*v5-v3+1, v6+1 ),
 ( v2-1, v3+1, v5-1, v6+1, v7+1 ),
 ( v1+1, v2-1, v3+1, v5-1, v6+1, v7+1 ),
 ( v1+1, v2-1, v3+1, v4-1, v6+1, v7+1 ),
 ( v1+1, v2-1, v3+1, v4+1, v6+1, v7-1 ),
 ( v1+1, v2-1, v3+1, v4+1, v5-1, v6+1 ),
 ( v2-1, v3+1, v4+1, v5-1, v6+1 ),
 ( v1+1, v3-1, v4+1, v5-1, v6+1 ),
 ( v1+1, v2-1, v4+1, v6+1, v5*v7-1*v7-v5+1 ),
 ( v1+1, v2-1, v4+1, v5-1, v6+1 ),
 ( v1+1, v2+1, v3-1, v5-1, v6+1, v7+1 ),
 ( v1+1, v2+1, v3-1, v4+1, v5-1 ),
 ( v2-1, v3-1, v4+1, v6+1, v1*v5*v7-1*v5*v7-v1*v7+1*v7-v1*v5+1*v5+v1-1 ),
 ( v2-1, v3-1, v4+1, v5-1, v6+1 ),
 ( v1-1, v2-1, v3+1, v6+1, v7+1 ),
 ( v1-1, v2-1, v3+1, v4+1, v6+1 ),
 ( v1-1, v2-1, v6+1, v7+1 ),
 ( v1-1, v2-1, v4+1, v6+1 ),
 ( v2-1, v3+1, v4-1, v6+1, v7+1 ),
 ( v2-1, v3+1, v4+1, v6+1, v7-1 ),
 ( v1-1, v3-1, v4+1, v6+1, v7-1 ),
 ( v1-1, v2+1, v3-1, v4+1, v6+1, v7-1 ),
 ( v1-1, v2+1, v3-1, v4+1, v5-1, v7-1 ),
 ( v1-1, v2+1, v3-1, v4+1, v5-1, v6-1 ),
 ( v1-1, v3-1, v4-1, v6+1, v7+1 ),
 ( v1-1, v2+1, v3-1, v4-1, v6+1, v7+1 ),
 ( v1-1, v2+1, v3-1, v4-1, v5-1, v7+1 ),
 ( v1-1, v2+1, v3-1, v5-1, v6-1, v7+1 ),
 ( v2+1, v3-1, v4+1, v5-1, v7-1 ),
 ( v2+1, v3-1, v4+1, v5-1, v6-1 ),
 ( v2+1, v3-1, v4-1, v5-1, v6+1, v7+1 ),
 ( v2+1, v3-1, v4+1, v5-1, v1*v6*v7+1*v6*v7-v1*v7-1*v7-v1*v6-1*v6+v1+1 ),
 ( v1-1, v3-1, v2^2*v4-1*v4-v2^2+1, v5-1, v2*v6+1*v6+v2+1, v7+1 ),
 ( v1-1, v2-1, v3-1, v4+1, v7-1 ),
 ( v1-1, v2-1, v4+1, v5-1, v7-1 ),
 ( v1-1, v2-1, v4+1, v5-1, v6+1 ),
 ( v1-1, v3-1, v4-1, v5-1, v6+1, v7+1 ),
 ( v1-1, v3-1, v4+1, v5-1, v7-1 ),
 ( v1-1, v3-1, v4+1, v5-1, v6+1, v7-1 ),
 ( v2-1, v3+1, v4-1, v5-1, v6+1, v7+1 ),
 ( v1+1, v2-1, v3+1, v4+1, v7-1 ),
 ( v1+1, v2-1, v3+1, v4+1, v5-1, v7-1 ),
 ( v2-1, v3+1, v4+1, v5-1, v7-1 ),
 ( v2+1, v3-1, v4+1, v6+1, v7-1 ),
 ( v1+1, v2+1, v3-1, v4+1, v6+1, v7-1 ),
 ( v2+1, v3-1, v4+1, v5*v6-1*v6+v5-1, v7-1 ) )

- after contract:













R = QQ[x2, x7, x6, x1, x4, x3, x5, MonomialOrder=>Lex]
I = ideal(x4*x3-x4-x3+1,
    x2*x1+x2+x1+1,
    x7*x6*x3-x7*x6-x7*x3+x7+x6*x3-x6-x3+1,
    x2*x7*x3+x2*x7+x2*x3+x2-x7*x3-x7-x3-1,
    x2*x6*x5-x2*x6-x2*x5+x2+x6*x1*x5-x6*x1-x1*x5+x1,
    x2*x7*x5+x2*x7+x2*x3*x5+x2*x3+2*x7*x1-x7*x5+x7-x1*x4*x5+x1*x4+2*x1*x3-x1*x5+x1-x4*x5+x4-x3*x5+x3-x5+1,
    x1*x4*x5-x1*x4+x1*x3*x5-x1*x3+x4*x5-x4+x3*x5-x3,
    x7*x6*x4*x5+x7*x6*x4-x7*x6*x5-x7*x6-x7*x4*x5-x7*x4+x7*x5+x7+x6*x4*x5+x6*x4-x6*x5-x6-x4*x5-x4+x5+1,
    x7*x1*x4*x5-x7*x1*x4+x7*x1*x5-x7*x1+x7*x4*x5-x7*x4+x7*x5-x7+x1*x4*x5-x1*x4+x1*x5-x1+x4*x5-x4+x5-1,
    x6*x1*x4*x5-x6*x1*x4+x6*x1*x5-x6*x1+x6*x4*x5-x6*x4+x6*x5-x6-x1*x4*x5+x1*x4-x1*x5+x1-x4*x5+x4-x5+1,
    x2*x6*x3*x5-x2*x6*x3-x2*x6*x5+x2*x6-x2*x3*x5+x2*x3+x2*x5-x2-x6*x3*x5+x6*x3+x6*x5-x6+x3*x5-x3-x5+1)
R1 = QQ[v7,v6,v5,v4,v3,v2,v1, MonomialOrder=>Lex]
I1 = sub(I, vars R1)
C1 = rawCharSeries raw gens I1 -- this is WRONG (seems to me to be wrong, at least)

R2 = ZZ/101[v7,v6,v5,v4,v3,v2,v1, MonomialOrder=>Lex]


I2 = sub(I1, vars R2)
C2 = rawCharSeries raw gens I2 -- 
--rawIdealReorder raw gens I  -- identity, so no reordering is being done
C = rawCharSeries raw gens I -- this is WRONG (seems to me to be wrong, at least)
  -- answer is (1/16/2013)
{*
COD4  x4-1 x1+1 x7+1 x2x6x3x5-x2x6x3-x2x6x5+x2x6-x2x3x5+x2x3+x2x5-x2-x6x3x5+x6x3+x6x5-x6+x3x5-x3-x5+1 
COD4   x4-1 x1+1 x6-1 x2x7x3+x2x7+x2x3+x2-x7x3-x7-x3-1 
  x4-1 x1+1 x6-1 x7+1 x2x3x5+x2x3-x2x5-x2-x3x5-x3+x5+1 
COD4  x3-1 x1+1 x7x6x4x5+x7x6x4-x7x6x5-x7x6-x7x4x5-x7x4+x7x5+x7+x6x4x5+x6x4-x6x5-x6-x4x5-x4+x5+1 x2x6x5-x2x6-x2x5+x2-x6x5+x6+x5-1 
COD4  x3-1 x1+1 x6-1 x7+1 
  x3-1 x4+1 x6-1 x7+1 x2+1 
  x5+1 x4-1 x1+1 x6-1 x7+1 
  x5+1 x3-1 x1+1 x7+1 x2x6-x2-x6+1 
  x5+1 x3+1 x4-1 x6-1 x7+1 x2+1 
  x5+1 x3+1 x4-1 x1+1 x6-1 
  x3-1 x4-1 x1+1 x7+1 x2x6x5-x2x6-x2x5+x2-x6x5+x6+x5-1 
COD4  x5-1 x3-1 x7+1 x2+1 
COD4  x5-1 x3-1 x1+1 x7+1 
  x3-1 x4+1 x1-1 x7+1 x2+1 
  x5-1 x4-1 x1+1 x7+1 x2-1 
  x5-1 x4-1 x1-1 x7+1 x2+1 
  x5-1 x3+1 x4-1 x1-1 x6-1 x2+1 
  x5-1 x3+1 x4-1 x6-1 x7-1 x2+1 
  x3+1 x4-1 x1+1 x6-1 x2-1 
  x3+1 x4-1 x1+1 x6-1 x7-1 
  x3+1 x4-1 x1-1 x6-1 x7+1 x2+1 
  x5-1 x3-1 x4-1 x1+1 x2-1 
  x5-1 x3-1 x1+1 x6-1 x2-1 
  x5+1 x3-1 x4+1 x1+1 x2-1 
  x3-1 x4+1 x1+1 x6-1 x2-1 
*}
C1 = C/(i -> map(R,i))
C2 = for i in C1 list saturate(ideal i, product flatten entries last topCoefficients i)
assert(intersect C2 == I) -- (FAILS) BUG: this should not fail (we think we are sure that I is radical!)
P = ideal (x5 + 1, x3 - 1, x1 + 1, x2 - 1) -- P is a minimal prime of I
assert((gens I) % P == 0)
-- one of the C2 components must (I think!) be P: (or be contained in P: i.e. C2 really should contain
-- the minimal primes of I).
for i in C2 list ((gens i) % P)
assert(# select(C2, i -> isSubset(i, P)) > 0) -- at least one of the primes in the list should contain P (FAILS)
assert(I == intersect decompose I) -- (FAILS)

end

----------------------------------------------------
Notes on IrrCharSeries, in libfac/charset/charset.cc
----------------------------------------------------
The following will cause lots of debugging for the algorithm to be displayed.
  // MES:
  #define CERR std::cerr
  #define IRRCHARSERIESDEBUG 1
  (before including debug.h, in charset.cc)
----------------------------------------------------
some util functions:
  sort(ListCFList):  this is strange: it seems to do the following: take a list [a1,a2,...,an]
    -- [an, (those with <= elements than an), (those with > elements f an)]
    -- this routine doesn't seem to sort the list at all.  It does seem to put the largest length one first.

function itself:
  calls MCharSetN
  in loop:
    irras
    factorps
    adjoin
    adjoinb
    charseta
  contract
  removecontent
  
utility:
adjoin(is, qs, qh) -- makes new sets (qs, i) for i in is.  But only takes those that are not in qh (?is this description right??!)
adjoinb(is,qs,qh,cs) -- adds in (qs,i,cs) for each i in is.
factorps(ps) returns the list of al irred constant factors of ps

irras(cs,ja,reducible) -- irred ascending set?
  int ja: output
  reducible: also output?
  
  irras calls:
    Factorize
    CFFactor
    newfactoras(f, as, &success) returns list
      success seems always to be "1"
      factors f via as.

MCharSetN(PS,Remembern) -- extended char set
  calls: 
    BasicSet
    initialset1
    Prem
    removefactor
    checkok
    
BasicSet(PS) : BS
  takes an asc set, and chooses a subset which is reduced. (Alg 19 in DGP)

Prem(f, CS) : pseudo-remainder of f wrt a basic set

CharSet(PS) : Cset
  Alg 20 in DGP?

charseta(PS) : Cset
  this one calls CharSet, but otherwise looks identical, what is this about?

initalset1

I don't understand factory routine "mapinto"
or libfac routine: (csutil.cc): myfitting

CFFactor(f,1)  (what does this do?)
nopower


BasicSet(PS:List) => List
  -- PS is a list of polynomials
  -- return a basic set which is a subset of PS
basicSet List := (PS) -> (
  if #PS <= 1 
  then PS
  else (
      QS := PS;
      BS := {};
      while #QS > 0 do (
          -- find lowest rank element b in QS.
          --   add it to BS.
          -- if rank == 0: then return a trivial marker
          -- if rank > 0 then 
          --   QS = select the elements of QS whose degree in var=rank is < deg_var(b)
          );
      BS
      )
  )

charSet List := (PS) -> (
    -- returns an "extended characteristic set"
    RS := PS;  -- those needing division
    QS := PS;  
    CS := {};  -- result charac set
    while #RS > 0 do (
        CS := basicSet QS;
        RS := {};
        if CS is not trivial then ( -- i.e. doesn't contain a unit
            D := QS - CS; -- set difference
            RS := for d in D list (
                r := prem(d, CS);
                if r == 0 then continue;
                r
                )
            );
            QS = QS + RS;
        );
    CS
    )

charSetA List := (PS) -> (
    -- ALMOST SAME AS charset, except the call to basicSet is to charSet,
    -- and in the trivial case, the answer is {1}
    )
    
MCharSetN(List, PremForm) := (PS, remembern) -> (
    QS := PS;
    RS := PS;
    CS := {};
    while #RS > 0 do (
        CS := basicSet QS;
        oldCS := CS;
        -- this next line seems not ever to be used
        --remembern.FS1 = remembern.FS1 + set initalset1 CS;
        RS := if CS is not trivial then (
            D = QS - CS;
            for d in D list (
                r = prem(d, CS);
                if r == 0 then continue;
                removefactor(r, remembern)
                )
            )
        else
           return a trivial CS (e.g. {1});
        QS = oldCS + RS;
        );
    CS
    )

removefactor = (r, remembern) -> (
    -- divide r as much as possible by all elements in remembern
    -- also divide r as much as possible by any variable
    -- return (r', remembern'), where r' is the result of division, and remembern' is
    -- the original list, union with any variables that were removed in this way.
    )

removecontent = (CS, remembern) -> (
    -- CS is a list of polynomials
    -- remembern is too
    )

factorps = (factorset) -> (
    -- factors all polynomials in factorset
    -- returning a (unique) list of all monic factors
    )

inital RingElement := (F) -> (
    -- return the "monic" lead coefficient of F
    -- if F = f(x1,...,x(n-1)) x_n^r + lower terms
    --   then return numerator(f/lc(f)), where lc(f) is (I think) the coefficient of the lead monomial, as
    --   an element of the base field.
    )

contractsub = (CS1, CS2) -> (
    -- ?? what does this do??
    )

contract = (charSeries) -> (
    -- charSeries: list of lists of polynomials
    -- returns the same type
    -- ?? what does this do ??
    )

adjoin = () -> null -- ??
adjoinb = () -> null -- ??

irras(List) := (AS) -> (
    -- AS is an ascending set>
    -- is it modified here?
    -- returns what?
    -- returns (ja, reducible)
    nr := 0;
    ind := 1;
    success := -1;
    ja := 0;  -- what is this?
    -- find an element which factors (over base field)
    -- sets ja=index of this element, ind is set to 0, and reducible is set to the element which factors
    -- and qs is set to the factorization of 'elem'.
    for i in AS do (
        nr = nr+1;
        
        );
    -- if nothing found:
    -- check: is AS irreducible?
    --   if yes: it still seems like the factor is added in??
    --   if no:
    --     for each element i of AS:
    --       
    )

newfactoras(RingElement, List, ZZ) -> (
    )

irrCharSeries(List) := (PS) -> (
    -- returns a list of lists of polynomials
    -- each one is an irreducible characteristic set
    -- together they form a char series of PS.
    highestlevel := PS/level//max;
    qhi := {PS}; -- todo list
    ppi := {}; -- what is this?
    qqi := set {}; -- what is this?
    cs := {};
    factorset := {};
    while #qhi > 0 do (
        qhi = sort(qhi); -- what order is this?
        qs := first qhi;
        ppi12 := partition(n -> n >= #qs, ppi);
        qqi = qqi + ppi12#true -- or false?
        -- first time through:ppi = {}
        -- after that: ppi = set ppi12#false + set qs;
        (cs,factorset) = MCharSetN(qs, factorset);
        cs = removecontent(cs,factorset);
        iss := if level(first cs) > 0 
          then (
            (ts, ts2, reducibleelem) := irras cs;
            if ts2 <= 0  
              then ( 
                     -- if cs is irreducible
                     qsi = append(qsi, cs);
                     is := if #cs == highestlevel
                           then factorps factorset;
                           else (initalset1 cs) + factorps factorset;
                     adjoin(is, qs, qqi)
                   )
            else adjoin(factorps factorset, qs, qqi)
            )
          else adjoin(factorps factorset, qs, qqi);
        );
    
    
    )

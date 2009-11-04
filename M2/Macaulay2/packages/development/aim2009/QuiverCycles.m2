newPackage(
     "QuiverCycles",
     Version => "0.1",
     Date => "November 3, 2009",
     Authors => { {Name => "Anders Buch"} },
     Headline => "quiver cycles",
     DebuggingMode => false
     )

export {
     "quiver",
     "Quiver",
     "vertexOrder",
     "cycleIdeal",
     "cycleClass",
     "classLeadingTerm",
     "quiverCoefficients"
     }

Quiver = new Type of HashTable;

quiver = (n, tailList, headList) -> new Quiver from 
  {"numVertices" => n, "tailList" => tailList, "headList" => headList};

vertexOrder0 = (Q) ->
-- Q is a Quiver
-- If Q is loop-free then return the list of vertices in head-first order.
(
  n := Q#"numVertices";
  tl := Q#"tailList";
  hl := Q#"headList";
  res := new MutableList from toList(0..n-1);
  vset = set toList(0..n-1);
  i := 0;
  while #vset > 0 do
  (
    heads := vset;
    for a from 0 to #tl-1 do
      if member(hl_a, vset) then
        heads = heads - {tl_a};
    if # heads == 0 then return false;
    res#i = first toList heads;
    vset = vset - {res#i};
    i = i+1;
  );
  new List from res
);
vertexOrder = memoize vertexOrder0;


invPerm = (w) ->
-- w is a permutation, e.g. {3,1,2,0}
(
  n := #w;
  iw := new MutableList from w;
  for i from 0 to n-1 do iw#(w#i) = i;
  toList(iw)
);


cycleIdeal = (Q, dv, A) ->
-- Q is a quiver
-- dv is a dimension vector of length Q#"numVertices"
-- A is a representation of Q of dimension dv.
-- Return ideal of orbit closure defined by A.
(
  n := Q#"numVertices";
  tl := Q#"tailList";
  hl := Q#"headList";
  detidx := (set tl)*(set hl);
  GR0 = ZZ/2003[splice flatten { 
    for v from 0 to n-1 list (symbol gl)_(v,0,0) .. (symbol gl)_(v,dv_v-1,dv_v-1),
    for v in toList(detidx) list (symbol dt)_v
  }];
  Gmat:= for v from 0 to n-1 list
         matrix pack(dv_v, gl_(v,0,0) .. gl_(v,dv_v-1,dv_v-1));
  GR := if #detidx > 0 then
    GR0 / ideal(for v in toList(detidx) list 1 - dt_v * det(Gmat_v))
  else
    GR0;

  ord := vertexOrder(Q);
  if ord === false then ord = toList(0..n-1);
  tdegs := for v from 0 to n-1 list
    for i from 0 to dv_v-1 list
      splice(
      for j in ord list
        if j==v then splice((i:0), 1, (dv_v-1-i:0)) else (dv_j:0)
      );
  Vdegs := flatten flatten(for a from 0 to length(tl)-1 list
    for i from 0 to dv_(hl_a)-1 list
      for j from 0 to dv_(tl_a)-1 list
	tdegs_(tl_a)_j - tdegs_(hl_a)_i
  );

  VR := ZZ/2003[splice(
    for a from 0 to length(tl)-1 list
      (symbol x)_(a,0,0) .. (symbol x)_(a,dv_(hl_a)-1,dv_(tl_a)-1)),
    Degrees=>Vdegs];

  Vmat := for a from 0 to length(tl)-1 list
    matrix pack(dv_(tl_a), 
      x_(a,0,0) .. x_(a,dv_(hl_a)-1,dv_(tl_a)-1));

  Gimat := for v from 0 to n-1 list
  (
    if not member(v, detidx) then
    (
      Gmat_v
    )
    else
    (
      r := dv_v;
      c := exteriorPower(r-1, Gmat_v);
      matrix(for i from 0 to r-1 list
        for j from 0 to r-1 list (-1)^(i+j) * c_(r-1-j,r-1-i) * dt_v
      )
    )
  );

  AA := apply(A, matrix);
  f := map(GR, VR, flatten(flatten(
    for a from 0 to length(tl)-1 list
    (
      m := Gmat_(hl_a) * AA_a * Gimat_(tl_a);
      for i from 0 to dv_(hl_a)-1 list
        for j from 0 to dv_(tl_a)-1 list
          m_(i,j)
  ))));
  kernel(f)
);


cycleClass = (Q, dv, A) ->
-- Q is a Quiver
-- dv is a dimension vector
-- A is a representation of Q of dimension dv
-- Return K-theory class of orbit closure defined by A
(
  I := cycleIdeal(Q, dv, A);
  C = prune res I;
  
  ord := vertexOrder(Q);
  if ord === false then ord = toList(0..Q#"numVertices"-1);
  
  KT := ZZ[splice(for v in ord list (symbol t)_(v,0)..(symbol t)_(v,dv_v-1)),
           MonomialOrder=>Lex, Inverses=>true];
  sum flatten(for j from 0 to length C list
    for wt in degrees(C_j) list (-1)^j * KT_wt)
)


sgroth0 = (la, xx, yy) ->
-- Compute the double stable Grothendieck polynomial for the partition la
-- applied to the two sets of variables xx and yy.
(
  p := #xx;
  q := #yy;
  if #la == 0 or la_0 == 0 then return 1;
  if #la > p and la_p > q then return 0;
  if p == 0 then return sgroth(conj(la), yy, {});
  
  n := #la;
  while n>0 and la_(n-1)==0 do n = n-1;
  inner := flatten{la_(toList(1..n-1)),0};
  outer := if p>n then la_(toList(0..n-1))
    else flatten{la_(toList(0..p-2)),min(la_(p-1),q),la_(toList(p..n-1))};
  
  res := 0;
  mu := outer;
  while mu =!= false do
  (
    rws := sum apply(n, i -> if inner_i<mu_i then 1 else 0);
    wlamu := (sum la) - (sum mu);
    res = res + sgroth(mu, xx_(toList(1..#xx-1)), yy) *
      (sum apply(toList(wlamu..rws+wlamu), 
      k -> (-1)^(k-wlamu)*binomial(rws,k-wlamu)*xx_0^k));
    mu = itrBetween(mu, inner, outer);
  );
  res
);
sgroth = memoize sgroth0;

conj = (la) ->
-- conjugate partition of la.
(
  n := #la;
  if n == 0 then return {};
  splice flatten(
  {(la_(-1) : n), for i from 1 to n-1 list (la_(n-i-1)-la_(n-i)) : n-i })
);

itrBetween = (mu, ta, la) ->
-- mu is a partition between ta and la.
-- return "next" partition between ta and la.
(
  i := #mu;
  while i>0 and mu_(i-1) == ta_(i-1) do i = i-1;
  if i == 0 then return false;
  splice flatten(
  {mu_(toList(0..i-2)), apply(i-1..#mu-1, j -> min(mu_(i-1)-1,la_j))})
);


laurent2ps = (ex, d) ->
-- ex is an element of a polynomial ring with Inverses=>true.
-- d is a positive integer.
-- Replace each variable z in ex with 1/(1-z) and expand the 
-- power series to degree d.
(
  vrs := support(ex);
  if #vrs == 0 then return ex;
  z := vrs_0;
  p := degree(z, ex);
  ex1 := sub(ex, {z => z^-1}) * z^p;
  res := 0;
  while ex1 != 0 do
  (
    rec := sub(ex1, {z => 0});
    if rec != 0 then
    (
      zp := if p <= 0 then 
        sum(apply(toList(0..d), k -> binomial(-p,k)*(-z)^k))
      else
        sum(apply(toList(0..d), k -> binomial(p-1+k,k)*z^k));
      res = res + part(0,d, zp * laurent2ps(rec,d));
    );
    ex1 = (ex1 - rec)*(z^-1);
    p = p-1;
  );
  res
);


classLeadingTerm = (Q, dv, gcls, d) ->
-- Q is a Quiver without oriented cycles
-- dv is a dimension vector
-- gcls is a K-theory class, e.g. returned by cycleClass()
-- d is a degree.
-- Return leading quiver coefficient if any exist of degree <= d.
(
  ord := vertexOrder(Q);
  if ord === false then error "quiver contains oriented cycles";
  xcls := laurent2ps(gcls, d);
  if xcls == 0 then return false;
  k := 0;
  while part(k, xcls) == 0 do k = k+1;
  xcls = part(k, xcls);
  m := first exponents leadMonomial(xcls);
  if sum m > d then return false;
  j := 0;
  lst := for i in ord list
  (
    j1 := j + dv_i;
    while j1 > j and m_(j1-1) == 0 do j1 = j1-1;
    p := m_(toList(j..j1-1));
    j = j + dv_i;
    p
  );
  {leadCoefficient xcls, lst_(invPerm(ord))}
)

quiverCoefficients = (Q, dv, gcls) ->
-- Q is a Quiver without oriented cycles
-- dv is a dimension vector
-- gcls is a K-theory class, e.g. returned by cycleClass()
-- Return the quiver coefficients associated to gcls.
-- This function terminates only if gcls can be expressed with finitely 
-- many quiver coefficients.  This is conjectured to be true for all 
-- quiver cycle classes.
(
  if vertexOrder(Q) === false then error "quiver contains oriented cycles";
  n := Q#"numVertices";
  tl := Q#"tailList";
  hl := Q#"headList";
  headVars := apply(toList(0..n-1), v -> 
    apply(toList(0..dv_v-1), i -> 1-(symbol t_(v,i))_(ring(gcls))^-1));
  invVars := apply(toList(0..n-1), v -> 
    apply(toList(0..dv_v-1), i -> 1-(symbol t_(v,i))_(ring(gcls))));
  tailVars := apply(toList(0..n-1), v ->
    flatten(apply(toList(0..#(tl)-1), u -> 
    if hl_u == v then invVars_(tl_u) else {})));

  r1 := gcls;
  res := new MutableList;
  d := 1;
  while r1 != 0 do
  (
    lt := classLeadingTerm(Q, dv, r1, d);
    while lt === false do
    (
      print concatenate("---- (", toString(d), ")");
      d = d+1;
      lt = classLeadingTerm(Q, dv, r1, d);
    );
    print (lt_1 => lt_0);
    
    r1 = r1 - lt_0 * product(apply(toList(0..n-1), v ->
      sgroth(lt_1_v, headVars_v, tailVars_v)));
    res#(#res) = (lt_1 => lt_0);
  );
  hashTable(res)
);


beginDocumentation()
document {
  Key => "QuiverCycles",
  Headline => "quiver cycles",
  EM "QuiverCycles", " is a package for computing classes and quiver coefficients of quiver cycles."
  }

TEST ///

loadPackage "QuiverCycles"
A2R = quiver(2, {0}, {1});
A2L = quiver(2, {1}, {0});
dv23 = {2, 3};
dv32 = {3, 2};
rep23 = {{{1,0,0},{0,0,0}}};
rep32 = {{{1,0},{0,0},{0,0}}};
assert(quiverCoefficients(A2R,dv23,cycleClass(A2R,dv23,rep32)) ===
  hashTable{{{},{1,1}} => 1})
assert(quiverCoefficients(A2R,dv32,cycleClass(A2R,dv32,rep23)) ===
  hashTable{{{},{2}} => 1})
assert(quiverCoefficients(A2L,dv23,cycleClass(A2L,dv23,rep23)) ===
  hashTable{{{2},{}} => 1})
assert(quiverCoefficients(A2L,dv32,cycleClass(A2L,dv32,rep32)) ===
  hashTable{{{1,1},{}} => 1})
A3R = quiver(3, {0,1}, {1,2});
dv232 = {2, 3, 2};
rep = { {{1,0},{0,0},{0,0}}, {{1,0,0},{0,0,0}} };
assert(quiverCoefficients(A3R,dv232,cycleClass(A3R,dv232,rep)) ===
  hashTable{{{},{1,1},{2}} => 1})
rep = { {{1,0},{0,0},{0,0}}, {{0,0,0},{0,0,1}} };
assert(quiverCoefficients(A3R,dv232,cycleClass(A3R,dv232,rep)) ===
  hashTable{{{},{1,1},{2,1}} => 1, {{},{2,1},{2,1}} => -1, {{},{2,1},{2}} => 1})
A3i = quiver(3, {0,2}, {1,1});
dv233 = {2, 3, 3};
rep = {{{1,0},{0,0},{0,0}}, {{1,0,0},{0,1,0},{0,0,0}}};
assert(quiverCoefficients(A3i,dv233,cycleClass(A3i,dv233,rep)) ===
  hashTable{{{},{3,1},{}} => 1, {{},{3,1},{1}} => -1, {{},{3},{1}} => 1})
Qloop = quiver(2, {0,1}, {1,0});
dv22 = {2, 2};
rep  = {{{1,0},{0,0}}, {{1,0},{0,0}}};
assert(cycleClass(Qloop,dv22,rep) == 0)

///


ToricCycle = new Type of HashTable
ToricCycle.synonym = "toric cycle"
debug Core --- kludge to access "hasAttribute" and getAttribute

------------------------------------------------------------------------------
-- Expression
------------------------------------------------------------------------------

expression ToricCycle := Expression => C ->(
  X := variety C;
  divisorSymbol :=
    if hasAttribute(X,ReverseDictionary) then
      expression toString getAttribute(X,ReverseDictionary)
    else
      expression "V";
  S := support C;
  if S == {} then
    return expression 0;
  Sum apply(S, j ->(
    coeff := expression abs(C#j);
    if C#j == -1 then
      Minus Subscript{divisorSymbol, j}
    else if C#j < 0 then
      Minus {coeff * Subscript{divisorSymbol, j}}
    else if C#j == 1 then
      Subscript{divisorSymbol, j}
    else coeff * Subscript{divisorSymbol, j}
    )
  )
);

net ToricCycle := C -> net expression C
ToricCycle#{Standard,AfterPrint} =
ToricCycle#{Standard,AfterNoPrint} = C ->(
    << endl; -- double space
    << concatenate(interpreterDepth:"o") << lineNumber << " : ToricCycle on ";
    << variety C << endl;
);

------------------------------------------------------------------------------
-- Methods and constructors
------------------------------------------------------------------------------

variety ToricCycle := C -> C.variety;

support ToricCycle := C -> C.cycleSupport;

toricCycle = method(TypicalValue => ToricCycle)
toricCycle (List, List, NormalToricVariety) := (L,S,X) ->(
  new ToricCycle from L | {
    symbol variety => X,
    symbol cycleSupport => S,
    symbol cache => new CacheTable
  }
);
toricCycle (List, NormalToricVariety) := (L,X) ->(
  S := new List;
  for t in L do (
    if t#1 != 0 then(
      S = S | {t#0};
    ) else(
      continue
    );
  );
  toricCycle(L,S,X)
);

ToricCycle _ List := ToricCycle => (D,L) ->(
  if member(L,keys D) then(
    return D#L
  ) else(
    return 0
  )
);

ToricCycle _ ZZ := ToricCycle => (D,i) -> D_{i};

NormalToricVariety _ List := (X,L) ->(
  S := {sort L};
  toricCycle({S_0 => 1}, S, X)
);

toricCycle(ToricDivisor) := D ->(
    X := variety D;
    L := for i from 0 to #rays X - 1 list ( {i}, (entries D)#i );
    toricCycle(L,X)
);

toricCycle(ToricCycle) := D -> D;

ToricCycle == ToricCycle := Boolean => (D,E) ->(
    return variety D === variety E and (for orbit in sort support(D) list D#orbit)==(for orbit in sort support(E) list E#orbit);
);

toricDivisorFromCycle = method(TypicalValue => ToricDivisor)
toricDivisorFromCycle(ToricCycle) := ToricDivisor => (D) ->(
  X := D.variety;
  L := apply(#(rays X), i -> D_i);
  return toricDivisor(L,X)
);

------------------------------------------------------------------------------
-- Addition and scalar multiplication
------------------------------------------------------------------------------

QQ * ToricCycle := ToricCycle => (k,C) ->(
    if k == 0 then(
        return toricCycle({},variety C)
    );
    L := apply(support C, s->(s => k*C_s) );
    toricCycle(L, support C, variety C)
);
ZZ * ToricCycle := ToricCycle => (k,C) ->( promote(k,QQ) * C );

-- Extend the multiplication of toric divisors
QQ * ToricDivisor := ToricDivisor => (q,D) ->(
    X := variety D;
    toricDivisor (apply (# rays X, i -> q*D#i), X)
);

ToricCycle + ToricCycle := ToricCycle => (C,D) ->(
    --assert(variety C === variety D);
    U := toList(set support(C) + set support(D));
    S := new List;
    L := new List;
    c := 0;
    for u in U do(
      c = C_u + D_u;
      if not(c == 0) then(
        L = L | {u => c};
        S = S | {u};
      ) else(
        continue
      );
    );
    toricCycle(L,S,variety C)
);

ToricCycle - ToricCycle := ToricCycle => (C,D) ->(C + (-1)*D);

- ToricCycle := ToricCycle => (C) ->(-1)*C

ToricDivisor + ToricCycle := ToricCycle => (D,C) ->(toricCycle(D) + C);
ToricDivisor - ToricCycle := ToricCycle => (D,C) ->(toricCycle(D) - C);
ToricCycle + ToricDivisor := ToricCycle => (C,D) ->(D+C);
ToricCycle - ToricDivisor := ToricCycle => (C,D) ->(-(D-C));

------------------------------------------------------------------------------
-- Intersection product
------------------------------------------------------------------------------

isTransverse = method(TypicalValue => Boolean)
isTransverse(ToricDivisor, List) := (D,E) ->(
    -- the first set is support D
    #( set( select( #(rays variety D), i -> D#i != 0) ) * set(E) ) == 0
);

--input: toric divisor D and a list C
--output: a toric divisor D' linearly equivalent to D such that its support and C are disjoint
makeTransverse = method(TypicalValue => ToricDivisor)
makeTransverse (ToricDivisor, List) := (D,C) ->(
  X := variety D;
  d := #(rays X); -- number of rays of X
  n := dim X;

  if not(X.cache#?PicardBases) then(
    X.cache#(symbol PicardBases) = {};
    X.cache#(symbol RaysMatrix) = transpose matrix rays X;
    X.cache#(symbol RaysMatrixRank) = rank (X.cache#RaysMatrix);
  );

  M := X.cache#RaysMatrix;
  r := X.cache#RaysMatrixRank;

  l := new List;
  LL := X.cache#PicardBases; --list of tuples (l,M)
  --where l index a basis of the Picard group of X
  --and M is a matrix whose rows are the coefficients of sequations for the classes of divisors
  k := position(LL, T -> isSubset(set C, set T#0));
  if instance(k,ZZ) then(
    M = (LL_k)#1;
    l = (LL_k)#0;
  ) else(
    L := C | toList (set(0..(d-1)) - C); --find a basis that contains C
    l = ( maxCol M_L )_1; --order the rays so that the ones indexed by C are first, then maxCol finds a maximal minor containing C
    l = L_l; --reorder the indices

    if r == n then ( --if M_l is a square matrix
      --M = inverse(promote(M_{0..numRows M-1},QQ)) * M;  -- Gaussian elimination
      M = entries (inverse(promote(M_l,QQ)) * M);  -- Gaussian elimination
    )
    else (
      L = l | toList (set(0..(d-1)) - l); -- reorder ray indices to put l first
      M = M_L;  -- reorder rays
      l' := ( maxCol transpose M_(toList(0..r-1)) )_1; --find a nonzero minor
      L' := l' | toList (set(0..(n-1)) - l'); -- reorder rows indices to put l' first
      M = M^L'; -- reorder rows
      M' := inverse promote(M_(toList(0..r-1))^(toList(0..r-1)),QQ);
      Omega := matrix toList(n-r:toList(r:0));
      M' = transpose ( transpose(M' | transpose Omega) | transpose(Omega | id_(ZZ^(n-r)) ) );
      M = M' * M;
      L'' := apply(#L, i->position(L,j->j==i));
      M = entries (M_L'');
    );
    --cache the result
    X.cache#PicardBases = X.cache#PicardBases | {(l,M)};
  );
  --subtract the equations to D
  D = entries D;
  for i from 0 to r-1 do (
    D = D - (D_(l_i) * M_i);
  );
  toricDivisor(D,X)
);

-- We assume that X has no torus factors and is simplicial
-- input: a divisor D on X, a cone C in the fan of X
-- output: a cycle equivalent to D * X_C
ToricDivisor * List := (D, C) ->(
    -- if not(isQQCartier(D)) then
    --   error "--the divisor is not QQCartier";
    X := variety D;
    i := dim X - #C; -- dimension of V(C)
    if i > 0 and member(C,orbits(X,i)) then (
      highDimCones := (orbits X)#(i-1);
    ) else (
      return toricCycle( {} , X );
    );

    if not isTransverse(D,C) then (
      D = makeTransverse(D,C);
    );

    S := select( #(rays X), i -> D#i != 0); --support D
    V := new List;
    if isSmooth X then(
      V = for r in S list (
          C' := sort(C|{r});
          if member(C',highDimCones) then
              ( C' => D#r )
          else
              continue
      );
    ) else(
      mult := abs(gcd flatten entries gens minors(#C,(transpose matrix rays X)_C)); -- lattice index of C
      V = for r in support D list (
        C' := sort(C|{r});
        if member(C',highDimCones) then (
          mult' := abs(gcd flatten entries gens minors(#C',(transpose matrix rays X)_C')); -- lattice index of C'
          ( C' => ((mult / mult') * D#r) )
        )
        else (
          continue
        )
      );
    );
    return toricCycle(V,X)
);

ToricDivisor * ToricDivisor := (D,E) ->(
    D * toricCycle E
);

ToricDivisor * ToricCycle := (D,C) ->(
    if support C == {} then
      return C;
    sum ( for c in support C list ((C#c) * (D*c)) )
);

ToricCycle * ToricDivisor := (C,D) -> D*C;

ToricCycle * ToricCycle := (C,D) ->(
  --assert(variety C === variety D);
  X := variety D;
  E := toricCycle({},{},X); --resulting cycle
  for d in support D do(
    V := C; --auxiliary cycle
    for i in d do(
      V = X_i * V;
    );
    V = (D#d) * V;
    E = E + V;
  );
  return E;
);

------------------------------------------------------------------------------
-- Degree
------------------------------------------------------------------------------

degCycle = method(TypicalValue => ZZ)
degCycle (ToricCycle) := C ->(
  X := variety C;
  if not(support C == {}) and #(max X)_0 == #(support C)_0 then(
      return sum apply(support C, c -> C#c);
  ) else(
    return 0;
  );
);

newPackage(
     "SymmetricFunctions",
     version => "0.1",
     Date => "October 30, 2009",
     Authors => { {Name => "Anders Buch"} },
     Headline => "symmetric functions",
     DebuggingMode => false
     )

export {
     SymmetricFunction,
     symmetricFunction
     }

SymmetricFunction = new Type of HashTable;

symmetricFunction = (lst) -> new SymmetricFunction from lst;

SymmetricFunction + SymmetricFunction := (f1,f2) ->
  select(merge(f1,f2, plus), la -> (la != 0));

ZZ * SymmetricFunction := (a,f) -> act(a_(ring h_1), f);

SymmetricFunction * ZZ := (f,a) -> act(a_(ring h_1), f);

SymmetricFunction * SymmetricFunction := (f1,f2) -> act(giambelli(f1),f2);

SymmetricFunction - SymmetricFunction := (f1,f2) -> f1 + (-1)*f2;

SymmetricFunction ^ ZZ := (f,n) ->
  ( g := f; for i from 2 to n do g = f*g; g );

net SymmetricFunction := f -> (net toExpression(f));

SymmetricFunctionSymbol = new Type of Symbol;
s = new SymmetricFunctionSymbol from symbol s;
SymmetricFunctionSymbol _ ZZ := (ss,i) ->
  symmetricFunction {{i}=>1};
SymmetricFunctionSymbol _ List := (ss,la) -> 
  symmetricFunction {la=>1};
SymmetricFunctionSymbol _ Sequence := (ss,la) -> 
  symmetricFunction {toList(la)=>1};
SymmetricFunctionSymbol List := (ss,la) ->
  symmetricFunction {la=>1};
SymmetricFunctionSymbol Array := (ss,la) ->
  symmetricFunction {la=>1};

toExpression = (f) ->
(
  Sum apply(keys f, la ->
  (
    c := f#la;
    t := (hold s)_(if #la==1 then la_0 else toSequence(la));
    if abs(c)>1 then t = Product(abs(c),t);
    if c<0 then t = Minus(t);
    t
  ))
);

pieriSet0 = (i, la, cols) ->
(
  if i==0 then
    return {la}
  else if #la == 0 then
    return {{i}};
  flatten(apply(toList(max(la_0,i)..min(la_0+i,cols)), j ->
    apply(pieriSet(i-j+la_0,la_(toList(1..#la-1)),la_0), x->flatten({j,x})))
  )
);
pieriSet = memoize pieriSet0;

pieri = (i, lc) ->
(
  res := new SymmetricFunction;
  scanKeys(lc, la ->
    res = merge(res, symmetricFunction(apply(pieriSet(i,la,infinity),
    mu->(mu=>lc#la))), plus)
  );
  select(res, x -> (x != 0))
);

giambelliPart0 = (la) ->
(
  if #la == 0 then
    return 1
  else if #la == 1 then
    return h_(la_0);
  i  := la_0;
  mu := la_(toList(1..#la-1));
  h_i*giambelliPart(mu) -
    sum(apply(pieriSet(i,mu,infinity) - set {la}, giambelliPart))
);

numHVars = 0;
setNumHVars := (n) ->
(
  if n <= numHVars then return;
  numHVars = n;
  ZZ[h_1..h_n];
  giambelliPart = memoize giambelliPart0;
);
setNumHVars(10);

giambelli = (lc) ->
(
  setNumHVars(max(apply(keys lc, sum)));
  res := 0;
  scanKeys(lc, la -> (
    res = res + lc#la * giambelliPart(la)
  ));
  res_(ring h_1)
);

act = (ex, lc) ->
(
  if ex == 0 then return new SymmetricFunction;
  vrs := support ex;
  if #vrs == 0 then
  (
    c := leadCoefficient(ex);
    return symmetricFunction(for la in keys(lc) list (la => c * lc#la));
  );
  i := (toList(baseName(vrs_0)))_1;
  ex0 := sub(ex, {h_i => 0});
  ex1 := (ex - ex0) // h_i;
  select(merge(pieri(i, act(ex1, lc)), act(ex0, lc), plus), x -> (x != 0))
);


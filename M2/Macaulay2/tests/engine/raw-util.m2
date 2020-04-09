--status: this old test depends on internal things and probably should be deleted


-- Routines useful for testing the raw interface 
-- to the engine.

-- make a degree monoid, with n degrees.
--trivmonoid = rawMonoid();
debug Core

trivring = rawPolynomialRing();
degmonoid = (n) -> (
     mo := rawMonomialOrdering { GroupLex => n };
     t := symbol t;
     varnames := if n === 1 then {"t"} else
        toList apply(1..n, i -> toString(t_i));
     rawMonoid(mo, 
	  toSequence varnames,
	  trivring,
	  {}, {}))

degring1 := rawPolynomialRing(rawZZ(), degmonoid 1)

degring = (n) -> (
     if n === 0 then trivring
     else if n === 1 then degring1
     else rawPolynomialRing(rawZZ(), degmonoid n))

singlemonoid = vars -> (
     vars = toList vars;
     mo := rawMonomialOrdering { GRevLex => apply(#vars, i -> 1) };
     varnames := toSequence apply(vars, toString);
     degs := apply(vars, i -> 1);
     rawMonoid(mo, varnames, degring 1, degs, {1}))

doublemonoid = (vars, degs) -> (
     -- vars should be a sequence or list of variable names
     -- degs should be a single or double nested array of degrees.
     --  the degree vector for the first variable comes first, etc.
     -- 
     )

lex = vars -> (
     mo := rawMonomialOrdering { Lex => #vars };
     varnames := apply(vars, toString);
     degs := apply(vars, i -> 1);
     rawMonoid(mo, varnames, degring 1, degs, {1}))

elim = (vars1,vars2) -> (
     vars := join(vars1,vars2);
     wts1 := vars1/(i -> 1);
     degs := vars/(i -> 1);
     mo := rawMonomialOrdering { Weights => wts1, GRevLex => degs };
     varnames := apply(vars, toString);
     rawMonoid(mo, varnames, degring 1, degs, {1}))

polyring = (K, vars) -> (
     -- each element of vars should be a symbol!
     R := rawPolynomialRing(K, singlemonoid vars);
     scan(#vars, i -> vars#i <- rawRingVar(R,i));
     R)

polyring2 = (K, vars, mo) -> (
     -- each element of vars should be a symbol!
     M := rawMonoid(mo, apply(vars, toString), 
	       degring 1, (#vars):1, {1});
     R := rawPolynomialRing(K, M);
     scan(#vars, i -> vars#i <- rawRingVar(R,i));
     R)

polyring3 = (K, vars, mo, degs) -> (
     degs = toList flatten splice degs;
     ndegs := (#degs)//(#vars);
     M := rawMonoid(mo, apply(vars, toString), 
	       degring ndegs, degs, toList(ndegs:0));
     R := rawPolynomialRing(K, M);
     scan(#vars, i -> vars#i <- rawRingVar(R,i));
     R)

mat = (tab) -> (
     -- tab should be a non-empty table of ring elements
     R := rawRing tab#0#0;
     nrows := #tab;
     ncols := #tab#0;
     F := rawFreeModule(R,nrows);
     result := rawMatrix1(F,ncols,toSequence flatten tab,0);
     result
     )

rawbettimat = (C,typ) -> (
     w := rawGBBetti(C,typ);
     w1 := drop(w,3);
     matrix pack(w1,w#2+1)
     )

rawgb = (m) -> (
  Gcomp = rawGB(m,false,0,{},false,0,0,0,10);
  rawStartComputation Gcomp;
  rawGBGetMatrix Gcomp)

rawsyz = (m) -> (
  Gcomp = rawGB(m,true,-1,{},false,0,0,0,10);
  rawStartComputation Gcomp;
  rawGBSyzygies Gcomp)

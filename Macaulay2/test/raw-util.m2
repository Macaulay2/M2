-- Routines useful for testing the raw interface 
-- to the engine.

-- make a degree monoid, with n degrees.
trivmonoid = rawMonoid();
degmonoid = (n) -> (
     mo := rawMonomialOrdering { GroupLex => n };
     varnames := if n === 1 then {"t"} else
        toList apply(1..n, i -> toString(t_i));
     rawMonoid(mo, 
	  varnames,
	  trivmonoid,
	  {}))

singlemonoid = (vars) -> (
     mo := rawMonomialOrdering { GRevLex => apply(#vars, i -> 1) };
     varnames := apply(vars, toString);
     degs := apply(vars, i -> 1);
     rawMonoid(mo, varnames, degmonoid 1, degs))

lex = (vars) -> (
     mo := rawMonomialOrdering { Lex => #vars };
     varnames := apply(vars, toString);
     degs := apply(vars, i -> 1);
     rawMonoid(mo, varnames, degmonoid 1, degs))

elim = (vars1,vars2) -> (
     vars := join(vars1,vars2);
     wts1 := vars1/(i -> 1);
     degs := vars/(i -> 1);
     mo := rawMonomialOrdering { Weights => wts1, GRevLex => degs };
     varnames := apply(vars, toString);
     rawMonoid(mo, varnames, degmonoid 1, degs))

polyring = (K, vars) -> (
     -- each element of vars should be a symbol!
     R := rawPolynomialRing(K, singlemonoid vars);
     scan(#vars, i -> assign(vars#i, rawRingVar(R,i,1)));
     R)

mat = (tab) -> (
     -- tab should be a table of ring elements
     R := rawRing tab#0#0;
     nrows := #tab;
     ncols := #tab#0;
     F := rawFreeModule(R,nrows);
     result := rawMatrix(F,
	  toSequence apply(ncols, c -> (
		     sum apply(nrows, r -> rawTerm(F, tab#r#c, r)))));
     result
     )

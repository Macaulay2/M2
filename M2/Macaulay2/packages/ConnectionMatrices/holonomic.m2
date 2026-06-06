-- modified version of holonomic rank to keep the standard basis
-- see Lemma 1.4.11 in SST
holonomicRank(List, Module) := (w, M) -> (
     W := ring M;
     createDpairs W;
     n := #(W.dpairInds#0);
     m := numgens W;
     presM := presentation M;
     -- get weight vectors for the order filtration
     -- refined by lex on the derivatives
     --w := { apply(m, i -> if member(i, W.dpairInds#1) then 1 else 0 ) };
     -- ring equipped with the new order
     tempW := (coefficientRing W)(monoid [W_*,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra,
	  MonomialOrder => WeightThenEliminationOrder w]);
     WtotempW := map (tempW, W, vars tempW);
     -- commutative ring of derivative variables
     Rvars := symbol Rvars;
     R := (coefficientRing W)(monoid [Rvars_0..Rvars_(n-1)]);
     newInds := inversePermutation join(W.dpairInds#1, W.dpairInds#0);
     matList := apply(m, i -> if newInds#i < n then R_(newInds#i) else 1_R );
     tempWtoR := map (R, tempW, matrix{ matList });
     -- computing GB with respect to new order
     ltM := leadTerm gens gb WtotempW presM;
     -- compute the rank
     redI := cokernel tempWtoR ltM;
     -- Note: we don't compute dim redI, because coefficientRing W might nonzero dimension
     B := try M.cache#"basis" = sub(cover basis(redI, Variables => gens R), matrix {W.dpairVars#1});
     if 0 == redI  then 0 else
     if B === null then error "expected ideal with finite holonomic rank"
     else numgens source B
)
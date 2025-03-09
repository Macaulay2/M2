----------------------------------------------------
--gaugeMatrix computes change of basis matrix
----------------------------------------------------
gaugeMatrix = method();
-- G old Groebner basis, old standard monomials, new standard monomials
gaugeMatrix(List, List, List) := (G, stdMon, newStdMon) -> (
  D := ring G#0;
  -- obtain weight ordering from D
  w := (((options(D)).MonomialOrder)#1)#1;
  gaugeMat := mutableMatrix map((fractionField D)^(length newStdMon), (fractionField D)^(length stdMon),0);
  -- rows are indexed by new std monomials
  for rowIndex from 0 to length(newStdMon)-1 do
  (
    -- compute normalForm of new std monomials wrt. gb G
    reducedWRTG := normalForm(sub(newStdMon#rowIndex,D),G);
    coeffsReduced := coefficients reducedWRTG;
    for j from 0 to length(flatten entries coeffsReduced_0)-1 do
    --runs through (d-)monomial support of the reduced element
    (
      -- sorts matrix according to the orderd std monomial basis
      monomialToFind := sub((flatten entries coeffsReduced_0)#j,D);
      colIndex := position(stdMon, i->sub(i,D) == monomialToFind);
      gaugeMat_(rowIndex,colIndex) = sub((flatten entries coeffsReduced_1)#j, fractionField D);
    );
  );
  matrix gaugeMat
);
needsPackage "MonodromySolver"
setRandomSeed 0
needsPackage "ExampleIdeals"

parametrizedKatsura = n -> (
    S := gens katsura(n,CC);
    R := ring S;
    polys := flatten entries S;
    ind := flatten apply(#polys,i-> -- indices for parameters
    	apply(exponents polys#i, t->(i,t))
    	);
    AR := CC[apply(ind,i->A_i)][gens R];
    polysP := for i to #polys-1 list -- system with parameteric coefficients and same support 
    sum(exponents polys#i, t->A_(i,t)*AR_(t));
    polySystem transpose matrix {polysP}
    )

end ------------------------------------------------
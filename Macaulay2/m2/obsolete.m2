--		Copyright 1997-2002 by Daniel R. Grayson

-- old internal engine routines:
scan({sendgg,ggPush,ConvertJoin,ConvertRepeat,ConvertApply,newHandle},
     s -> s <- X -> error ("'", s, "' has been removed"))

unlist = X -> error "'unlist' has been replaced by toSequence"
elements = X -> error "'elements' has been replace by toList"
evaluate = X -> error "'evaluate' has been replaced by 'value'"
seq = X -> error "'seq' has been replaced by 'singleton'"
verticalJoin = X -> error "'verticalJoin' has been replaced by 'stack'"
netRows = X -> error "netRows' has been replaced by unstack'"
-- name = X -> error "'name' has been replaced by 'toString'"
quote = X -> error "'quote' has been replaced by 'symbol'"
Numeric = X -> error "'Numeric' has been replaced by 'numeric'"
submodule = X -> error "'submodule' has been removed"
stats = X -> error "'stats' has been replaced by 'summary'"
monomialCurve = X -> error "'monomialCurve' has been replaced by 'monomialCurveIdeal'"
assign = X -> error "assign' has been replaced by <-'"

undocumented(map,Ideal)
undocumented(map,Ideal,Ideal)
map(Ideal) := Matrix => options -> (I) -> (
     stderr << "warning: map(Ideal) has been deprecated: use 'map(module I, 1)' instead" << endl;
     map(module I,options)
     )
map(Ideal,Ideal) := Matrix => options -> (I,J) -> (
  stderr << "warning: map(Ideal,Ideal) has been deprecated: use 'map(module I,module J)' instead" << endl;
  map(module I,module J,options)
  )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

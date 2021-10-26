-- I think this test is now obsolete, because of this line
--  	  if not instance(opts.TypicalValue,Type) then error("expected typical value ", toString opts.TypicalValue, " to be a type");
-- in methods.m2

a = select(pairs typicalValues, (k,t) -> not instance(t,Type))
if #a > 0 then (
     stderr << "typical values that are not types:" << endl;
     a / ((k,t) -> stderr << (k => t) << endl);
     assert(#a == 0)
     )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test typicalval.out"
-- End:

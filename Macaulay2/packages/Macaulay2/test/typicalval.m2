a = select(pairs typicalValues, (k,t) -> not instance(t,Type))
if #a > 0 then (
     stderr << "typical values which are not types:" << endl;
     a / ((k,t) -> stderr << (k => t) << endl);
     assert(#a == 0)
     )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test typicalval.out"
-- End:

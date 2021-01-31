-- errorDepth 0
R = QQ[x]
f = map(R^1,R^1)
assert(
     try (
     	  HH_f(f); -- we need to make this not work, eventually (low priority)
     	  false					    -- we got no error message, sigh
	  )
     else true
     )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test HH.out"
-- End:

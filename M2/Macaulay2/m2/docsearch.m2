-- about - a function for searching through all documentation in all installed packages

about = method(Options => {SearchBody => false})	    -- exported


lastabout = null

about String := o -> re -> lastabout = (
     NumberedVerticalList flatten for p in installedPackages(IncludeCore=>true, Database=>true) list (
     pkg := p#0;
     dbname := p#1;
     if not fileExists dbname then continue;
     db := openDatabase dbname;
     pkgd := pkg | "::";
     kys := select(keys db,
	  if o.SearchBody
	  then key -> match(re,key) or match(re,db#key)
	  else key -> match(re,key));
     close db;
     apply(kys, key -> pkgd | key)))
about Function := 
about Type := 
about Symbol := o -> s -> about toString s

help#0 ZZ := i -> (
     if lastabout === null then error "no previous 'about' response";
     if not lastabout#?i then error ("previous 'about' response contains no entry numbered ", i);
     help lastabout#i)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

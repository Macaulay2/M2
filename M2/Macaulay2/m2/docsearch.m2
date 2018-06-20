-- about - a function for searching through all documentation in all installed packages on the prefixPath

about = method(Options => {SearchBody => false})	    -- exported

lastabout = null

about String := o -> re -> lastabout = (
     NumberedVerticalList sort flatten for p in getPackageInfoList() list (
	  pkgname := p#"name";
	  dbname := p#"doc db file name";
	  dbkeys := keys p#"doc keys";
	  local kys;
	  if o.SearchBody then (
	       db := openDatabase dbname;
	       kys = select(dbkeys, key -> match(re,key) or match(re,db#key));
	       close db;
	       )
	  else (
	       kys = select(dbkeys, key -> match(re,key))
	       );
	  x := pkgname | "::";
	  apply(kys, key -> x | key)))
about Function := 
about Type := 
about Symbol := o -> s -> about("\\b" | toString s | "\\b", o)

help#0 ZZ := i -> (
     if lastabout === null then error "no previous 'about' response";
     if not lastabout#?i then error ("previous 'about' response contains no entry numbered ", i);
     help lastabout#i)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

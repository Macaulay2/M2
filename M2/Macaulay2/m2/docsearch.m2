-- about - a function for searching through all documentation in all installed packages on the prefixPath

about = method(Options => {Body => false})	    -- exported

lastabout = null

about String := o -> re -> lastabout = (
     packagesSeen := new MutableHashTable;
     matchfun := db -> (
	  if db === null
	  then (key -> match(re,key))	    -- not quite right, because the body might assert that the key is undocumented.  We need a quicker way to identify undocumented keys.
	  else if instance(db,Database) 
	  then (key -> (match(re,key) or match(re,db#key))
	       and not match(///"undocumented" => true///,db#key) -- not quite right, because this string might occur in the raw documentation as part of the description.  Unlikely, though.
	       )
	  else if instance(db,HashTable)
	  then (key -> not db#key#?"undocumented" and (match(re,key) or db#key.?Description and match(re,toString db#key.Description))));
     NumberedVerticalList sort join(
	  flatten for p in loadedPackages list (
	       pkgname := p#"pkgname";
	       x := pkgname | "::";
	       if packagesSeen#?pkgname then continue else packagesSeen#pkgname = 1;
	       kys := join (
		    if p#?"raw documentation database" 
		    then select(keys p#"raw documentation database", matchfun if o.Body then p#"raw documentation database")
		    else {},
		    select(keys p#"raw documentation", matchfun if o.Body then p#"raw documentation"));
	       apply(kys, key -> x | key)),
	  flatten for p in getPackageInfoList() list (
	       pkgname := p#"name";
	       x := pkgname | "::";
	       if packagesSeen#?pkgname then continue else packagesSeen#pkgname = 1;
	       dbname := p#"doc db file name";
	       dbkeys := keys fetchDocKeys p;
	       if o.Body then db := openDatabase dbname;
	       kys := select(dbkeys, matchfun db);
	       if o.Body then close db;
	       apply(kys, key -> x | key))))
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

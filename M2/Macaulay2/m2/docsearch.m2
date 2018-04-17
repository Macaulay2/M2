-- about - a function for searching through all documentation in all installed packages

about = method ( Options => { } )

findSomeDocDatabases = layoutpath -> (
     (pth,stem) := toSequence (lines ( replace("PKG","\n",layoutpath )));
     if not isDirectory pth then return {};
     pkgnames := select(readDirectory pth, name -> not match ("^\\.", name));
     r := apply(pkgnames, pkg -> (pkg, pth|pkg|stem));
     r = select(r, (pkg,pth) -> fileExists pth);
     r)

findAllDocDatabases = () -> (
     join (
     	  findSomeDocDatabases ( applicationDirectory()|"local/"|Layout#1#"packagecache"|"rawdocumentation"|databaseSuffix),
	  findSomeDocDatabases ( prefixDirectory | currentLayout#"packagecache" | "rawdocumentation" | databaseSuffix )))

dbKeys = new MutableHashTable
dbTimes = new MutableHashTable

getkeys = dbname -> (
     db := openDatabase dbname;
     k := keys db;
     dbKeys#dbname = k;
     dbTimes#dbname = fileTime dbname;
     close db;
     k)

getkeys' = dbname -> (
     if dbKeys#?dbname 
     then (
	  if dbTimes#dbname === fileTime dbname 
	  then dbKeys#dbname
	  else getkeys dbname
	  )
     else getkeys dbname)

findAllDocHashtables = () -> (
     apply(unique values PackageDictionary, sym -> (toString sym, (value sym)#"raw documentation"))
     )	  

about Function := 
about Type := 
about Symbol := o -> s -> (				    -- exported
     about toString s
     )

lastabout = null

about String := o -> s -> (				    -- exported
     checkLoadDocumentation();
     dbs := findAllDocDatabases();
     hts := findAllDocHashtables();
     lastabout = new NumberedVerticalList from sort unique join (
	  flatten apply(hts, (pkg,ht) -> (
		    r := select(keys ht, key -> match(s,key));
		    r = apply(r, key -> pkg | "::" | key);
		    r)),
	  flatten(apply(dbs,(pkg,dbname) -> (
			 r := select(getkeys' dbname, key -> match(s,key));
			 r = apply(r, key -> pkg | "::" | key);
			 r)))))

help#0 ZZ := i -> (
     if lastabout === null then error "no previous 'about' response";
     if not lastabout#?i then error ("previous 'about' response contains no entry numbered ", i);
     help lastabout#i)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

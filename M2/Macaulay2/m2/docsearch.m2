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

getkeys = dbname -> (
     db := openDatabase dbname;
     k := keys db;
     close db;
     k)

getkeys = memoize getkeys

about String := o -> s -> (				    -- exported
     dbs := findAllDocDatabases();
     flatten(apply(dbs,(pkg,dbname) -> (
		    r := select(getkeys dbname, key -> match(s,key));
		    r = apply(r, key -> pkg | "::" | key);
		    r))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

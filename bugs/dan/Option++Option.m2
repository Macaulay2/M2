-- priority: low

-- What does this code do?

    Option ++ Option := directSum
    directSum Option := o -> directSum(1 : o)
    Option.directSum = args -> (
	 if #args === 0 then error "expected more than 0 arguments";
	 modules := apply(args,last);
	 y := youngest modules;
	 key := (directSum, args);
	 if y =!= null and y#?key then y#key else (
	      type := single apply(modules, class);
	      if not type.?directSum then error "no method for direct sum";
	      M := type.directSum modules;
	      if y =!= null then y#key = M;
	      keys := M.cache.indices = toList args/first;
	      M.cache.indexComponents = new HashTable from apply(#keys, i -> keys#i => i);
	      M))

-- If it's not important, get rid of it.

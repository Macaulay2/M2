-- (Some) support for OM references and cookies.

-- storedOMrefs is a hash table, whose 
-- * keys are the references (as strings)
-- * the values are pairs (the values (as Macaulay2 objects), openMathObj)
-- addOMref ensures that the openMathObj in storedOMrefs have proper"id"s attached.

-- storedOMids is the other way around:
-- * keys are the Macaulay2 objects
-- * values are the ids as strings.
-- storedOMrefCounter is used to get serials for objects

storedOMrefs = new MutableHashTable;
storedOMids = new MutableHashTable;
storedOMrefCounter = 0;

existsOMref = method()
existsOMref String := x -> storedOMrefs#?x

--Careful! getOMref returns a pair of "thing" and "XMLnode" ;
--  the second one of these could be null
getOMref = method()
getOMref String := x -> storedOMrefs#x

getNewLocalRef = x -> (
	while (
		storedOMrefCounter = storedOMrefCounter + 1;
		r := concatenate("#r", toString(storedOMrefCounter));
		existsOMref(r)
	) do ();
	r
)
getNewForRemoteRef = x -> (
	while (
		i := random(10^11, 10^12);
		r := concatenate("scscp://macaulay2/", toString i);
		existsOMref(r)
	) do ();
	r
)

addOMref = method()
addOMref (String, Thing, XMLnode) := (s,t,x) -> ( 
	-- (a reference to "#x" will yield an id of "x")
	if not x#?"id" then	x#"id" = (select("^#?(.*)$", "\\1", s))#0;
	storedOMrefs#s = (t, x);
	storedOMids#t = s;
	s
);
addOMref (String, Thing, Nothing) := (s,t,x) -> ( 
	storedOMrefs#s = (t, null);
	storedOMids#t = s;
	s
);

addOMref (Thing, XMLnode) := (t, x) -> (
	if hasOMid(t) then
		getOMid(t)
	else
		addOMref(getNewLocalRef(), t, x)
);
addOMref (Thing, Nothing) := (t,x) -> (
	if hasOMid(t) then
		getOMid(t)
	else
		addOMref(getNewLocalRef(), t, x)
);

removeOMref = method()
removeOMref String := s -> (
	if not storedOMrefs#?s then return false;
	
	v := (storedOMrefs#s)#0;
	
	remove(storedOMrefs, s);
	remove(storedOMids, v);
	true
)


hasOMid = method()
hasOMid Thing := t -> storedOMids#?t

getOMid = method()
getOMid Thing := t -> storedOMids#t

removeOMid = method()
removeOMid Thing := t -> (
	if not (hasOMid t) then return false;
	
	removeOMref getOMid t;
	true
)


-- idCheck is used when converting to OpenMath: Automatically checks whether
-- an object has a stored ID and, if so, returns a reference instead of creating
-- the full object.
-- Similarly, autoIDCheck automagically creates such things.

-- printedIDs is used to determine whether idCheck and autoIDcheck are allowed
--   to return the reference. This allows to be reset by resetPrintedIDs, so that
--   you can be sure to print the actual object at least once on output.

declaredIDs = new MutableHashTable
resetDeclaredIDs = x -> (declaredIDs = new MutableHashTable)

idCheck = f -> x -> (
	--Doesn't have an id
	if (not hasOMid(x)) then return f x; 

	--Does have an id and it was declared, so we are allowed to OMR it
	s := getOMid(x);
	if declaredIDs#?s then return OMR(s);
	
	--ID wasn't declared yet, so we should. So get the OM object.
	--And we printed it now.
	declaredIDs#s = 1;
	
	--careful: we assume the OpenMath object is known and stored along with the reference...
	-- I think that's fine in this case, but am not entirely sure
	return (getOMref(s))#1;
)	
autoCreateIDCheck = f -> x -> (
	--If it exists, we use it
	r := (idCheck(f))(x);
	if class(r) === XMLnode and r.tag === "OMR" then return r;
	
	--Otherwise we assign it, and return the original object with that ID set
	-- and record that we printed it now.
	s := addOMref(x, r);
	declaredIDs#s = 1;
	(getOMref(s))#1
)



-- (Some) support for OM references and cookies.

-- storedOMrefs is a hash table, whose 
-- * keys are the references (as strings)
-- * the values are the values (Macaulay2 objects)
-- storedOMids is the other way around
-- storedOMrefCounter is used to get serials for objects

storedOMrefs = new MutableHashTable;
storedOMids = new MutableHashTable;
storedOMrefCounter = 0;

existsOMref = method()
existsOMref String := x -> storedOMrefs#?x

getOMref = method()
getOMref String := x -> storedOMrefs#x

getNextFreeRef = x -> (
	r := concatenate("#r", toString(storedOMrefCounter));
	while existsOMref(r) do (
		storedOMrefCounter = storedOMrefCounter + 1;
		r = concatenate("#r", toString(storedOMrefCounter));
	);
	r
)

addOMref = method()
addOMref (String, Thing) := (s,t) -> ( 
	storedOMrefs#s = t;
	storedOMids#t = s;
	s
)
addOMref Thing := t -> (
	if hasOMid(t) then
		getOMid(t)
	else
		addOMref(getNextFreeRef(), t)
);

removeOMref = method()
removeOMref String := s -> (
	if not storedOMrefs#?s then return false;
	
	v := storedOMrefs#s;
	
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
----TO FIX ----
idCheck = f -> x -> if hasOMid(x) then OMR(getOMid(x)) else f x
autoCreateIDCheck = idCheck

-- declaredIDs = set{}
-- 
-- idCheck = f -> x -> (
-- 	--Doesn't have an id
-- 	if (not hasOMid(x)) then return f x; 
-- 
-- 	--Does have an id, and it was declared, so we are allowed to OMR it
-- 	s := getOMid(x);
-- 	if printedIDs#?s then return OMR(s);
-- 	
-- 	--ID wasn't declared yet, so we should.
-- 	
-- 	or (printedIDs#?(s := getOMid(x))) then f x else OMR(s)
-- autoCreateIDCheck = f -> x -> (
-- 	--If it exists, we use it
-- 	r := (idCheck(f))(x);
-- 	if class(r) === XMLnode and r.tag === "OMR" then return r;
-- 	
-- 	--Otherwise we assign it, and return the original object with that ID set.
-- 	<< "r = " << r << endl;
-- 	s := addOMref(x);
-- 	r = new MutableHashTable from r;
-- 	r#"id" = s;
-- 	r = new HashTable from r;
-- 	r
-- )
-- resetDeclaredIDs = x -> (declaredIDs = set{})


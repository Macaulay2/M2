-- (Some) support for OM references and cookies.

-- storedOMrefs is a hash table, whose 
-- * keys are the references (as strings)
-- * the values are pairs (the values (as Macaulay2 objects), openMathObj)
-- addOMref ensures that the openMathObj in storedOMrefs have proper"id"s attached.

-- storedOMids is the other way around:
-- * keys are the Macaulay2 objects
-- * values are the ids as strings.
-- storedOMrefCounter is used to get serials for objects

striphashsign = id -> (select("^#?(.*)$", "\\1", id))#0;

storedOMrefs = new MutableHashTable;
storedOMids = new MutableHashTable;
storedOMrefCounter = 0;

existsOMref = method()
existsOMref String := x -> storedOMrefs#?x;

--Careful! getOMref returns a pair of "thing" and "XMLnode" ;
--  the second one of these could be null
getOMref = method()
getOMref String := x -> storedOMrefs#x;

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
--<<<<<<< .working
	if not x#?"id" then	x#"id" = striphashsign s;
--=======
--	if not x#?"id" then	x#"id" = s;
-->>>>>>> .merge-right.r9457
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

idCheck = f -> x -> if (not hasOMid(x)) then f x else (getOMref(getOMid(x)))#1;
autoCreateIDCheck = f -> x -> (
	--If it exists, we use it
	r := (idCheck(f))(x);
	if class(r) === XMLnode and r.tag === "OMR" then return r;
	
	--Otherwise we assign it
	s := addOMref(x, r);
	(getOMref(s))#1
)

-- This function will ensure that, when an object occurs more than once inside
-- an OpenMath object, only one actual instance remains, and the rest be changed to
-- references.
replaceMultipleIDs = x -> ( found := new MutableHashTable; replaceMultipleIDsRec(found, x) )
replaceMultipleIDsRec = (found, x) -> (
	if x#?"id" and x#"id" =!= null then (
		if not found#?(x#"id") then (
			--First time around
			found#(x#"id") = 1
		) else (
			--Second (or more) time around
			return OMR(x#"id")
		)
	);

	if x.?children then (
		for i in 0..(#(x.children)-1) do (
			nw := if class((x.children)#i) === XMLnode then replaceMultipleIDsRec(found, (x.children)#i) else (x.children)#i;
			if (nw =!= (x.children)#i) then x.children = replace(i, nw, x.children);
		);
		x
	) else (
		x
	)	
)


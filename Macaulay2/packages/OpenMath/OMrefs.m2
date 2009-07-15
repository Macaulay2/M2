-- (Some) support for OM references and cookies.

-- storedOMrefs is a hash table, whose 
-- * keys are the references (as strings)
-- * the values are the values (Macaulay2 objects)
-- storedOMids is the other way around

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



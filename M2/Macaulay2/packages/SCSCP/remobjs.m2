remoteObject = new Type of XMLnode;

getGlobalSymbol(SCSCP#"private dictionary", "connection")
net remoteObject := x -> (
	if x.?connection and x.connection#?"service_name" then 
		concatenate("<< Remote ", x.connection#"service_name", " object >>")
	else if x.?connection and x.connection#?"nicedesc" then
		concatenate("<< Remote object at ", x.connection#"nicedesc", " >>")
	else
		concatenate("<< Some remote object >>")
)


identifyRemoteObjects = method();
-- identifyRemoteObjects (SCSCPConnection, XMLnode) := (s,x)-> (
-- 	--I try to avoid copying here unless I have a choice
--  -- should use "drop" and "replace"
-- 	if x.tag === "OMR" then (
-- 		r := new remoteObject from x;
-- 		r.connection = s;
-- 		r
-- 	) else if x.?children then (
-- 		for i in 0..(#(x.children)-1) do
-- 			(x.children)#i = if class((x.children)#i) === XMLnode then identifyRemoteObjects(s,(x.children)#i) else (x.children)#i;
-- 		x
-- 	) else (
-- 		x
-- 	)
-- )
identifyRemoteObjects (SCSCPConnection, XMLnode) := (s,x)-> (
	if x.tag === "OMR" then (
		r := new remoteObject from x;
		r.connection = s;
		r
	) else if x.?children then (
		x.children = apply(x.children, i -> if class(i) === XMLnode then identifyRemoteObjects(s,i) else i);
		x
	) else (
		x
	)
)

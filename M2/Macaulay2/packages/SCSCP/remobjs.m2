RemoteObject = new Type of XMLnode
RemoteObject.synonym = "remote object"

protect connection

getGlobalSymbol(SCSCP#"private dictionary", "connection")
net RemoteObject := x -> (
	if x.?connection and x.connection#?"service_name" then 
		concatenate("<< Remote ", x.connection#"service_name", " object >>")
	else if x.?connection and x.connection#?"nicedesc" then
		concatenate("<< Remote object at ", x.connection#"nicedesc", " >>")
	else
		concatenate("<< Some remote object >>")
)



identifyRemoteObjects = method();
identifyRemoteObjects (SCSCPConnection, XMLnode) := (s,x)-> (
	--I try to avoid copying here unless I have a choice
	if x.tag === "OMR" then (
		r := new RemoteObject from x;
		r.connection = s;
		r
	) else if x.?children then (
		for i in 0..(#(x.children)-1) do (
			nw := if class((x.children)#i) === XMLnode then identifyRemoteObjects(s,(x.children)#i) else (x.children)#i;
			if (nw =!= (x.children)#i) then x.children = replace(i, nw, x.children);
		);
		x
	) else (
		x
	)
)

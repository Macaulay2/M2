----------------------------------
---------- CLIENT CODE -----------
----------------------------------
net SCSCPConnection := x -> x#"nicedesc";

closeConnection := s -> (
	s#"nicedesc" = "Closed SCSCP connection";
	try close s#"fd";
)

Manipulator SCSCPConnection := (m, s) -> (
	if m === close then closeConnection s
	else error concatenate("Cannot apply Manipulator ", toString m, " to SCSCPConnection");
)

newConnection = method();
newConnection (String, String) := (host, port) -> (
	hostport := host|":"|port;
	dbgout(1) << "[Client] Connecting to " << hostport << endl;

	s := new SCSCPConnection;
	s#"nicedesc" = "Uninitialized SCSCP connection to " | hostport;
	s#"fd" = openInOut ("$"|hostport);

	ans := ""; buf := "";
	while #ans === 0 or ans#-1 =!= "\n" do (
		buff := read s#"fd";
		buf = buff|"";
		if atEndOfFile s#"fd" then ( 
			dbgout(0) << "[Client]  atEndOFFile" << endl; 
			closeConnection s;		
			error("atEndOfFile during connection"); 
		);

		ans = ans|buf;
	);

	dbgout(2) << "[Client] Received: '" << ans << "'" << endl;
	re := "<\\?scscp (service_name)=\"([^\"]+)\" (service_version)=\"([^\"]+)\" (service_id)=\"([^\"]+)\" (scscp_versions)=\"([^\"]+)\" \\?>";
	if (mtch := regex(re, ans)) === null then (
		dbgout(1) << "[Client] Received invalid HELO" << endl;
		s#"nicedesc" = "SCSCP Connection to " | hostport;
	) else (
		for i in 1..4 do s#(substring(ans, mtch#(2*i-1)#0, mtch#(2*i-1)#1)) = substring(ans, mtch#(2*i)#0, mtch#(2*i)#1);
		s#"nicedesc" = "SCSCP Connection to " | s#"service_name" | " (" | s#"service_version" | ") on " | hostport;
	);

	versionstr := ///<?scscp version="/// | ProtVersion | ///" ?>///;
	dbgout(2) << "[Client] Requesting '" << versionstr << "'" << endl;
	s#"fd" << versionstr << endl << flush;
	
	dbgout(2) << "[Client] Waiting for response to version request..." << endl;
	ans = "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		buff = read s#"fd";
		buf = buff|"";
		if atEndOfFile s#"fd" then ( 
			dbgout(0) << "[Client]  atEndOFFile" << endl; 
			closeConnection s;		
			error("atEndOfFile during version negotiation"); 
		);

		ans = ans|buf;
	);
	
	dbgout(2) << "[Client] Received: '" << ans << "'" << endl;
	
	if ans =!= versionstr|"\n" then (
		dbgout(0) << "[Client] Incompatible." << endl;
		closeConnection s;
		error("SCSCP connection failed.");
	);
	
	s	
);
newConnection (String, ZZ) := (host, port) -> newConnection(host, toString port);
newConnection (String) := s -> (
	mtch := regex("^(.*)\\:(.*)$", s);
	if mtch === null then 
		newConnection(s, "26133")
	else
		newConnection(substring(s, mtch#1#0, mtch#1#1),substring(s, mtch#2#0, mtch#2#1))
)


compute := method()
compute (SCSCPConnection, XMLnode, String) := (s,x, ret) -> (

	if not isOpen s#"fd" then error("Connection is closed.");

	dbgout(5) << "[Compute] replaceMultipleIDs..." << endl;
	x = replaceMultipleIDs x;

	dbgout(2) << "[Compute] Constructing procedure call..." << endl;
	if x.tag =!= "OMA" then x = OMA("fns1", "identity", {x});
	pc := OMA("scscp1", "procedure_call", { x });
	setOMAttr(pc, OMS("scscp1", "call_id"), OMSTR(toString (callIDCounter = callIDCounter+1)));
	setOMAttr(pc, OMS("scscp1", "option_return_"|ret), OMSTR(""));
	pc = OMOBJ(pc);
	pc = createOMATTRObj(pc);
	dbgout(5) << endl << pc << endl;

	dbgout(2) << "[Compute] Sending procedure call..." << endl;
	tspc := toString toLibxmlNode pc;
	dbgout(5) << endl << tspc << endl;
	s#"fd" << "<?scscp start ?>" << endl << flush;
	s#"fd" << tspc << endl;
	s#"fd" << "<?scscp end ?>" << endl << flush;

	dbgout(2) << "[Compute] Waiting for response..." << endl;
	ans := "";
	waitfor := "(.*)<\\?scscp end \\?>\n$";
	while not match(waitfor, ans) do (
		buff := read s#"fd";
		buf := buff|"";
		if atEndOfFile s#"fd" then ( dbgout(0) << "[Client]  atEndOFFile" << endl; return null; );

		ans = ans|buf;
	);
	
	dbgout(2) << "[Client] Answer received..." << endl;
	dbgout(5) << endl << ans << endl;
	
	dbgout(2) << "[Client] Parsing answer..." << endl;
	y := parse ans;
	if class(y) =!= XMLnode then
		error "Parsing failed.";

	dbgout(2) << "[Client] Identifying remote objects..." << endl;
	y = identifyRemoteObjects(s, y);
		
	y
)
compute (SCSCPConnection, Thing, String) := (s,x, ret) -> (
	if not isOpen s#"fd" then error("Connection is closed.");

	dbgout(2) << "[Compute] Constructing OpenMath object..." << endl;
	o := openMath x;
	if class(o) === XMLnode and o.tag === "OME" then (
		stderr << o << endl;
		error(concatenate("Could not convert '", toString x, "' of type '",toString class x,"'to OpenMath"));
	);
	
	a := compute(s, o, ret);
	
	dbgout(2) << "[Compute] Evaluating answer..." << endl;
	dbgout(5) << endl << a << endl;
	t := value a;

	t
)
compute (SCSCPConnection, XMLnode) := (s,x) -> compute(s, x, "object");
compute (SCSCPConnection, Thing) := (s,t) -> compute(s, t, "object");



-- This allows for s( .. something .. )
SCSCPConnection Thing := (s,t) -> (
	if      class(t) === Sequence and #t === 1 then compute(s, t#0)
	else if class(t) === Sequence and #t === 2 then compute(s, t#0, t#1)
	else compute(s,t)
)



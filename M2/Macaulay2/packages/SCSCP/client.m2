
----------------------------------
---------- CLIENT CODE -----------
----------------------------------
newConnection = method();
newConnection (String, String) := (host, port) -> (
	hostport := host|":"|port;
	dbgout(1) << "[Client] Connecting to " << hostport << endl;

	s := new SCSCPConnection;
	s#"fd" = openInOut ("$"|hostport);

	ans := ""; buf := "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		buf = read s#"fd";
		if atEndOfFile s#"fd" then ( 
			dbgout(0) << "[Client]  atEndOFFile" << endl; 
			error("atEndOfFile during connection"); 
		);

		ans = ans|buf;
	);

	dbgout(2) << "[Client] Received: '" << ans << "'" << endl;

	versionstr := ///<?scscp version="/// | ProtVersion | ///" ?>///;
	dbgout(2) << "[Client] Requesting '" << versionstr << "'" << endl;
	s#"fd" << versionstr << endl << flush;
	
	dbgout(2) << "[Client] Waiting for response to version request..." << endl;
	ans = "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		buf = read s#"fd";
		if atEndOfFile s#"fd" then ( 
			dbgout(0) << "[Client]  atEndOFFile" << endl; 
			error("atEndOfFile during version negotiation"); 
		);

		ans = ans|buf;
	);
	
	dbgout(2) << "[Client] Received: '" << ans << "'" << endl;
	
	if ans =!= versionstr|"\n" then (
		dbgout(0) << "[Client] Incompatible." << endl;
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

Manipulator SCSCPConnection := (m, s) -> (
	if m === close then close s#"fd"
	else error "Cannot apply this Manipulator to SCSCPConnection";
)

compute := method()
compute (SCSCPConnection, XMLnode) := (s,x) -> (

	dbgout(2) << "[Compute] Constructing procedure call..." << endl;
	if x.tag =!= "OMA" then x = OMA("fns1", "identity", {x});
	pc := OMA("scscp1", "procedure_call", { x });
	pc = setOMAttr(pc, OMS("scscp1", "call_id"), OMSTR(toString (callIDCounter = callIDCounter+1)));
	pc = setOMAttr(pc, OMS("scscp1", "option_return_object"), OMSTR(""));
	pc = OMOBJ(pc);
	pc = createOMATTRObj(pc);

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
		buf = read s#"fd";
		if atEndOfFile s#"fd" then ( dbgout(0) << "[Client]  atEndOFFile" << endl; return null; );

		ans = ans|buf;
	);
	
	dbgout(2) << "[Client] Answer received..." << endl;
	dbgout(5) << endl << ans << endl;
	
	dbgout(2) << "[Client] Parsing answer..." << endl;
	y := parse ans;
	if class(y) =!= XMLnode then
		error "Parsing failed.";
		
	y
)
compute (SCSCPConnection, Thing) := (s,x) -> (
	--When constructin an OpenMath object, we first make sure that we do not throw undeclared
	--  and possibly automatically generated ids around. I consider this a good idea, I think.
	resetDeclaredIDs();

	dbgout(2) << "[Compute] Constructing OpenMath object..." << endl;
	o := openMath x;
	if class(o) === XMLnode and o.tag === "OME" then (
		stderr << o << endl;
		error(concatenate("Could not convert '", toString x, "' of type '",toString class x,"'to OpenMath"));
	);
	
	a := compute(s, o);
	
	dbgout(2) << "[Compute] Evaluating answer..." << endl;
	t := value a;

	t
)

-- This allows for s( .. something .. )
SCSCPConnection Thing := compute


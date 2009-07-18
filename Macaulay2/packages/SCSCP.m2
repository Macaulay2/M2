needsPackage "OpenMath"
needsPackage "XML"
newPackage(
	"SCSCP",
	Version => "0.9", 
	Date => "July 17, 2009",
	Authors => {
		{Name => "Dan Roozemond", Email => "d.a.roozemond@tue.nl", HomePage => "http://www.win.tue.nl/~droozemo/"}
	},
	Headline => "SCSCP for Macaulay2",
	DebuggingMode => true,
	AuxiliaryFiles => false
)

debug needsPackage "OpenMath" -- so that we have all global symbols of OpenMath in here.
needsPackage "XML"

----------------------------------
---------- SETTINGS --------------
----------------------------------
SCSCPConnection = new Type of MutableHashTable
ProtVersion = "1.3"
ProtCompatibleVersions = { "1.2", "1.3" }
ServiceName = "Macaulay2"
ServiceVersion = (options SCSCP).Version

callIDCounter = 0;
incomingConnCounter = 0;

----------------------------------
-- CONVENIENT DEBUGGING OUTPUT ---
----------------------------------
dbgout = l -> (
	if debugLevel >= l then
		stderr << "[SCSCP]"
	else
		null
)
--Convention:
-- 0: really critical stuff.
-- 1: main messages
-- 2: other info
-- 5: things you almost certainly never want to read

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
	o := openMathValue x;
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


----------------------------------
---------- SERVER CODE -----------
----------------------------------
serverSocket = null;
startServer = method();
startServer (String) := hostport -> (
	--Maybe we should cleanup the old socket?
	if serverSocket =!= null and class(serverSocket) === File then (
		dbgout(1) << "[Server] Cleaning up old socket." << endl;
		close serverSocket;
	);

	--Here we go...
	if match("[0-9]+", hostport) then hostport = ":"|hostport;
	dbgout(0) << "[Server] Listening on " << hostport << endl;

	serverSocket = openListener("$"|hostport);
	while true do (
		dbgout(2) << "[Server] Waiting for incoming connection "  << endl;
		g := openInOut serverSocket;
		dbgout(0) << "[Server] Incoming connection. Forking. " << endl;
		
		--Once we are here an incoming connection arrived, since openInOut blocks.
		
		--So we fork. Furthermore, to avoid zombie processes, we create children, and
		--have them create a "grandchild" immediately, and terminate. In the parent, 
		--we then wait (hopefully for a short time) for the child to terminate. Yay.
		incomingConnCounter = incomingConnCounter+1;
		collectGarbage();
		pid := fork();
		if pid =!= 0 then (
			--parent; pid is the pid of the child.
			close g;
			r := wait pid;
			if r =!= 0 then 
				dbgout(0) << "[Server] Child exited with nonzero exit code." << endl;
		) else (
			--child (can find out own pid with getpid)
			--Close stdin in the child. We'll still be in the same process group, so that Ctrl-C
			--hopefully still kills us.
			
			stopIfError = true;
			debuggingMode = false;
			--handleInterrupts = false; --so that, on Ctrl-C, all children get eliminated.
			closeIn stdio;
			
			pid2 := fork();
			if pid2 =!= 0 then (
				--still in child; terminate.
				exit(0);
			) else (
				--in grandchild; do stuff
				handleIncomingConnection(g);
				dbgout(2) << "[Server] Child " << incomingConnCounter << " terminated" << endl;
				exit(0); --this closes g automatically as well.
			);
		);
		
	);

	close serverSocket;
)
startServer (ZZ) := port -> startServer toString port;
installMethod(startServer, () -> startServer("26133"));

handleIncomingConnection = sock -> (
	cid := incomingConnCounter;
	dbgout(2) << "[handleIncoming " << cid << "] Handling new connection" << endl;
	
	s := concatenate(
		"<?scscp service_name=", format ServiceName, 
		" service_version=", format ServiceVersion, 
		" service_id=", format "0",
		" scscp_versions=", format ProtVersion, 
		" ?>");

	dbgout(2) << "[handleIncoming " << cid << "] Sending announcement" << endl;
	sock << s << endl << flush;

	dbgout(2) << "[handleIncoming " << cid << "] Waiting for version request..." << endl;
	ans := ""; buf := ""; mtch := null;
	waitfor := "<\\?scscp version=\"(.*)\" \\?>"; 
	while (mtch = regex(waitfor, ans)) === null do (
		buf = read sock;

		--Handle EOF
		if atEndOfFile sock then (
			dbgout(1) << "[handleIncoming " << cid << "]  atEndOFFile" << endl;
			return;
		);

		ans = ans|buf;
	);
	ver := substring(ans, mtch#1#0, mtch#1#1);
	if not member(ver, ProtCompatibleVersions) then (
		dbgout(1) << "[handleIncoming " << cid << "] Incompatible version: '" << ver << "'" << endl;
		sock << "<?scscp quit reason=\"incompatible version " << ver << "\" ?>" << endl << flush;
		return;
	);
	ans = substring(ans, mtch#0#0 + mtch#0#1);

	dbgout(2) << "[handleIncoming " << cid << "] Great! Compatible version: '" << ver << "'" << endl;
	sock << "<?scscp version=\"" << ver << "\" ?>" << endl << flush;
	
	while true do (
		dbgout(5) << "[handleIncoming " << cid << "] Waiting for pc...'" << "'" << endl;
		waitfor = "(.*)<\\?scscp ([a-z]+)( [^>]*)? \\?>";
		while (mtch = regex(waitfor, ans)) === null do (
			buf = read sock;

			--Handle EOF
			if atEndOfFile sock then (
				dbgout(1) << "[handleIncoming " << cid << "]  atEndOFFile" << endl;
				return;
			);

			ans = ans|buf;
		);
		keyw := substring(ans, mtch#2#0, mtch#2#1);

		if keyw === "quit" then (
			--<?scscp quit ?>
			dbgout(2) << "[handleIncoming " << cid << "] 'quit' received" << endl;
			return;
		) else if keyw === "cancel" then (
			--<?scscp cancel ?>
			dbgout(2) << "[handleIncoming " << cid << "] 'cancel' received" << endl;
			ans = "";
		) else if keyw === "start" then (
			--<?scscp start ?>
			dbgout(2) << "[handleIncoming " << cid << "] 'start' received" << endl;
			--take off this bit, otherwise we'll end up in this case every time...
			ans = substring(ans, mtch#0#0 + mtch#0#1);
		) else if keyw === "ack" then (
			--<?scscp ack ?>
			dbgout(5) << "[handleIncoming " << cid << "] 'ack' received" << endl;
			--take off this bit
			ans = substring(ans, mtch#0#0 + mtch#0#1);
		) else if keyw === "end" then (
			--<?scscp end ?>
			dbgout(5) << "[handleIncoming " << cid << "] 'end' received" << endl;
			
			question := substring(ans, 0, mtch#1#0);
			ans = substring(ans, mtch#0#0 + mtch#0#1);
			resp := handleProcedureCall(question);
			
			sock << "<?scscp start ?>" << endl << resp << endl << "<?scscp end ?>" << endl << flush;
		) else (
			--Unknown keyword!!
			dbgout(1) << "[handleIncoming " << cid << "] Unrecognized keyword: '" << keyw << "'" << endl;
			--take off this bit, otherwise we'll end up in this case every time...
			ans = substring(ans, mtch#0#0 + mtch#0#1);		
		)
	
	)

	
)

handleProcedureCall = str -> (
	--input will be a string, output will be a string.
	--don't need to do <?scscp start ?> and -end nonsense, though.

	--note that the toOpenMath of a procedure call is somewhat special, since it returns an 
	--  XMLnode automatically. To allow for people to do somewhat different things (like typing
	--  OpenMath without a procedure call), we try an openMathValue x if necessary.
	
	cid := incomingConnCounter;
	
	dbgout(5) << "[handleProcedureCall " << cid << "] Disassembling procedure call..." << endl;
	dbgout(5) << endl << str << endl;
	try (
		t := parse str;
	) else (
		dbgout(1) << "[handleProcedureCall " << cid << "] Parsing failed...." << endl;
		pt := constructProcTerm("Parsing your question failed.", OMSTR("0"));
		return toString toLibxmlNode createOMATTRObj OMOBJ pt;
	);

	dbgout(2) << "[handleProcedureCall " << cid << "] Evaluating procedure call..." << endl;
	ret := value t;
	
	if class(ret) =!= XMLnode then (
		dbgout(2) << "[handleProcedureCall " << cid << "] Hmz, response was no XMLnode. Ah well, we'll make one" << endl;
		ret = openMathValue ret;
	);
	
	dbgout(2) << "[handleProcedureCall " << cid << "] Returning response..." << endl;
	
	return toString toLibxmlNode createOMATTRObj OMOBJ ret;
)


------------------------
----DOCUMENTATION-------
------------------------
export { "SCSCPConnection", "newConnection", "startServer" }

beginDocumentation()
document { 
	Key => SCSCP,
	Headline => "SCSCP (Symbolic Computation Software Composability Protocol) support"
	}

document {
	Key => { newConnection, (newConnection, String), (newConnection, String, String), (newConnection, String, ZZ) },
	Headline => "Create a connection to an SCSCP server",
	Usage => "newConnection (host, port)",
	Inputs => { 
		"host" => String => { "The name or IP address of the host to connect to, with an optional colon followed by the number of the port" }, 
		"port" => {ofClass{String, ZZ}, ", providing the port number (this argument may be omitted)"}
	},
	Outputs => { SCSCPConnection => "The connection that was established" },
	"The port to connect to may be specified either by giving only one argument of the form host:port
	or by specifying the second argument. If neither of these is present, the port defaults to 26133.",
	SeeAlso => { (symbol SPACE, SCSCPConnection, Thing), (symbol SPACE, Manipulator, SCSCPConnection) }
	}

document {
	Key => { (symbol SPACE, Manipulator, SCSCPConnection) },
	Headline => "Close an SCSCP connection",
	Usage => "close s",
	Inputs => { 
		"close" => Manipulator => {"The manipulator called close"},
		"s" => SCSCPConnection => {"A connection, previously created with newConnection"}
	},
	SeeAlso => { (symbol SPACE, SCSCPConnection, Thing), newConnection }
 	}


document {
	Key => { SCSCPConnection, (symbol SPACE, SCSCPConnection, Thing) },
	Headline => "Execute computations using SCSCP",
	Usage => "s x",
	Inputs => { 
		"s" => SCSCPConnection => {"The server that should compute it"},
		"x" => Thing => {"The expression to be computed "}
	},
	Outputs => { Thing => "The result of the computation" },
	"As an example, we connect to a locally running SCSCP server: ",
	PRE ///
		i2 : s = newConnection("127.0.0.1", 26133);

		i3 : s(hold(2)+3)

		o3 = 5

		i4 : close s
	 	///,

	"We could also explicitly have a look at the openMath that's being passed around",
	PRE ///
		i2 : s = newConnection "127.0.0.1"

		o2 = SCSCPConnection{...1...}

		o2 : SCSCPConnection

		i3 : o = openMathValue (hold(2)+3)

		o3 = <OMA
		       <OMS cd="arith1" name="plus"
		       <OMI "2"
		       <OMI "3"

		o3 : XMLnode

		i4 : s(o)

		o4 = <OMOBJ
		       <OMATTR
		         <OMATP
		           <OMS cd="scscp1" name="call_id"
		           <OMSTR "1"
		         <OMA
		           <OMS cd="scscp1" name="procedure_completed"
		           <OMI "5"

		o4 : XMLnode

		i5 : value oo

		o5 = 5

	 	///,

	SeeAlso => { newConnection, (symbol SPACE, Manipulator, SCSCPConnection) }	
 	}


document {
	Key => { startServer, 1:startServer, (startServer, String), (startServer, ZZ) },
	Headline => "Start an SCSCP server",
	Usage => "startServer port",
	Inputs => { 
		"port" => {ofClass{String, ZZ}, ", providing the port number (defaults to 26133)"}
	},
	"The server will keep running indefinitely; it may be stoppend by sending a Ctrl-C. Furthermore,
	the server forks for every new incoming connection, so that it can serve many clients simultaneously.
	The amount of output printed to the screen is determined by the vaule of debugLevel.",
	PRE ///
		i2 : debugLevel = 2;

		i3 : startServer(26137)
		[SCSCP][Server] Listening on :26137
		[SCSCP][Server] Waiting for incoming connection 
		[SCSCP][Server] Incoming connection. Forking. 
		[SCSCP][handleIncoming 1] Handling new connection
		[SCSCP][handleIncoming 1] Sending announcement
		[SCSCP][handleIncoming 1] Waiting for version request...
		[SCSCP][handleIncoming 1] Great! Compatible version: '1.3'
		[SCSCP][Server] Waiting for incoming connection 
		[SCSCP][handleIncoming 1] 'start' received
		[SCSCP][handleProcedureCall 1] Evaluating procedure call...
		[SCSCP][handleProcedureCall 1] Returning response...
		[SCSCP][handleIncoming 1]  atEndOFFile
		[SCSCP][Server] Child 1 terminated
	 	///
	}




------------------------
----FOR DEBUGGING-------
------------------------
endPackage("SCSCP");
debug SCSCP

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

SCSCPConnection = new Type of MutableHashTable
ProtVersion = "1.3"
ProtCompatibleVersions = { "1.2", "1.3" }
ServiceName = "Macaulay2"
ServiceVersion = (options SCSCP).Version

callIDCounter = 0;
incomingConnCounter = 0;

----------------------------------
---------- CLIENT CODE -----------
----------------------------------

newConnection = hostport -> (
	s := new SCSCPConnection;
	s#"fd" = openInOut ("$"|hostport);

	stderr << "[SCSCP][Client] Connecting..." << endl;
	ans := ""; buf := "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		buf = read s#"fd";
		if atEndOfFile s#"fd" then ( stderr << "[SCSCP][Client]  atEndOFFile" << endl; return; );

		ans = ans|buf;
	);

	stderr << "[SCSCP][Client] Received: '" << ans << "'" << endl;

	versionstr := ///<?scscp version="/// | ProtVersion | ///" ?>///;
	stderr << "[SCSCP][Client] Requesting '" << versionstr << "'" << endl;
	s#"fd" << versionstr << endl << flush;
	
	stderr << "[SCSCP][Client] Waiting for response to version request..." << endl;
	ans = "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		buf = read s#"fd";
		if atEndOfFile s#"fd" then ( stderr << "[SCSCP][Client]  atEndOFFile" << endl; return; );

		ans = ans|buf;
	);
	
	stderr << "[SCSCP][Client] Received: '" << ans << "'" << endl;
	
	if ans =!= versionstr|"\n" then (
		stderr << "[SCSCP][Client] Incompatible." << endl;
		error("SCSCP connection failed.");
	);
	
	s	
);

Manipulator SCSCPConnection := (m, s) -> (
	if m === close then close s#"fd"
	else error "Cannot apply this Manipulator to SCSCPConnection";
)

compute = method()
compute (SCSCPConnection, XMLnode) := (s,x) -> (

	stderr << "[SCSCP][Compute] Constructing procedure call..." << endl;
	if x.tag =!= "OMA" then x = OMA("fns1", "identity", {x});
	pc := OMA("scscp1", "procedure_call", { x });
	pc = setOMAttr(pc, OMS("scscp1", "call_id"), OMSTR(toString (callIDCounter = callIDCounter+1)));
	pc = setOMAttr(pc, OMS("scscp1", "option_return_object"), OMSTR(""));
	pc = OMOBJ(pc);
	pc = createOMATTRObj(pc);

	stderr << "[SCSCP][Compute] Sending procedure call..." << endl;
	tspc := toString toLibxmlNode pc;
	stderr << endl << tspc << endl;
	s#"fd" << "<?scscp start ?>" << endl << flush;
	s#"fd" << tspc << endl;
	s#"fd" << "<?scscp end ?>" << endl << flush;

	stderr << "[SCSCP][Compute] Waiting for response..." << endl;
	ans := "";
	waitfor := "(.*)<\\?scscp end \\?>\n$";
	while not match(waitfor, ans) do (
		buf = read s#"fd";
		if atEndOfFile s#"fd" then ( stderr << "[SCSCP][Client]  atEndOFFile" << endl; return null; );

		ans = ans|buf;
	);
	
	stderr << "[SCSCP][Client] Answer received..." << endl;
	<< ans << endl;
	
	stderr << "[SCSCP][Client] Parsing answer..." << endl;
	y := parse ans;
	if class(y) =!= XMLnode then
		error "Parsing failed.";
		
	y
)
compute (SCSCPConnection, Thing) := (s,x) -> (
	--When constructin an OpenMath object, we first make sure that we do not throw undeclared
	--  and possibly automatically generated ids around. I consider this a good idea, I think.
	resetDeclaredIDs();

	stderr << "[SCSCP][Compute] Constructing OpenMath object..." << endl;
	o := openMathValue x;
	if class(o) === XMLnode and o.tag === "OME" then (
		stderr << o << endl;
		error(concatenate("Could not convert '", toString x, "' of type '",toString class x,"'to OpenMath"));
	);
	
	a := compute(s, o);
	
	stderr << "[SCSCP][Compute] Evaluating answer..." << endl;
	t := value a;

	t
)

-- This allows for s( .. something .. )
SCSCPConnection Thing := compute


----------------------------------
---------- SERVER CODE -----------
----------------------------------
startServer = hostport -> (
	stderr << "[SCSCP][Server] Listening on hostport " << hostport << endl;

	f := openListener("$"|hostport);
	while true do (
		stderr << "[SCSCP][Server] Waiting for incoming connection "  << endl;
		g := openInOut f;
		stderr << "[SCSCP][Server] Incoming connection. Forking. " << endl;
		
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
				stderr << "[SCSCP][Server] Child exited with nonzero exit code." << endl;
		) else (
			--child (can find out own pid with getpid)
			--Close stdin in the child. We'll still be in the same process group, so that Ctrl-C
			--hopefully still kills us.
			
			stopIfError = true;
			debuggingMode = false;
			closeIn stdio;
			
			pid2 := fork();
			if pid2 =!= 0 then (
				--still in child; terminate.
				exit(0);
			) else (
				--in grandchild; do stuff
				handleIncomingConnection(g);
				stderr << "[SCSCP][Server] Child " << incomingConnCounter << " terminated" << endl;
				exit(0); --this closes g automatically as well.
			);
		);
		
	);

	close f;
)

handleIncomingConnection = sock -> (
	cid := incomingConnCounter;
	stderr << "[SCSCP][handleIncoming " << cid << "] Handling new connection" << endl;
	
	s := concatenate(
		"<?scscp service_name=", format ServiceName, 
		" service_version=", format ServiceVersion, 
		" service_id=", format "0",
		" scscp_versions=", format ProtVersion, 
		" ?>");

	stderr << "[SCSCP][handleIncoming " << cid << "] Sending announcement" << endl;
	sock << s << endl << flush;

	stderr << "[SCSCP][handleIncoming " << cid << "] Waiting for version request..." << endl;
	ans := ""; buf := ""; mtch := null;
	waitfor := "<\\?scscp version=\"(.*)\" \\?>"; 
	while (mtch = regex(waitfor, ans)) === null do (
		buf = read sock;

		--Handle EOF
		if atEndOfFile sock then (
			stderr << "[SCSCP][handleIncoming " << cid << "]  atEndOFFile" << endl;
			return;
		);

		ans = ans|buf;
	);
	ver := substring(ans, mtch#1#0, mtch#1#1);
	if not member(ver, ProtCompatibleVersions) then (
		stderr << "[SCSCP][handleIncoming " << cid << "] Incompatible version: '" << ver << "'" << endl;
		sock << "<?scscp quit reason=\"incompatible version " << ver << "\" ?>" << endl << flush;
		return;
	);
	ans = substring(ans, mtch#0#0 + mtch#0#1);

	stderr << "[SCSCP][handleIncoming " << cid << "] Great! Compatible version: '" << ver << "'" << endl;
	sock << "<?scscp version=\"" << ver << "\" ?>" << endl << flush;
	
	while true do (
		stderr << "[SCSCP][handleIncoming " << cid << "] Waiting for pc...'" << "'" << endl;
		waitfor = "(.*)<\\?scscp ([a-z]+)( [^>]*)? \\?>";
		while (mtch = regex(waitfor, ans)) === null do (
			buf = read sock;

			--Handle EOF
			if atEndOfFile sock then (
				stderr << "[SCSCP][handleIncoming " << cid << "]  atEndOFFile" << endl;
				return;
			);

			ans = ans|buf;
		);
		keyw := substring(ans, mtch#2#0, mtch#2#1);

		if keyw === "quit" then (
			--<?scscp quit ?>
			stderr << "[SCSCP][handleIncoming " << cid << "] 'quit' received" << endl;
			return;
		) else if keyw === "cancel" then (
			--<?scscp cancel ?>
			stderr << "[SCSCP][handleIncoming " << cid << "] 'cancel' received" << endl;
			ans = "";
		) else if keyw === "start" then (
			--<?scscp start ?>
			stderr << "[SCSCP][handleIncoming " << cid << "] 'start' received" << endl;
			--take off this bit, otherwise we'll end up in this case every time...
			ans = substring(ans, mtch#0#0 + mtch#0#1);
		) else if keyw === "end" then (
			--<?scscp end ?>
			stderr << "[SCSCP][handleIncoming " << cid << "] 'end' received" << endl;
			
			question := substring(ans, 0, mtch#1#0);
			ans = substring(ans, mtch#0#0 + mtch#0#1);
			resp := handleProcedureCall(question);
			
			sock << "<?scscp start ?>" << endl << resp << endl << "<?scscp end ?>" << endl << flush;
		) else (
			--Unknown keyword!!
			stderr << "[SCSCP][handleIncoming " << cid << "] Unrecognized keyword: '" << keyw << "'" << endl;
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
	
	stderr << "[SCSCP][handleProcedureCall " << cid << "] Disassembling procedure call..." << endl;
	try (
		t := parse str;
	) else (
		stderr << "[SCSCP][handleProcedureCall " << cid << "] Parsing failed...." << endl;
		pt := constructProcTerm("Parsing your question failed.", OMSTR("0"));
		return toString toLibxmlNode createOMATTRObj OMOBJ pt;
	);

	stderr << "[SCSCP][handleProcedureCall " << cid << "] Evaluating procedure call..." << endl;
	ret := value t;
	
	if class(ret) =!= XMLnode then (
		stderr << "[SCSCP][handleProcedureCall " << cid << "] Hmz, response was no XMLnode. Ah well, we'll make one" << endl;
		ret = openMathValue ret;
	);
	
	stderr << "[SCSCP][handleProcedureCall " << cid << "] Returning response..." << endl;
	
	return toString toLibxmlNode createOMATTRObj OMOBJ ret;
)




------------------------
----For Debugging-------
------------------------
endPackage("SCSCP");
debug SCSCP

------------------------
-------Testing----------
------------------------
{*
loadPackage "SCSCP"
s = newConnection("127.0.0.1:26133")
s(OMA("arith1", "plus", {OMI(3), OMI(4)}))
s(17)
*}




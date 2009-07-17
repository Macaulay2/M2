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
SCSCPProtVersion = "1.3"
SCSCPProtCompatibleVersions = { "1.2", "1.3" }
SCSCPServiceName = "Macaulay2"
SCSCPServiceVersion = (options SCSCP).Version

callIDCounter = 0;
incomingCounter = 0;

----------------------------------
---------- CLIENT CODE -----------
----------------------------------

newSCSCPConnection = hostport -> (
	s := new SCSCPConnection;
	s#"fd" = openInOut ("$"|hostport);

	stderr << "[SCSCPClient] Connecting..." << endl;
	ans := "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		ans = ans|(read s#"fd");
	);

	stderr << "[SCSCPClient] Received: '" << ans << "'" << endl;

	versionstr := ///<?scscp version="/// | SCSCPProtVersion | ///" ?>///;
	stderr << "[SCSCPClient] Requesting '" << versionstr << "'" << endl;
	s#"fd" << versionstr << endl << flush;
	
	stderr << "[SCSCPClient] Waiting for response to version request..." << endl;
	ans = "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		ans = ans|(read s#"fd");
	);
	
	stderr << "[SCSCPClient] Received: '" << ans << "'" << endl;
	
	if ans =!= versionstr|"\n" then (
		stderr << "[SCSCPClient] Incompatible." << endl;
		error("SCSCP connection failed.");
	);
	
	s	
);

closeSCSCPConnection = s -> (close s#"fd";)

computeSCSCP = method()
computeSCSCP (SCSCPConnection, XMLnode) := (s,x) -> (

	stderr << "[computeSCSCP] Constructing procedure call..." << endl;
	if x.tag =!= "OMA" then x = OMA("fns1", "identity", {x});
	pc := OMA("scscp1", "procedure_call", { x });
	pc = setOMAttr(pc, OMS("scscp1", "call_id"), OMSTR(toString (callIDCounter = callIDCounter+1)));
	pc = setOMAttr(pc, OMS("scscp1", "option_return_object"), OMSTR(""));
	pc = OMOBJ(pc);
	pc = createOMATTRObj(pc);

	stderr << "[computeSCSCP] Sending procedure call..." << endl;
	tspc := toString toLibxmlNode pc;
	stderr << endl << tspc << endl;
	s#"fd" << "<?scscp start ?>" << endl << flush;
	s#"fd" << tspc << endl;
	s#"fd" << "<?scscp end ?>" << endl << flush;

	stderr << "[computeSCSCP] Waiting for response..." << endl;
	ans := "";
	waitfor := "(.*)<\\?scscp end \\?>\n$";
	while not match(waitfor, ans) do (
		ans = ans|(read s#"fd");
	);
	
	stderr << "[computeSCSCP] Answer received..." << endl;
	<< ans << endl;
	
	stderr << "[computeSCSCP] Parsing answer..." << endl;
	y := parse ans;
	if class(y) =!= XMLnode then
		error "Parsing failed.";
	stderr << y << endl;
		
	y
)
computeSCSCP (SCSCPConnection, Thing) := (s,x) -> (
	--When constructin an OpenMath object, we first make sure that we do not throw undeclared
	--  and possibly automatically generated ids around. I consider this a good idea, I think.
	resetDeclaredIDs();

	stderr << "[computeSCSCP] Constructing OpenMath object..." << endl;
	o := openMathValue x;
	if class(o) === XMLnode and o.tag === "OME" then (
		stderr << o << endl;
		error(concatenate("Could not convert '", toString x, "' of type '",toString class x,"'to OpenMath"));
	);
	
	a := computeSCSCP(s, o);
	
	stderr << "[computeSCSCP] Evaluating answer..." << endl;
	t := value a;

	t
)

-- This allows for s( .. something .. )
SCSCPConnection Thing := computeSCSCP


----------------------------------
---------- SERVER CODE -----------
----------------------------------


handleIncomingSCSCPConnection = sock -> (
	cid := (incomingCounter = incomingCounter + 1);
	stderr << "[handleIncomingSCSCP" << cid << "] Handling new connection" << endl;
	
	s := concatenate(
		"<?scscp service_name=", format SCSCPServiceName, 
		" service_version=", format SCSCPServiceVersion, 
		" service_id=", format "0",
		" scscp_versions=", format SCSCPProtVersion, 
		" ?>");

	stderr << "[handleIncomingSCSCP" << cid << "] Sending announcement" << endl;
	sock << s << endl << flush;

	stderr << "[handleIncomingSCSCP" << cid << "] Waiting for version request..." << endl;
	ans := "";
	waitfor := "<\\?scscp version=\"(.*)\" \\?>"; 
	while not match(waitfor, ans) do (
		ans = ans|(read sock);
	);
	hits := select(waitfor, "\\1", ans);
	if not member(hits#0, SCSCPProtCompatibleVersions) then (
		stderr << "[handleIncomingSCSCP" << cid << "] Incompatible version: '" << hits#0 << "'" << endl;
		sock << "<?scscp quit reason=\"incompatible version " << hits#0 << "\" ?>" << endl << flush;
		return;
	);

	stderr << "[handleIncomingSCSCP" << cid << "] Great! Compatible version: '" << hits#0 << "'" << endl;
	
)
startSCSCPServer = hostport -> (
	stderr << "[SCSCPServer] Listening on hostport " << hostport << endl;

	f := openListener("$"|hostport);
	--while true do (
		stderr << "[SCSCPServer] Waiting for incoming connection "  << endl;
		g := openInOut f;
		stderr << "[SCSCPServer] Incoming connection. Forking. " << endl;
		
		--Once we are here an incoming connection arrived, since openInOut blocks.
		
		--So we fork. Furthermore, to avoid zombie processes, we create children, and
		--have them create a "grandchild" immediately, and terminate. In the parent, 
		--we then wait (hopefully for a short time) for the child to terminate. Yay.
		pid := fork();
		if pid =!= 0 then (
			--parent; pid is the pid of the child.
			close g;
			r := wait pid;
			if r =!= 0 then 
				stderr << "[SCSCPServer] Child exited with nonzero exit code." << endl;
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
				handleIncomingSCSCPConnection(g);
				exit(0); --this closes g automatically as well.
			);
		);
		
	--);

	close f;
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
s = newSCSCPConnection("127.0.0.1:26133")
s(OMA("arith1", "plus", {OMI(3), OMI(4)}))
s(17)
*}




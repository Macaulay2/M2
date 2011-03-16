
----------------------------------
---------- SERVER CODE -----------
----------------------------------

serverSocket = null;
startServer = method();
startServer (String, String) := (host, port) -> (
	--Maybe we should cleanup the old socket?
	if serverSocket =!= null and class(serverSocket) === File then (
		dbgout(1) << "[Server] Cleaning up old socket." << endl;
		try close serverSocket;
	);
	
	--Workaround: Creating the first finite field takes a while because the ConwayPolynomials
	-- package needs to initialize. So, instead of having every single forked process do that,
	-- we do it now.
	GF(4);

	--Here we go...
	hostport := host|":"|port;
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
			handleInterrupts = false; --so that, on Ctrl-C, all children get eliminated.
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
startServer (ZZ) := port -> startServer ("", toString port)
startServer (String) := s -> (
	mtch := regex("^(.*)\\:(.*)$", s);
	if mtch === null then startServer(s, "26133")
	else startServer(substring(s, mtch#1#0, mtch#1#1),substring(s, mtch#2#0, mtch#2#1))
)
installMethod(startServer, () -> startServer("", "26133"));

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
		buff := read sock;
		buf = buff|"";

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
			buff = read sock;
			buf = buff|"";

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
			dbgout(2) << "[handleIncoming " << cid << "] 'ack' received" << endl;
			--take off this bit
			ans = substring(ans, mtch#0#0 + mtch#0#1);
		) else if keyw === "end" then (
			--<?scscp end ?>
			dbgout(2) << "[handleIncoming " << cid << "] 'end' received" << endl;
			
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
	--  OpenMath without a procedure call), we try an openMath x if necessary.
	
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
		ret = openMath ret;
	);
	
	dbgout(2) << "[handleProcedureCall " << cid << "] Returning response..." << endl;
	ret = createOMATTRObj OMOBJ ret;
	dbgout(5) << "[handleProcedureCall " << cid << "] " << ret << endl;
	ret = toString toLibxmlNode ret;
	dbgout(5) << "[handleProcedureCall " << cid << "] " << ret << endl;
	
	ret
)

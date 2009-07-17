needsPackage "OpenMath"
needsPackage "XML"
newPackage("SCSCP")

debug needsPackage "OpenMath" -- so that we have all global symbols of OpenMath in here.
needsPackage "XML"

SCSCPConnection = new Type of MutableHashTable
SCSCPVersion = "1.3"
callIDCounter = 0;

newSCSCPConnection = hostport -> (
	s := new SCSCPConnection;
	s#"fd" = openInOut ("$"|hostport);

	stderr << "Connecting..." << endl;
	ans := "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		while not isReady s#"fd" do nothing; 
		ans = ans|(read s#"fd");
	);

	stderr << "Received: '" << ans << "'" << endl;

	versionstr := ///<?scscp version="/// | SCSCPVersion | ///" ?>///;
	stderr << "Requesting '" << versionstr << "'" << endl;
	s#"fd" << versionstr << endl << flush;
	
	stderr << "Waiting for response to version request..." << endl;
	ans = "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		while not isReady s#"fd" do nothing; 
		ans = ans|(read s#"fd");
	);
	
	stderr << "Received: '" << ans << "'" << endl;
	
	if ans =!= versionstr|"\n" then (
		stderr << "Incompatible." << endl;
		error("SCSCP connection failed.");
	);
	
	s	
);

closeSCSCPConnection = s -> (close s#"fd";)

computeSCSCP = method()
computeSCSCP (SCSCPConnection, XMLnode) := (s,x) -> (

	stderr << "Constructing procedure call..." << endl;
	if x.tag =!= "OMA" then x = OMA("fns1", "identity", {x});
	pc := OMA("scscp1", "procedure_call", { x });
	pc = setOMAttr(pc, OMS("scscp1", "call_id"), OMSTR(toString (callIDCounter = callIDCounter+1)));
	pc = setOMAttr(pc, OMS("scscp1", "option_return_object"), OMSTR(""));
	pc = OMOBJ(pc);
	pc = createOMATTRObj(pc);

	stderr << "Sending procedure call..." << endl;
	tspc := toString toLibxmlNode pc;
	stderr << endl << tspc << endl;
	s#"fd" << "<?scscp start ?>" << endl << flush;
	s#"fd" << tspc << endl;
	s#"fd" << "<?scscp end ?>" << endl << flush;

	stderr << "Waiting for response..." << endl;
	ans := "";
	waitfor := "(.*)<\\?scscp end \\?>\n$";
	while not match(waitfor, ans) do (
		while not isReady s#"fd" do nothing; 
		ans = ans|(read s#"fd");
	);
	
	stderr << "Answer received..." << endl;
	<< ans << endl;
	
	stderr << "Parsing answer..." << endl;
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

	stderr << "Constructing OpenMath object..." << endl;
	o := openMathValue x;
	if class(o) === XMLnode and o.tag === "OME" then (
		stderr << o << endl;
		error(concatenate("Could not convert '", toString x, "' of type '",toString class x,"'to OpenMath"));
	);
	
	a := computeSCSCP(s, o);
	
	stderr << "Evaluating answer..." << endl;
	t = value a;

	t
)

-- This allows for s( .. something .. )
SCSCPConnection Thing := computeSCSCP


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



-- f = openInOut "$192.168.1.9:26133"
-- while not isReady f do nothing
-- sleep 1
-- read f
-- f << ///<?scscp version="1.3" ?>/// << endl << flush
-- while not isReady f do nothing
-- sleep 1
-- read f
-- f << ///
--   <?scscp start ?>
--   <OMOBJ>
--   <OMATTR>
--   <OMATP>
--     <OMS cd="scscp1" name="call_id"/>
--     <OMSTR>hi there</OMSTR>
--     <OMS cd="scscp1" name="option_return_object"/>
--     <OMSTR></OMSTR>
--   </OMATP>
--     <OMA>
--       <OMS cd="scscp1" name="procedure_call"/>
--       <OMA>
-- 	<OMS cd="arith1" name="plus"/>
-- 	<OMI>1</OMI>
-- 	<OMI>2</OMI>
--       </OMA>
--     </OMA>
--   </OMATTR>
--   </OMOBJ>
--   <?scscp end ?>
--   /// << endl << flush
-- 
-- 
-- to do:
-- 
--   Dan G:
--        install GAP kernel and packages
--   
--   Dan R:
  


  
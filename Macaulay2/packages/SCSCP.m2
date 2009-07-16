newPackage("SCSCP")

SCSCPConnection = new Type of MutableHashTable
SCSCPVersion = "1.3"

newSCSCPConnection = hostport -> (
	s := new SCSCPConnection;
	s.fd = openInOut ("$"|hostport);

	stderr << "Connecting..." << endl;
	ans := "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		while not isReady s.fd do nothing; 
		ans = ans|(read s.fd);
	);

	stderr << "Received: '" << ans << "'" << endl;

	versionstr := ///<?scscp version="/// | SCSCPVersion | ///" ?>///;
	stderr << "Requesting '" << versionstr << "'" << endl;
	s.fd << versionstr << endl << flush;
	
	stderr << "Waiting for response to version request..." << endl;
	ans = "";
	while #ans == 0 or ans#-1 =!= "\n" do (
		while not isReady s.fd do nothing; 
		ans = ans|(read s.fd);
	);
	
	stderr << "Received: '" << ans << "'" << endl;
	
	if ans =!= versionstr|"\n" then (
		stderr << "Incompatible." << endl;
		error("SCSCP connection failed.");
	);
	
	s	
);

--closeSCSCPConnection = s -> close s.fd;


--computeSCSCP = (s, q)

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
  
  
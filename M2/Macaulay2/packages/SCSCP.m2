newPackage(
	"SCSCP",
	Version => "0.1", 
	Date => "July 17, 2009",
	Authors => {
		{Name => "Dan Roozemond", Email => "d.a.roozemond@tue.nl", HomePage => "http://www.win.tue.nl/~droozemo/"}
	},
	Headline => "SCSCP for Macaulay2",
	DebuggingMode => true,
	AuxiliaryFiles => true
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
---------- THE FILES -------------
----------------------------------

load "./SCSCP/remobjs.m2"
load "./SCSCP/client.m2"
load "./SCSCP/server.m2"
load "./SCSCP/expr.m2"
load "./SCSCP/docs.m2"

---------------
----THE END----
---------------
endPackage("SCSCP");

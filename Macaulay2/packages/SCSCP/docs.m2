-- TODO
-- * Write documentation and nice examples about all the fun we can have with remoteObject

------------------------
----DOCUMENTATION-------
------------------------
export { "SCSCPConnection", "newConnection", "startServer", "remoteObject" }

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
	EXAMPLE { PRE ///
i2 : s = newConnection("127.0.0.1", 26133);

i3 : s(hold(2)+3)

o3 = 5

i4 : close s
/// },

	"We could also explicitly have a look at the openMath that's being passed around",
	EXAMPLE { PRE ///
i2 : s = newConnection "127.0.0.1"

o2 = SCSCPConnection{...1...}

o2 : SCSCPConnection

i3 : o = openMath (hold(2)+3)

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

/// },

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
	EXAMPLE { PRE ///
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
	}



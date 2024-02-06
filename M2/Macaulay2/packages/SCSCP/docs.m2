------------------------
----DOCUMENTATION-------
------------------------
export { "SCSCPConnection", "newConnection", "startServer", "RemoteObject" }

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
	Key => { SCSCPConnection },
	Headline => "The class of all SCSCP connections"
}

document {
	Key => { (symbol <==,SCSCPConnection,Thing), (symbol <===,SCSCPConnection,Thing), (symbol SPACE, SCSCPConnection, Thing) },
	Headline => "Execute computations using SCSCP",
	Usage => "s x",
	Inputs => { 
		"s" => SCSCPConnection => {"The server that should compute it"},
		"x" => Thing => {"The expression to be computed "}
	},
	Outputs => { Thing => "The result of the computation" },
	"As an example, we connect to a locally running SCSCP server: ",
	EXAMPLE { PRE get (currentFileDirectory|"docinput/gap1.out") },

	"We could also explicitly have a look at the OpenMath that's being passed around",
	EXAMPLE { PRE get (currentFileDirectory|"docinput/gap2.out") },

	"Another syntax offered is using the <== and <=== operators. The first of these denotes
	a computation that returns the computed object, whereas the second denotes a computation
	that returns a reference (i.e. a remote object). Fortunately, these operators have lower
	parsing precedence than most others, so few parentheses will be required.",
	EXAMPLE { PRE get (currentFileDirectory|"docinput/magmaremobj.out") },
	SeeAlso => { newConnection, (symbol SPACE, Manipulator, SCSCPConnection), RemoteObject }	
 	}


document {
	Key => { startServer, 1:startServer, (startServer, String), (startServer, ZZ), (startServer, String, String) },
	Headline => "Start an SCSCP server",
	Usage => "startServer (host, port)",
	Inputs => { 
		"host" => String => {"The IP address of the interface to bind to (may be omitted, and defaults to binding to all interfaces) "}, 
		"port" => {ofClass{String, ZZ}, ", providing the port number (defaults to 26133) "}
	},
	"The server will keep running indefinitely; it may be stopped by sending a Ctrl-C. Furthermore,
	the server forks for every new incoming connection, so that it can serve many clients simultaneously.
	The amount of output printed to the screen is determined by the value of debugLevel.",
	EXAMPLE { PRE ///
i1 : debugLevel = 2;

i2 : startServer(26137)
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

document {
	Key => { RemoteObject },
	Headline => "The class of all remote SCSCP objects",
	"As an example, we store three polynomials on a remote server, compute their product both locally and
	remotely, and then ask the remote server whether the results are equal. Note that <== and <=== may be
	used without their first argument if no confusion can arise about the SCSCP server where the 
	computation should take place.",
	EXAMPLE { PRE get (currentFileDirectory|"docinput/gappol.out") },
    "We create matrices in Macaulay2 and compute the order of the group they generate in GAP.
    Note that you may have to set 'SCSCPserverAcceptsOnlyTransientCD := false;' in your GAP configuration
    (particularly scscp/config.g) in order for this example to work.",
	EXAMPLE { PRE get (currentFileDirectory|"docinput/gapmats.out") },
	SeeAlso => { newConnection, (symbol SPACE, Manipulator, SCSCPConnection), (symbol SPACE, SCSCPConnection, Thing) }	

}
 

undocumented { (identifyRemoteObjects, SCSCPConnection, XMLnode) }
undocumented{ (symbol *,RemoteObject,RemoteObject) }
undocumented{ (symbol *,RemoteObject,Thing) }
undocumented{ (symbol +,RemoteObject,RemoteObject) }
undocumented{ (symbol +,RemoteObject,Thing) }
undocumented{ (symbol -,RemoteObject,RemoteObject) }
undocumented{ (symbol -,RemoteObject,Thing) }
undocumented{ (symbol /,RemoteObject,RemoteObject) }
undocumented{ (symbol /,RemoteObject,Thing) }
undocumented{ (symbol ==,RemoteObject,RemoteObject) }
undocumented{ (symbol ==,RemoteObject,Thing) }
undocumented{ (symbol and,RemoteObject,RemoteObject) }
undocumented{ (symbol and,RemoteObject,Thing) }
undocumented{ (symbol or,RemoteObject,RemoteObject) }
undocumented{ (symbol or,RemoteObject,Thing) }
undocumented{ (symbol *,Thing,RemoteObject) }
undocumented{ (symbol +,Thing,RemoteObject) }
undocumented{ (symbol -,Thing,RemoteObject) }
undocumented{ (symbol /,Thing,RemoteObject) }
undocumented{ (symbol ==,Thing,RemoteObject) }
undocumented{ (symbol and,Thing,RemoteObject) }
undocumented{ (symbol or,Thing,RemoteObject) }
undocumented{ (openMath,RemoteObject) }
undocumented{ (size,RemoteObject) }

undocumented{ (net,SCSCPConnection) }
undocumented{ (symbol ===>,Thing,SCSCPConnection) } 
undocumented{ (symbol ==>,Thing,SCSCPConnection) } 
undocumented{ (symbol <==,RemoteObject) } 
undocumented{ (symbol <===,RemoteObject) } 
undocumented{ (net,RemoteObject) }

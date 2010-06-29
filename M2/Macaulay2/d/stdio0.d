use arithmetic;
use nets;
use pthread0;

export ERROR ::= -1;
export NOFD ::= -1;
export EOF ::= -2;					    -- end of file
export STDIN ::= 0;
export STDOUT ::= 1;
export STDERR ::= 2;

export iseof      (c:int ):bool := c == EOF;
export iserror    (c:int ):bool := c == ERROR;

export fileOutputSyncState :=
{+
     	-- output file stuff
	outbuffer:string,	-- buffer
	                        -- outbuffer . 0 is the first char in the buffer
	outindex:int,	        -- outbuffer.(outindex-1) is the last char
	outbol:int,	        -- outbuffer.outbol = first char of the current line
	     	       	        -- The text after this point may be combined with
				-- subsequently printed nets.
        hadNet:bool,		-- whether a Net is present, in which case the
	     	       	        -- buffer will be empty
	nets:NetList,	        -- list of nets, to be printed after the outbuffer
        bytesWritten:int,       -- bytes written so far
	lastCharOut:int        -- when outbuffer empty, last character written, or -1 if none

};

export newFileOutputSyncState(
 	outbuffer:string,	-- buffer
	                        -- outbuffer . 0 is the first char in the buffer
	outindex:int,	        -- outbuffer.(outindex-1) is the last char
	outbol:int,	        -- outbuffer.outbol = first char of the current line
	     	       	        -- The text after this point may be combined with
				-- subsequently printed nets.
        hadNet:bool,		-- whether a Net is present, in which case the
	     	       	        -- buffer will be empty
	nets:NetList,	        -- list of nets, to be printed after the outbuffer
        bytesWritten:int,       -- bytes written so far
	lastCharOut:int         -- when outbuffer empty, last character written, or -1 if none
):fileOutputSyncState := (
fileOutputSyncState(outbuffer,outindex,outbol,hadNet,nets,bytesWritten,lastCharOut)
);

export file := {+
        -- general stuff
     	hash:int,     	   	-- hash code
	filename:string,	-- name of file
	pid:int,	        -- pid if it's a pipe or pair of pipes to a child process, else 0
        error:bool,             -- a system call returned ERROR
	errorMessage:string,    -- the error message associated to the system call that returned ERROR
	-- listener stuff
        listener:bool,	   	-- is a listener
	listenerfd:int,	    	-- file descriptor of listener, or -1
	connection:int,	   	-- file descriptor of accepted connection, not made into file yet
	numconns:int,	        -- count connections accepted
     	-- input file stuff
     	input:bool,	        -- is input file
	infd:int,		-- file descriptor or -1
        inisatty:bool,
	inbuffer:string,        -- buffer
	inindex:int,		-- inbuffer.inindex is the first available char
        insize:int,		-- inbuffer.(insize-1) is the last char
				-- we always have inindex <= insize
	eof:bool,		-- last read got 0 characters (still might have some chars in inbuffer)
        promptq:bool,           -- whether to prompt and to reward for input
	prompt:function():string, -- function to call to get prompt string when input is required
        reward:function():string, -- function to call to get reward string when input has been obtained
	fulllines:bool,		-- whether to read at least up to the next newline each time input is required
        bol:bool,     	   	-- at the beginning of a line, and a prompt is needed
	echo:bool,     	   	-- whether to echo characters read to corresponding output file
	echoindex:int,	        --   inbuffer.echoindex is the next character to echo
        readline:bool,           -- input handled by readline()
     	output:bool,	        -- is output file
	outfd:int,		-- file descriptor or -1
        outisatty:bool,
	unsyncOutputState:fileOutputSyncState, -- default sync state to use for unsync output
	threadOutputMode:int,		-- current thread output mode.  0 is unsync, 1 is sync, 2 is thread exclusive
	threadInputMode:int, 		-- current thread input mode.  1 is unsync, 2 is thread exclusive
	threadSyncMutex:ThreadMutex -- Mutex for syncronization and for buffering
	
	};

export PosFile := {+ file:file, lastchar:int, filename:string, line:ushort, column:ushort };


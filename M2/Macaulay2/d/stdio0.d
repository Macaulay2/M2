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

bufsize ::= 4 * 1024;
newbuffer():string := new string len bufsize do provide ' ';
export newDefaultFileOutputSyncState():fileOutputSyncState := ( newFileOutputSyncState(newbuffer(),0,0,false,dummyNetList,0,-1) );
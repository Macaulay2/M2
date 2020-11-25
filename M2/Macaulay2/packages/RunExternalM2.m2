-- -*- coding: utf-8 -*-
-*
  Copyright 2016 Brian Pike

  You may redistribute this file under the terms of the GNU General Public
  License as published by the Free Software Foundation, either version 2 of
  the License, or any later version.
*-
newPackage(
		"RunExternalM2",
		Version => "0.83",
		Date => "January 06, 2016",
		Authors => {
			{Name => "Brian Pike", 
			Email => "bapike@gmail.com",
			HomePage => "http://www.brianpike.info/"}},
		Headline => "run Macaulay2 functions outside the current Macaulay2 process",
		Keywords => {"Miscellaneous"},
		DebuggingMode => false,
		Configuration => {"isChild"=>false} 
	)

-*
Changelog:
 v0.83:
  o Remove runExternalM2InClone because it is fundamentally unsafe to use
    fork() in a multithreaded application like M2 (github issue #355) 
  o Improve documentation
 v0.82:
  o Remove a workaround for github issue #296, fixed in M2 v1.8
  o Change default PreRunScript
  o Make the default KeepStatisticsCommand try to detect which time implementation to use
 v0.81:
  o remove setExternalM2Child() and use Configuration option instead
  o change default M2Location to null so that documentation looks OK
  o use toAbsolutePath when finding default M2Location
 v0.8:
  o initial release

Known issues:
 o Untested on OS X or Cygwin
 o The method used to discover the location of M2 can fail.  If you get an
   error about libgmp from the child M2 process, then try initially
   starting the parent M2 by using its full path.
*-


-*
  How RunExternalM2 Works:
  Call:
    runExternalM2("filename.m2","name-of-procedure" (string or symbol), parameters of that procedure)
  This proc will spawn a new M2 process in such a way that we may:
    keep track of stdout, stderr, running time, memory usage, and M2's return value
  and execute something like:
    loadPackage("RunExternalM2",Configuration=>{"isChild"=>true},Reload=>true);
    load "filename.m2";
    runExternalM2ReturnAnswer("blargh.ans",name-of-procedure(parameters));
  where runExternalM2ReturnAnswer will print to blargh.ans 
    the value returned by name-of-procedure, serialized into a string with toExternalString or the like.
  Then runExternal parses the stdout and returns a hash with all of this data.

  Motivation:  ulimits apply to processes, not threads, and
     using try and alarm seems buggy when executing code in a library
     (e.g., alarms that would ring in the middle of computing a groebner basis
     seem to not work.) 

  Possible Methods:
    - forking
      No.  M2 is a multithreaded process, and it is unsafe to do almost
      anything after forking, even if only one thread ever runs.
    - run()ing a new M2 process
      Yes, how we actually do it.
*-


-*
TODO:
 - Test on Cygwin?
*-  


export {
	"isExternalM2Child",
	"isExternalM2Parent",
	"runExternalM2",
	"runExternalM2ReturnAnswer",
	-- Various Options:
	"M2Location",
	"KeepStatistics",
	"KeepStatisticsCommand",
	"PreRunScript"
};

--exportMutable {};

mydoc:="";

mydoc=concatenate(mydoc,///
Node
	Key
		RunExternalM2
	Headline
		a package to run Macaulay2 code externally and retrieve the result
	Description
		Text
			This package allows the user to run a Macaulay2 function in
			a brand-new instance of Macaulay2, wait for the instance to finish,
			and automatically retrieve any results.
			This is primarily done with the @TO runExternalM2@ function.

			One of the main purposes of this package is to allow the use of
			operating system features (e.g., ulimit on Unix-like systems) to
			restrict the resources (e.g., memory, running time) that the
			Macaulay2 function is allowed to use; see @TO "resource limits"@.
			This is particularly useful if you want a computation to give up
			after a certain amount of time, as an alternative to using
			@TO alarm@ and @TO "try"@.

			For example, suppose you want to find an example of an ideal that
			has a particular property, and the time required to check for
			this property can vary enormously.
			First write a Macaulay2 function {\tt foo()} that generates a random
			ideal {\tt I} and checks whether {\tt I} has this property.
			Then use a loop to repeatedly call {\tt foo()} using @TO runExternalM2@,
			with a time limit of 20 seconds, until such an ideal is found;
			now let it run overnight.

			For examples, please see @TO runExternalM2@ or
			@TO "suggestions for using RunExternalM2"@.
	SeeAlso
		runExternalM2
		"suggestions for using RunExternalM2"
		"resource limits"
///);

mydoc=concatenate(mydoc,///
Node
	Key
		"suggestions for using RunExternalM2"
	Headline
		suggestions for using RunExternalM2
	Description
		Text
			A suggested way of writing programs using this package
			is to write your program in a file,
			perhaps named {\tt "somefilename.m2"},
			with the following structure: 
		Pre
			needsPackage "RunExternalM2"

			...
			(ring definitions, etc., here)
			...
			

			mycalc = (...) -> (
				...
				(do a calculation)
				...
			);

			if isExternalM2Parent() then (
				...
				h=runExternalM2("somefilename.m2",mycalc,...);
				...
			);
		Text
			and then execute your program in Macaulay2 by running:
		Pre
			i1 : load "somefilename.m2"
		Text

			This has a few advantages.
			It ensures that ring definitions are always the same in the parent
			and child Macaulay2 processes, even if you are frequently editing
			the file and running the calculation again.
			If the file containing the function has syntax errors,
			then they should appear directly in the console rather than inside
			a temporary file containing the program's output.
			If a particular try at execution does not work, then you can immediately call
			the function directly for quick debugging.

			For instance, the following code calculates an ideal of minors:
		Pre
			needsPackage "RunExternalM2"

			m=4;
			n=3;
			R=QQ[x_(1,1)..x_(m,n)];

			idealOfMinors = (i) -> (
				M:=genericMatrix(R,m,n);
				return trim minors(i,M);
			);

			if isExternalM2Parent() then (
				h=runExternalM2("example1.m2",idealOfMinors,2);
				if (h#"exit code"===0) then (
					stdio<<"The code ran successfully and the answer was:"<<endl;
					stdio<<toExternalString(h#value)<<endl;
				) else (
					stderr<<"There was a problem."<<endl;
				);
			);
		Text

			When using random numbers, it may be useful for the parent to
			provide the random number seed to the child, so that the output of
			the child is reproducible.
			For instance, the following code searches for a random integer matrix
			with a large determinant:
		Pre
			needsPackage "RunExternalM2"

			getRandomMat = (seed,size,height) -> (
				setRandomSeed(seed);
				blankMat:=mutableMatrix(ZZ,size,size);
				return matrix fillMatrix(blankMat,Height=>height);
			);

			if isExternalM2Parent() then (
				seed:=0;
				while (true) do (
					seed=random(0,2^32);
					h:=runExternalM2("z1.m2",getRandomMat,(seed,5,5));
					if (h#"exit code"===0) then (
						d:=det(h#value);
						stdio<<"Starting with seed "<<seed<<" gives this matrix (with det="<<d<<"):"<<endl;
						stdio<<"  "<<h#value<<endl;

						if (d>100) then (
							break;
						);
					) else (
						stderr<<"There was a problem; aborting."<<endl;
						break;
					);
				);

				stdio<<"Using the seed "<<seed<<" again gives the same matrix:"<<endl;
				stdio<<"  "<<getRandomMat(seed,5,5)<<endl;
			);
	SeeAlso
		RunExternalM2
		runExternalM2
		isExternalM2Parent
///);


--- Some generic documentation on resource limitations
mydoc=concatenate(mydoc,///
Node
	Key
		"resource limits"
	Headline
		how to use resource limits with Macaulay2
	Description
		Text
			Many operating systems support limiting the amount of resources
			(for instance, memory or CPU time) that a given instance of a
			running program (``process'') may use.
			Processes that use more than the alloted resource may have requests
			denied (as in the case of memory or disk usage),
			or may be terminated.
			In some cases it is useful to apply these limits to Macaulay2.

			In Unix-like operating systems (including Linux and OS X)
			these limits are known as ``ulimits'' or ``rlimits''.
			Users typically set these limits using a built-in shell command called
			{\tt ulimit} or similar; for help, consult the documentation
			for your shell (e.g., run {\tt man bash}).
			In particular, the units of any limits can vary from system to system.
			There are two types of limits, {\it hard} and {\it soft}:
			soft levels are always less than or equal to the hard limits, and
			hard levels can only be adjusted down.
			When a process creates a child process, the
			{\bf child process
			receives the ulimits of the parent process}.

			As an example, we give commands for a Linux system using a Bash shell.
			To {\bf view} the current limits, run:
		Pre
			ulimit -a
		Text
			To {\bf set} soft ulimits on your shell of 123456 kilobytes of memory
			and 5 seconds of CPU time, run:
		Pre
			ulimit -S -m 123456 -S -t 5
		Text
			Because ulimits are inherited by child processes, any commands run
			later in your shell session (say, Macaulay2) will inherit these ulimits.
			Perhaps the {\bf best way to set} the limits on a specific process is to
			run something like
		Pre
			(ulimit -S -m 123456 -S -t 5; M2)
		Text
			which will run M2 with these limits while leaving the shell's ulimits unchanged.

			{\bf From within Macaulay2}, you can {\bf view} the ulimits currently available
			to the Macaulay2 process by using the @TO run@ command:
		Example
			run("ulimit -a")
		Text
			-- M2/M2/Macaulay2/d/scclib.c: system_run
			This starts a new shell
			--(which may differ from your user's shell!)
			and executes the command given, which in this
			case provides the list of ulimits of the shell.
			Since ulimits are inherited, this should be the same as
			the ulimits of Macaulay2 itself.

			{\bf From within Macaulay2}, it is not possible to set ulimits on the
			current Macaulay2 process because Macaulay2 
			does not provide access to the {\tt setrlimit} system call
			(other than the @TO limitFiles@ and @TO limitProcesses@ commands).
			However, it is possible to {\bf set} ulimits on child Macaulay2
			processes that are started by @TO runExternalM2@,
			by using the @TO PreRunScript@ option of @TO runExternalM2@.

--			As of 2012, Windows and Cygwin do not support the 
--			{\tt setrlimit} system call, and so on these systems {\tt ulimit} is
--			not functional.
--			There is a different interface, Job Objects, present in Windows,
--			but I am
--			unaware if there is any way to use this for Macaulay2.
--			--TODO 
	SeeAlso
		alarm
		"try"
		[runExternalM2,PreRunScript]
///);


-*
  These are the options to automatically provide when calling the M2 executable.
  We would use --script (=--stop --no-debug --silent -q ), but we do NOT want
  -q so that our child processes can find installed packages, namely, this package,
  unless the parent M2 was also called with -q.
*-
M2Options:=" --stop --no-debug --silent ";
debug Core
if noinitfile then M2Options = M2Options | " -q ";

mydoc=concatenate(mydoc,///
Node
	Key
		isExternalM2Child
	Headline
		indicate if this process is a child process or not
	Usage
		b=isExternalM2Child()
	Outputs
		b:Boolean
			true if this process is a child process, and false otherwise
	Description
		Text
			This function indicates whether the currently-running process is a
			child process as created by the @TO RunExternalM2@ package, or not.
	SeeAlso
		RunExternalM2
		isExternalM2Parent
///);
isExternalM2Child = () -> (
	v:=(options RunExternalM2).Configuration#"isChild";
	if not(instance(v,Boolean)) then (
		error "RunExternalM2: package option isChild should be a boolean";
	);
	return v;
);

mydoc=concatenate(mydoc,///
Node
	Key
		isExternalM2Parent
	Headline
		indicate if this process is a parent process or not
	Usage
		b=isExternalM2Parent()
	Outputs
		b:Boolean
			true if this process is a parent process, and false otherwise
	Description
		Text
			This function indicates whether the currently-running process is a 
			parent process, or not, from the point-of-view of the
			@TO RunExternalM2@ package.
	SeeAlso
		RunExternalM2
		isExternalM2Child
///);
isExternalM2Parent = () -> (
	v:=(options RunExternalM2).Configuration#"isChild";
	if not(instance(v,Boolean)) then (
		error "RunExternalM2: package option isChild should be a boolean";
	);
	return not(v);
);


mydoc=concatenate(mydoc,///
Node
	Key
		runExternalM2ReturnAnswer
	Headline
		stores a Macaulay2 variable's value in a file, to later be read by the RunExternalM2 package
	Usage
		runExternalM2ReturnAnswer(f,t)
	Inputs	
		f:String
			the filename
		t:Thing
			the variable to store
	Description
		Text
			This function tries to serialize {\tt t} into the file {\tt f}, that is,
			convert it into a standard format for storage.
			In this case, we convert {\tt t} into a string using
			@TO toExternalString@.
			This function is not intended for public use.
	SeeAlso
		RunExternalM2
		toExternalString
///);
runExternalM2ReturnAnswer = (f,t) -> (
	assert(instance(f,String));
	try (
		file:=openOut(f);
		s:=toExternalString(t);
		file<<s<<flush<<close;
	) else (
		error "runExternalM2ReturnAnswer: failed to store output! Return type unhandled by toExternalString? Disk full or read-only?";
	);
);


-- Try to read the answer from a file.
-- Return a pair consisting of the answer and a boolean indicating
-- whether the job was successful enough that a sensible thing
-- was written to the file. 
-- Internal only
parseAnswer := (fname) -> (
	if not(fileExists(fname)) then (
		stderr<<"RunExternalM2: expected answer file does not exist"<<endl;
		return (null,false);
	);
	-- This stores the whole output file in memory, which is suboptimal. 
	contents:=try (
		get(fname)
	) else (
		stderr<<"RunExternalM2: could not read answer file"<<endl;
		return (null,false);
	);

	-- The file should just contain a M2 object
	rv:=null;
	wassuccess:=false;
	try (
		rv=value(contents);
		wassuccess=true;
	) else (
		error("RunExternalM2: error parsing contents of answer file: "|contents);
		rv=null;
	);

	return (rv,wassuccess);
);

-- error checking around removing files
-- Internal only
safelyRemoveFile := (s,f) -> (
	try (
		removeFile(f);
	) else (
		stderr<<s<<" "<<f<<endl;
	);
);


mydoc=concatenate(mydoc,///
Node
	Key
		[runExternalM2,KeepFiles]
	Headline
		indicate whether or not temporary files should be saved
	Description
		Text
			If {\tt true}, then always keep any files that arise during the use of
			@TO runExternalM2@.

			If {\tt false}, then always delete these files.

			If {\tt null} (the default), then keep the files only if the job was a
			failure, meaning
			that an understandable result was not able to be read from the temporary
			file storing the results of the computation.
			If the job was unsuccessful (e.g., exit with nonzero error code, or
			the process was killed by the operating system) then the files will be
			saved to aid investigation. 
	Caveat
		All of the files involved were created using @TO temporaryFileName@ in the
		parent Macaulay2 process, and hence will always be deleted upon termination
		of the parent, no matter this setting.
	SeeAlso
		RunExternalM2
		runExternalM2
///);

mydoc=concatenate(mydoc,///
Node
	Key
		runExternalM2
		[runExternalM2,KeepStatistics]
		[runExternalM2,M2Location]
		KeepStatistics
		M2Location
	Headline
		run a Macaulay2 function in a new Macaulay2 process
	Usage
		h=runExternalM2(fname,func,params)
	Inputs	
		fname:String
			the name of the file that contains the function to run
		func:String
			the function to run.  Can also be a named @TO Function@ or @TO Symbol@.
		params:Thing
			the parameters for {\tt func}, usually a @TO Sequence@
		M2Location=>String
			the location of the M2 executable.  If {\tt null}, then the absolute
			path of the first entry of @TO "commandLine"@ is used.
		KeepStatistics => Boolean
			whether to keep statistics on the resources used during execution
	Outputs
		h:HashTable
			contains the output of {\tt func} and other data
	Description
		Text
			This function starts a short-lived new (``child'') Macaulay2 process,
			loads the file {\tt fname},  
			runs the function {\tt func} with the parameters {\tt params},
			captures the value returned by {\tt func},
			and stores it inside {\tt h} in the original Macaulay2 process.
			Optionally, strict @TO "resource limits"@ may be imposed on the
			child process from within Macaulay2, or data may be collected
			about the resources used by the child process.

			Since the child is a new Macaulay2 process, it has no defined
			variables or functions except those defined in {\tt fname}.
			Hence, {\tt func} and anything it needs (e.g., ring definitions)
			must be defined in the file {\tt fname}.

			The hash table {\tt h} stores
			the {\tt exit code} of the created Macaulay2 process,
			the {\tt return code} of the created Macaulay2 process
			(see @TO run@ for details; this is usually 256 times the exit code, plus
			information about any signals received by the child),
			the wall-clock {\tt time used} (as opposed to the CPU time),
			the name of the {\tt output file} (unless it was deleted),
			the name of the {\tt answer file} (unless it was deleted),
			any {\tt statistics} recorded about the resource usage,
			and the {\tt value} returned by the function {\tt func}.
			If the child process terminates abnormally, then usually
			the {\tt exit code} is 
			nonzero and the {\tt value} returned is @TO null@. 

			For example, we can write a few functions to a temporary file:
		Example
			fn=temporaryFileName()|".m2"
			fn<<//// square = (x) -> (stderr<<"Running"<<endl; sleep(1); x^2); ////<<endl;
			fn<<//// justexit = () -> ( exit(27); ); ////<<endl;
			fn<<//// spin = (x) -> (stderr<<"Spinning!!"<<endl; startTime:=cpuTime(); while(cpuTime()-startTime<x) do for i to 10000000 do i; return(x);); ////<<endl;
			fn<<flush;
-- TODO: there seems to be a bug where the line count is wrong if I use a multi-line string with ////
		Text
			and then call them:
		Example
			h=runExternalM2(fn,"square",(4));
			h
			h#value===4^2
			h#"exit code"===0
		Text

			An abnormal program exit will have a nonzero 
			{\tt exit code}; also, the {\tt value} will be null,
			the {\tt output file} should exist,
			but the {\tt answer file} may not exist unless
			the routine finished successfully.
		Example
			h=runExternalM2(fn,"justexit",());
			h
			fileExists(h#"output file")
			fileExists(h#"answer file")
		Text
			Here, we use @TO "resource limits"@ to limit the
			routine to 2 seconds of computational time,
			while the system is asked to use 10 seconds of computational time:
		Example
			h=runExternalM2(fn,"spin",10,PreRunScript=>"ulimit -t 2");
			h
			if h#"output file" =!= null and fileExists(h#"output file") then get(h#"output file")
			if h#"answer file" =!= null and fileExists(h#"answer file") then get(h#"answer file")
		Text

			We can get quite a lot of detail on the resources used
			with the @TO KeepStatistics@ command: 
		Example
			h=runExternalM2(fn,"spin",3,KeepStatistics=>true);
			h#"statistics"
		Text

			We can handle most kinds of objects as return values,
			although @TO MutableMatrix@ does not work.
			Here, we use the built-in @TO identity@ function:
		Example
			v=//// A complicated string^%&C@#CERQVASDFQ#BQBSDH"' ewrjwklsf////;
			(runExternalM2(fn,identity,v))#value===v
		Text
			Some care is required, however:
		Example
			R=QQ[x,y];
			v=coker random(R^2,R^{3:-1})
			h=runExternalM2(fn,identity,v)
		Text
			To view the error message:
		Example
			get(h#"output file")
		Text
			Keep in mind that the object you are passing must make sense
			in the context of the file containing your function!
			For instance, here we need to define the ring:
		Example
			fn<<////R=QQ[x,y];////<<endl<<flush;
			(runExternalM2(fn,identity,v))#value===v			
		Text
			This problem can be avoided by following some
			@TO "suggestions for using RunExternalM2"@.

			The objects may unavoidably lose some internal references, though:
		Example
			v=R;
			h=runExternalM2(fn,identity,v);
			h#value
			v===h#value
		Text
			but this happens because
		Example
			R===value(toExternalString(R))
	SeeAlso
		RunExternalM2
		"suggestions for using RunExternalM2"
///);
mydoc=concatenate(mydoc,///
Node
	Key
		KeepStatisticsCommand
		[runExternalM2,KeepStatisticsCommand]
	Headline
		an option that tells how to collect statistics
	Description
		Text
			This value of this option is a function
			that takes a filename {\tt f} and
			a command string {\tt c} as its two parameters,
			and returns a string that, when @TO run@, will execute {\tt c}
			and put statistics in {\tt f}.

			The default function should work in most cases.
			It first tries to use {\tt /usr/bin/time}, 
			then the {\tt time} implementation built-in to certain shells,
			and otherwise makes the feature unavailable.
			Certain implementations of {\tt /usr/bin/time} are given special
			flags to provide more details.

	SeeAlso
		RunExternalM2
		runExternalM2
///);
defaultKeepStatsCommand := (f,c) -> (
	-- Every POSIX-y system should have a 'time' command, which may be
	-- a shell internal or its own executable, and should follow:
	--   http://pubs.opengroup.org/onlinepubs/9699919799/utilities/time.html

	whichTime:="";
	-- Prefer the executable.
	if fileExecutable("/usr/bin/time") then (
		-- Detect certain implementations to request more details.
		-- Note: get("!command-with-nonzero-exit-status") produces an error

		tf:=temporaryFileName();
		if run("/usr/bin/time --version > \""|tf|"\" 2>&1")==0 and match("GNU",get(tf)) then (
			-- GNU Time (version is on stderr)
			whichTime="/usr/bin/time --verbose";
		) else if fileExecutable("/usr/bin/sw_vers") and run("/usr/bin/sw_vers -productName > \""|tf|"\" 2>&1")==0 and match("OS X",get(tf)) then (
			-- OS X's time
			whichTime="/usr/bin/time -l";
		) else (
			whichTime="/usr/bin/time";
		);
	) else if run("(time sh -c \"\") >/dev/null 2>&1")==0 then (
		-- Then the shell internal probably exists
		whichTime="time";
	) else (
		-- No executable, no shell internal.  Dash doesn't have one.
		return " (echo \"Statistics not supported on this system\">\""|f|"\"; "|c|")";
	);
	-- Per the standard, time writes usage details to stderr.  Use a
	-- technique suggested there to separate the usage details from
	-- the command's output.
	-- The parantheses are necessary for bash's builtin time command
	return " ("|whichTime|" sh -c '"|c|"') >\""|f|"\" 2>&1";
);

mydoc=concatenate(mydoc,///
Node
	Key
		PreRunScript
		[runExternalM2,PreRunScript]
	Headline
		a way to impose resource limits
	Description
		Text
			This option's value is a string containing a shell command
			that can impose
			any @TO "resource limits"@ on the Macaulay2 process started by
			@TO runExternalM2@.

			By default, it is set to the shell command
		Pre
			true
		Text
			but you can set it to things like
		Pre
			ulimit -t 5
		Text
			(to limit CPU time to 5 seconds) or something more complicated:
		Pre
			ulimit -S -m 654243 && ulimit -H -t 50
		Text

			In all cases, it should be a single command such that appending
		Pre
			&& other_command
		Text
			results in a valid shell command.
	SeeAlso
		RunExternalM2
		runExternalM2
		"resource limits"
///);

runExternalM2 = {
	M2Location => null,
	KeepFiles => null,
	KeepStatistics => false,
	KeepStatisticsCommand => defaultKeepStatsCommand,
	PreRunScript => "true"
} >> opt -> (fname, proc, params) -> (
	-- Validate options
  M2Loc:=opt#M2Location;
	if not(instance(M2Loc,String) or M2Loc===null) then (
		error "runExternalM2: M2Location must be a string, or null";
	);	
	if (M2Loc===null) then (
		-- TODO, do we need rootPath for Cygwin?
		M2Loc=toAbsolutePath(first(commandLine));
	);
	if not(fileExists(M2Loc)) then (
		error "runExternalM2: error finding location of M2 program; check M2Location option";
	);
	if not(instance(opt#KeepFiles,Boolean) or opt#KeepFiles===null) then (
		error "runExternalM2: invalid KeepFiles setting";
	);
	if not(instance(opt#KeepStatistics,Boolean)) then (
		error "runExternalM2: KeepStatistics must be true or false";
	);
	if not(instance(opt#KeepStatisticsCommand,Function)) then (
		error "runExternalM2: invalid setting for KeepStatisticsCommand";
	);
	statsCommand:=opt#KeepStatisticsCommand;
	if (opt#KeepStatistics===false) then (
		statsCommand=(f,c) -> c;
	);
	if not(instance(opt#PreRunScript,String)) then (
		error "runExternalM2: PreRunScript must be a String";
	);

	-- Validate parameters
	if not(instance(fname,String)) then (
		error "runExternalM2: first parameter should be a string containing a filename";
	);
	procString:="";
	if (instance(proc,String)) then (
		procString=proc;
	) else if (instance(proc,Function)) then (
		-- "i->i^2" is a Function, but neither toString nor toExternalString gives a
		-- meaningful answer; the latter gives an error.
		try (
			procString=toExternalString(proc);
		) else (
			error "runExternalM2: second parameter must be a named, non-anonymous function.";
		);
	) else if (instance(proc,Symbol)) then (
		procString=toString proc;
	) else (
		error "runExternalM2: second parameter has unknown type";
	);
	paramString:="";
	if (instance(params,Sequence)) then (
		paramString=toExternalString(params);
	) else if (instance(params,Thing)) then (
		paramString="("|toExternalString(params)|")";
	) else (
		-- Since everything in M2 is a Thing, this should never execute.
		error "runExternalM2: third parameter's type is unhandled";
	);

	-- Setup temporary files
	base:=rootPath|temporaryFileName();
	outFileName:=base|".out";
	statFileName:=base|".stat";
	answerFileName:=base|".ans";
	scriptFileName:=base|".m2";

	-- Build the invocation string. Can look like:
	--   (echo -n && (/usr/bin/time --verbose -o time.txt /usr/bin/M2 < in.m2 > out.txt 2>&1))
	--   (echo -n && (/usr/bin/M2 < in.m2 > out.txt 2>&1))
	runString:="("|opt#PreRunScript|" && ("|
		statsCommand(
			statFileName,
			M2Loc|" "|M2Options|" <\""|scriptFileName|"\" >\""|outFileName|"\" 2>&1"
		)|" ))";

	-- Write the script
	try (
		scriptFile:=openOut(scriptFileName);
		scriptFile<<"-- Script "|scriptFileName|" automatically generated by RunExternalM2"<<endl;
		scriptFile<<"needsPackage(\"RunExternalM2\",Configuration=>{\"isChild\"=>true});"<<endl;
		scriptFile<<"load "<<toExternalString(fname)<<";"<<endl;
		scriptFile<<"runExternalM2ReturnAnswer("<<toExternalString(answerFileName)<<","<<procString<<" "<<paramString<<");"<<endl;
		scriptFile<<"exit();"<<endl<<close;
	) else (
		error "runExternalM2: error writing to script file";
	);

	-- Run the code and wait for it to finish.
	stderr<<"Running "<<runString<<endl;
	timer:=currentTime();
	returncode:=run(runString);
	timer=currentTime()-timer;
	stderr<<"Finished running."<<endl;

	-- Calculate the exit code
	exitcode:=returncode//256;

	-- Now read the output.
	(theanswer,wasSuccess):=parseAnswer(answerFileName);

	-- Collect statistics
	stats:=null;
	if (opt#KeepStatistics) then (
		try (
			stats=get(statFileName);
		) else (
			stderr<<"runExternalM2: error recovering statistics"<<endl;
		); 
	);

	-- Delete any files, if desired
	if (opt#KeepFiles===false or (opt#KeepFiles===null and wasSuccess)) then (
		-- Remove the temporary files
		safelyRemoveFile("runExternalM2: missing output file",outFileName); 
		outFileName=null;
		if (opt#KeepStatistics) then (
			safelyRemoveFile("runExternalM2: missing statistics file",statFileName); 
			statFileName=null;
		);
		safelyRemoveFile("runExternalM2: missing answer file",answerFileName); 
		answerFileName=null;
		safelyRemoveFile("runExternalM2: missing script file",scriptFileName); 
		scriptFileName=null;
	);

	return hashTable({
		"exit code"=>exitcode,
		"return code"=>returncode,
		"time used"=>timer,
		"output file"=>outFileName,
		"answer file"=>answerFileName,
		"statistics"=>stats,
		value=>theanswer});
);

beginDocumentation();
--print(mydoc);
multidoc(mydoc);

------------------------------------- TESTS -------------------------

-- Check that we set the external child setting correctly
TEST ///
fn=temporaryFileName()|".m2";
fn<<flush;

assert(isExternalM2Child()===false);
assert(isExternalM2Parent()===true);
h=runExternalM2(fn,isExternalM2Child,());
assert(h#value===true);
h=runExternalM2(fn,isExternalM2Parent,());
assert(h#value===false);
///
 
-- runExternalM2 tests
-- a basic test of functionality.
-- Also test different ways of referring to the function.
TEST ///
fn=temporaryFileName()|".m2";
fn<<////
  f = (j)->for i from 1 when i<=j list (i^2);
////<<endl<<close;
r=runExternalM2(fn,"f",5);
assert(r#value==={1,4,9,16,25});
assert(r#"exit code"===0);
assert(instance(r#"time used",ZZ));

r=runExternalM2(fn,symbol f,5);
assert(r#value==={1,4,9,16,25});
assert(r#"exit code"===0);

load fn;
r=runExternalM2(fn,f,5);
assert(r#value==={1,4,9,16,25});
assert(r#"exit code"===0);
///;

-- Test that we can return various objects.
TEST ///
fn=temporaryFileName()|".m2";
fn<<////
justwait = (z,t)->(
  i:=0;
  for i from 0 when i<t do ();
  z
);
R=QQ[x,y,z];
////<<endl<<close;
basicChecks = (h) -> (
	assert(h#"exit code"===0);
	assert(instance(h#"time used",ZZ));
	assert(h#"time used">=0);
);

h=runExternalM2(fn,justwait,(QQ,4));
basicChecks(h);
assert(h#value===QQ);
assert(h#"answer file"===null);

R=QQ[x,y,z];
f=x^2+y^2+z+(3/2);
h=runExternalM2(fn,justwait,(f,10^2));
basicChecks(h);
assert(ring(h#value)===R);
assert(h#value===f);
assert(h#"answer file"===null);

M=image random(R^3,R^{5:-2});
h=runExternalM2(fn,justwait,(M,10^2));
basicChecks(h);
assert(ring(h#value)===R);
assert(h#value===M);

s=toExternalString(M);
h=runExternalM2(fn,justwait,(s,10^2));
basicChecks(h);
assert(h#value===s);
///;

-- Test just 1 argument, and test abnormal exits
TEST ///
fn=temporaryFileName()|".m2";
fn<<////
justexit = (z)->(
  exit(z);
);
justnull = (z)-> (
  3+3;
);
just3 = () -> (
  return 3;
);
////<<endl<<close;

basicChecks = (h,eec,ev) -> (
	assert(h#"exit code"===eec);
	assert(h#value===ev);
	assert(instance(h#"time used",ZZ));
	assert(h#"time used">=0);
);

-- Test a singleton object, and a list of length 1 give the same result
h=runExternalM2(fn,justexit,3);
basicChecks(h,3,null);
assert(not(h#"answer file"===null));
assert(not(fileExists(h#"answer file")));
assert(not(h#"output file"===null));
assert(fileExists(h#"output file"));

h=runExternalM2(fn,justexit,(3));
basicChecks(h,3,null);
assert(not(h#"answer file"===null));
assert(not(fileExists(h#"answer file")));
assert(not(h#"output file"===null));
assert(fileExists(h#"output file"));

-- What happens if we don't return anything?
h=runExternalM2(fn,justnull,("howdy"));
basicChecks(h,0,null);

-- What happens if there are no parameters?
h=runExternalM2(fn,just3,());
basicChecks(h,0,3);

-- Check handling of saved files
-- KeepFiles=null has been checked above
-- KeepFiles=true?
h=runExternalM2(fn,justexit,(3),KeepFiles=>true);
basicChecks(h,3,null);
assert(not(h#"answer file"===null));
assert(not(fileExists(h#"answer file")));
assert(not(h#"output file"===null));
assert(fileExists(h#"output file"));

h=runExternalM2(fn,just3,(),KeepFiles=>true);
basicChecks(h,0,3);
assert(not(h#"answer file"===null));
assert(fileExists(h#"answer file"));
assert(not(h#"output file"===null));
assert(fileExists(h#"output file"));

-- KeepFiles=false?
h=runExternalM2(fn,justexit,(3),KeepFiles=>false);
basicChecks(h,3,null);
assert(h#"answer file"===null);
assert(h#"output file"===null);

h=runExternalM2(fn,just3,(),KeepFiles=>false);
basicChecks(h,0,3);
assert(h#"answer file"===null);
assert(h#"output file"===null);
///;

-- Check that ulimits and statistics work
TEST ///
fn=temporaryFileName()|".m2";
fn<<////
spin = (x,t) -> (
  startTime:=cpuTime();
	while(cpuTime()-startTime<t) do ();
	return(x);
);
////<<endl<<close;

r=runExternalM2(fn,"spin",(5,6),PreRunScript=>"ulimit -t 2");
assert(not(r#"return code"===0));
assert(r#value===null);

r=runExternalM2(fn,"spin",(5,2),KeepStatistics=>true);
assert(r#"return code"===0);
assert(r#value===5);
assert(instance(r#"statistics",String));
assert(length(r#"statistics")>0);
///;

end

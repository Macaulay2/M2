--		Copyright 1995 by Daniel R. Grayson

document { quote exec,
     TT "exec argv", " -- uses the 'exec' operating system call to
     start up another program, replacing the current Macaulay 2 process.
     Here ", TT "argv", " is a string, or a sequence or list of strings
     to be passed as arguments to the new process.  The first string
     is the name of the executable file."
     }

restart = new Command from ( 
     () -> exec commandLine
     )

document { quote restart,
     TT "restart", " -- restart Macaulay 2 from the beginning."
     }

     
-* 
PARADIGM:

process 0 is the MANAGER (possibly interactive)
the rest of processes are WORKERS
communication: 
  * MANAGER sends WORKER a task (a string containing lines of M2 code to execute)
  * WORKER completes the task and sends the result back to MANAGER (a string containing the output of the last line) 
  
*** DONE *** 

Core functions:
* send a string to a process
* receive a string from process
* get process number (rank)
* get number of processes 

*** TO DO ***
* write a package containing convenience parallelization routines

Perhaps:
* broadcast a string to all workers
* "spawn" new workers
* make workers prepend error messages (or anything that goes to stdin/stderr) with their ID. 
 
*-

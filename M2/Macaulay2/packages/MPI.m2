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

-*
Possible use case:
  bunch of Groebner bases computed, i.e. using different monomial orders.
  run through all of them, receive from each, the initial monomials, and/or GB elements themselves.

Possible use case:
  queue of tasks: input, output, status (processor, actual status). (e.g. doRandomBinomialIdealSearch(R, 1000)), tag a task (e.g. an index).
  list of completed tasks?
  indicator function: is a worker available? (mutable list of (string sent, or null)) (or three states...)
    -- once we do receiveString(worker), we set this to null for that worked.
  freeWorker (returns null if none are free), or block+probe.
    block until one is free.

  need a loop: task manager which does all the tasks:
    probe workers (blocks until one is free)
      if probe tells us we have a result: we store it in the task output, set it to be free
      if one is free: grab the next task, send it to that worker.
    
Compute random binomial ideals, find their betti numbers, and keep only the ones with regularity >= n, some fixed n.

broadcastCode(String);
-- want to compute r
  
for i in mpiNodes do sendMessage("a = {1,2,3}
*-

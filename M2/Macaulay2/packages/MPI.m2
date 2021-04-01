-* 
*** PARADIGM ***

process 0 is the MANAGER (possibly interactive)
the rest of processes are WORKERS

communication: 
  * MANAGER sends WORKER a task (a string containing lines of M2 code to execute)
  * WORKER completes the task and sends the result back to MANAGER (a string containing the output of the last line) 

To build:
  * Install MPI: e.g. `brew install open-mpi`
  * `../../configure  CC=mpicc CXX=mpic++ --enable-download`  

To run: 
  * see comments in `MPI/master-worker-MPI.m2`
  
*** DONE *** 

Core functions:
* send a string to a process
* receive a string from process
* get process number (rank)
* get number of processes 


*** TO DO ***

* probe for a message (from any process)
*? "spawn" new workers
* broadcast
* write a package containing convenience parallelization routines
*? make workers prepend error messages (or anything that goes to stdin/stderr) with their ID. 
 
*** USE CASES ***

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

  need a loop: task manager which distributes all the tasks:
    probe workers (blocks until one is free)
      if probe tells us we have a result: we store it in the task output, set it to be free
      if one is free: grab the next task, send it to that worker.
    
E.g.: Compute random binomial ideals, find their betti numbers, and keep only the ones with regularity >= n, some fixed n.

*-

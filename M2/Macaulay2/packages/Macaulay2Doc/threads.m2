--		Copyright 2010 by Daniel R. Grayson

multidoc ///
Node
 Key
  "programming with threads"
 Subnodes
  Thread
  "threadVariable"
  (isReady,Thread)
  taskResult
  cancelTask
Node
 Key
  (cancelTask,Thread)
  cancelTask
 Headline
  stop a thread
 Usage
  cancelTask t
 Inputs
  t:
 Consequences
  Item
   The thread {\tt t} is interrupted by setting a flag.  Eventually it will stop,
   as can be detected with @ TO (isReady,Thread) @.  Then @ TO (taskResult,Thread) @
   can be used to retrieve the final value, which is always @ TO null @.
Node
 Key
  schedule
  (schedule,Function)
  (schedule,Function,Thing)
 Headline
  start a new thread
 Usage
  schedule(f,x)
  schedule f
 Inputs
  f:Function
  x:Thing
 Outputs
  :
   a new thread computing the value of the {\tt f(x)}, or, if {\tt x} is not provided, then of {\tt f()}
 Description
  Text
   The computation proceeds in the background, in the new thread.  The status of the thread can be observed
   by printing {\tt t}.  When the computation is finished, as can be detected with @ TO (isReady,Thread) @,
   the final value can be retrieved with @ TO (taskResult,Thread) @.
   
   If @ TO "notify" @ is set to @ TO true @, then useful messages are printed when the thread changes state.
Node
 Key
  Thread
 Headline
  the class of all threads
Node
 Key
  (taskResult,Thread)
  taskResult
 Headline
  retrieve the value returned by a thread
 Usage
  taskResult t
 Inputs
  t:
 Outputs
  :
   the value returned by the function provided to @ TO (schedule,Function) @ when the thread was started,
   provided it is ready (done), as determined by @ TO (isReady, Thread) @.
 Consequences
  Item
   The field in {\tt t} where the return value is stored is set to @ TO null @, and the thread is
   considered to have completely terminated.  Attempting to fetch the return value a second time
   will signal an error.
Node
 Key
  "threadVariable"
 Headline
  create a symbol whose value in one thread is not shared with others
 Usage
  threadVariable foo
 Outputs
  :
   a new symbol, whose name is "foo", for example, whose values in each thread will be independent of each other,
   with initial value @ TO null @
Node
 Key
  (isReady,Thread)
 Headline
  whether a thread is finished
 Usage
  isReady t
 Inputs
  t:
 Outputs
  :
   whether the thread {\tt t} has finished executing and a return value is available
 Description
  Text
   The return value can be retrieved with @ TO (taskResult, Thread )@.
///

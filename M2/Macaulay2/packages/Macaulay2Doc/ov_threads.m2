-- -*- coding: utf-8 -*-
--		Copyright 2010 by Daniel R. Grayson

multidoc ///
Node
 Key
  "parallel programming with threads and tasks"
 Subnodes
  parallelApply
  createTask
  addCancelTask
  addDependencyTask
  addStartTask
  schedule
  (isReady,Task)
  taskResult
  cancelTask
  isCanceled
  setIOExclusive
  setIOSynchronized
  setIOUnSynchronized
  getIOThreadMode
  "threadLocal"
  "allowableThreads"
  "maxAllowableThreads"
  Task
 SeeAlso
  "parallelism in engine computations"
  "elapsedTime"
 Description
  Text
    The simplest way to run computations in parallel is to use @ TO parallelApply @. This works
    like @ TO (apply,BasicList,Function) @, except that it uses all your cores, and always
    returns a @ TO List @.
  Example
       parallelApply(1..10, n -> n!)
  Text
    There is some overhead to parallelism, so this will only speed things up for a big
    computation. If the list is long, it will be split into chunks for each core, reducing the
    overhead. But the speedup is still limited by the different threads competing for memory,
    including cpu caches; it is like running Macaulay2 on a computer that is running other big
    programs at the same time. We can see this using @ TO "elapsedTime" @.
  Example
       L = random toList (1..10000);
       elapsedTime         apply(1..100, n -> sort L);
       elapsedTime parallelApply(1..100, n -> sort L);
  Text
    You will have to try it on your examples to see how much they speed up.

    Warning: Threads computing in parallel can give wrong answers if their code is not "thread
    safe", meaning they make modifications to memory without ensuring the modifications get
    safely communicated to other threads. (Thread safety can slow computations some.) Currently,
    modifications to Macaulay2 variables and mutable hash tables are thread safe, but not
    changes inside mutable lists. Also, access to external libraries such as singular, etc., may
    not currently be thread safe.
    
    The rest of this document describes how to control parallel tasks more directly.
    
    The task system schedules functions and inputs to run on a preset number of
    threads. The number of threads to be used is given by the variable
    @ TO "allowableThreads" @, and may be examined and changed as follows.
    (@ TO "allowableThreads" @ is temporarily increased if necessary inside
    @ TO parallelApply @.)
  Example
       allowableThreads
       allowableThreads = maxAllowableThreads
  Text
    To run a function in another thread use @ TO schedule @, as in the
    following example.
  Example
       R = QQ[x,y,z];
       I = ideal(x^2 + 2*y^2 - y - 2*z, x^2 - 8*y^2 + 10*z - 1, x^2 - 7*y*z)
       f = () -> gens gb I
       t = schedule f
  Text
    Note that @ TO schedule @ returns a task, not the result of the computation,
    which will be accessible only after the task has completed the computation.
  Example
       t
  Text
    Use @ TO isReady @ to check whether the result is available yet.
  Example
       isReady t
  Text
    To wait for the result and then retrieve it, use @ TO taskResult @.
  Example
       taskResult t
       assert instance(oo, Matrix)
  Text
    It is possible to make a task without starting it running, using @ TO createTask @.
  Example
       t' = createTask f
       t'
  Text
    Start it running with @ TO schedule @.
  Example
       schedule t';
       t'
       taskResult t'
  Text
    One may use @ TO addStartTask @ to specify that one task is to be started after another
    one finishes.  In the following example, {\tt G} will start after {\tt F} finishes.
  Example
       F = createTask(() -> "result of F")
       G = createTask(() -> "result of G")
       addStartTask(F,G)
       schedule F
       taskResult F
       taskResult G
  Text
    Use @ TO addCancelTask @ to specify that the completion of one task triggers the cancellation
    of another, by means of an interrupt exception.
    
    Use @ TO addDependencyTask @ to schedule a task, but to ensure that it will not run until one or more
    other tasks finish running.
    
    Using the functions above, essentially any parallel functionality needed can be created. 
    
    Low level C API functionality using the same scheduler also exists in the
    Macaulay2/system directory. It works essentially the same way as the
    Macaulay2 interface.
Node
 Key
  parallelApply
  (parallelApply, BasicList, Function)
  [parallelApply, Strategy]
 Headline
  apply a function to each element in parallel
 Usage
  parallelApply(L,f)
 Inputs
  L:BasicList
  f:Function
  Strategy => {Nothing, String}
 Outputs
  :{List, BasicList}
    If the @TO Strategy@ option is @TO null@, then this behaves like
    @M2CODE "toList apply(L,f)"@, with the result computed in parallel in
    chunks using all cores.
    If it is the string @SAMP "\"raw\""@, then this behaves like
    @M2CODE "apply(apply(L, e -> schedule(f, e)), taskResult)"@.
 Description
  Text
    If the option @SAMP "Strategy"@ is given the string @SAMP "\"raw\""@, then
    a separate task is created for each element of @VAR "L"@. @VAR "L"@ is not
    split into chunks, @ TO "allowableThreads" @ is used unchanged, and the
    result has the same class as @VAR "L"@.   Normally the default strategy
    (@M2CODE "Strategy => null"@) is more efficient.

    See @ TO "parallel programming with threads and tasks" @ for more information and an
    important warning about thread safety.
Node
 Key
  (addCancelTask, Task, Task)
  addCancelTask
 Headline
  specify that the completion of one task triggers the cancellation of another
 Usage
  addCancelTask(t,u)
 Inputs
  t:Task
  u:Task
 Consequences
  Item
   when the task {\tt t} is finished, then the task {\tt u} will be cancelled
Node
 Key
  (cancelTask,Task)
  cancelTask
 Headline
  stop a task
 Usage
  cancelTask t
 Inputs
  t:
 Consequences
  Item
   The task {\tt t} is interrupted by setting a flag.  Eventually it will stop.
 Description
  Example
   n = 0
   t = schedule(() -> while true do n = n+1)
   sleep 1
   t
   n
   sleep 1
   t
   n
   isReady t
   cancelTask t
   sleep 2
   t
   n
   sleep 1
   n
   isReady t
Node
 Key
  schedule
  (schedule,Function)
  (schedule,Function,Thing)
  (schedule,Task)
 Headline
  schedule a task for execution
 Usage
  schedule(f,x)
  schedule f
  schedule t
 Inputs
  f:Function
  x:Thing
  t:Task
 Outputs
  :
   a new task computing the value of the {\tt f(x)}, or, if {\tt x} is not provided, then of {\tt f()}.
   Alternatively, the task {\tt t}, created previously with @ TO createTask @, is scheduled for execution.
 Description
  Text
   The computation proceeds in the background, in a thread.  The status of the task can be observed
   by printing {\tt t}.  When the computation is finished, as can be detected with @ TO (isReady,Task) @,
   the final value can be retrieved with @ TO (taskResult,Task) @.
   
   If @ TO "notify" @ is set to @ TO true @, then useful messages are printed when the task changes state.
  Example
   f = x -> 2^x
   t = createTask(f,3)
   schedule t
   taskResult t
   u = schedule(f,4)
   taskResult u
Node
 Key
  Task
 Headline
  the class of all tasks
Node
 Key
  (taskResult,Task)
  taskResult
 Headline
  retrieve the value returned by a task
 Usage
  taskResult t
 Inputs
  t:
 Outputs
  :
   the value returned by the function provided to @ TO (schedule,Function) @ when the task was started.
   @ TO (taskResult,Task) @ will first wait for the task to finish if necessary.  If the task is cancelled,
   an error will be signaled.
 Consequences
  Item
   The field in {\tt t} where the return value is stored is set to @ TO null @, and the task is
   considered to have completely terminated.  Attempting to fetch the return value a second time
   will signal an error.
Node
 Key
  "threadLocal"
 Headline
  create a symbol whose value in one thread is not shared with others
 Usage
  threadLocal foo
 Outputs
  :
   a new symbol, whose name is "foo", for example, whose values in each thread will be independent of each other,
   with initial value @ TO null @
 Description
  Example
   threadLocal x
   x = 1
   t = schedule ( () -> ( x = 2 ; x ) )
   taskResult t
   x
Node
 Key
  (isReady,Task)
 Headline
  whether a task is finished
 Usage
  isReady t
 Inputs
  t:
 Outputs
  :
   whether the task {\tt t} has finished executing and a return value is available
 Description
  Text
   The return value can be retrieved with @ TO (taskResult, Task )@.
Node
 Key
  createTask
  (createTask, Function, Thing)
  (createTask, Function)
 Headline
  create a task
 Usage
  createTask(f,x)
  createTask f
 Inputs
  f:Function
  x:Thing
 Outputs
  :Task
   which when scheduled, will apply the function {\tt f} to the argument {\tt x}.  In the
   second form, where {\tt x} is not specified, it is take to be {\tt ()}.
 Description
  Example
   f = x -> 2^x
   t = createTask(f,3)
   schedule t
   taskResult t
Node
 Key
  (addDependencyTask, Task, Task)
  addDependencyTask
 Headline
  schedule a task, but ensure that it will not run until another task finishes
 Usage
  addDependencyTask(t,u)
 Inputs
  t:Task
  u:Task
 Consequences
  Item
   the task {\tt t} will be scheduled for execution, but execution will not begin until the
   task {\tt u} finishes
 Description
   Example
     for i to 5 do t_i = createTask(() -> i)
     for i from 1 to 5 do addDependencyTask(t_i, t_(i - 1))
     schedule t_0
     sleep 1
     taskResult t_5
Node
 Key
  (addStartTask, Task, Task)
  addStartTask
 Headline
  schedule a task upon completion of another
 Usage
  addStartTask(t,u)
 Inputs
  t:Task
  u:Task
 Consequences
  Item
   After task {\tt t} finishes, task {\tt u} will be scheduled for execution.
 Description
  Example
       f = () -> "f value";
       g = () -> "g value";
       F = createTask f
       G = createTask g
       addStartTask(F,G)
       schedule F
       taskResult F
       taskResult G
Node
 Key
  "allowableThreads"
 Headline
  the current maximum number of simultaneously running tasks
 Usage
  allowableThreads = n
 Consequences
  Item
   The number of threads devoted to computation of tasks is set to {\tt n}.  The
   number includes the main thread, but not the threads started independently by
   the garbage collector.  Thus the maximum number of background tasks running simultaneously
   will be {\tt n-1}.  The value of {\tt n} should be not larger than the value
   of @ TO "maxAllowableThreads" @.
 SeeAlso
  "parallel programming with threads and tasks"
Node
 Key
  "maxAllowableThreads"
 Headline
  the maximum possible number of simultaneously running tasks
 Usage
  maxAllowableThreads
 Outputs
  :ZZ
   the maximum number to which @ TO "allowableThreads" @ can be set
 Description
  Example
   maxAllowableThreads
 SeeAlso
  "parallel programming with threads and tasks"
Node
 Key
  setIOExclusive
 Headline
  exclusive I/O for the current thread
 Usage
  setIOExclusive()
  setIOExclusive f
 Inputs
  f:File
 Consequences
  Item
   the current thread becomes the only one permitted to use the file @VAR "f"@,
   or if no file is given, the files @ TO stdio @ and @ TO stderr @.
 SeeAlso
  "parallel programming with threads and tasks"
  setIOUnSynchronized
  setIOSynchronized
  getIOThreadMode
Node
 Key
  setIOSynchronized
 Headline
  synchronized I/O for threads
 Usage
  setIOSynchronized()
  setIOSynchronized f
  getIOThreadMode
 Inputs
  f:File
 Consequences
  Item
   threads are permitted to use the file @VAR "f"@, or if no file is given,
   @ TO stdio @ and @ TO stderr @, to output complete lines only
 Caveat
   this function is experimental
 SeeAlso
  "parallel programming with threads and tasks"
  setIOUnSynchronized
  setIOExclusive
  getIOThreadMode
Node
 Key
  setIOUnSynchronized
 Headline
  unsynchronized I/O for threads
 Usage
  setIOUnSynchronized()
  setIOUnSynchronized f
  getIOThreadMode
 Inputs
  f:File
 Consequences
  Item
   threads are permitted to use the file @VAR "f"@, or if no file is given,
   @ TO stdio @ and @ TO stderr @, in an unregulated manner
 SeeAlso
  "parallel programming with threads and tasks"
  setIOSynchronized
  setIOExclusive
Node
 Key
  getIOThreadMode
 Headline
  get I/O thread mode
 Usage
  getIOThreadMode()
  getIOThreadMode f
 Inputs
  f:File
 Consequences
  Item
   returns the I/O thread mode for the file @VAR "f"@, or if no file is given,
   @TO stdio@, as an integer:
   @UL {
       "0 for unsynchronized",
       "1 for synchronized",
       "2 for exclusive"}@
 SeeAlso
  setIOUnSynchronized
  setIOSynchronized
  setIOExclusive
Node
 Key
  (isCanceled,Task)
  isCanceled
 Headline
  whether a task has been canceled
 Usage
  isCanceled t
 Inputs
  t:
 Outputs
  :
   whether the task {\tt t} has been canceled
 Description
   Example
     n = 0
     t = schedule(() -> while true do n = n + 1)
     sleep 1
     isCanceled t
     sleep 1
     cancelTask t
     sleep 2
     isCanceled t
 SeeAlso
  "parallel programming with threads and tasks"
  cancelTask
///

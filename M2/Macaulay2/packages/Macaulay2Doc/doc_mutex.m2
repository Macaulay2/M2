doc ///
  Key
    Mutex
  Headline
    the class of mutexes
  Description
    Text
      A @EM "mutex"@ (short for @EM "mutual exclusion"@) is a synchronization
      primitive used to prevent multiple threads from accessing shared
      data at the same time. It ensures that only one thread can hold
      the lock at a time, which helps avoid race conditions in
      concurrent programs.

      In Macaulay2, a @M2CODE "Mutex"@ object can be used to protect critical
      sections of code. When a thread locks a mutex, other threads attempting
      to lock it will wait until it is unlocked.

      For example, suppose multiple threads try to modify the same string.
      Each thread will need to get the current value of the string, make its
      modification, and then save the new value.  However, there is a good
      chance that another thread might save its updated value after another
      thread has fetched it but before it saved the new value.  We can
      see this in the code below.
    Example
      msgs = ""
      sayhello = i -> msgs |= "hello from thread #" | toString i | newline
      T = apply(10, i -> schedule(() -> sayhello i))
      while not all(T, isReady) do null
      stack sort lines msgs
    Text
      We likely ended up with fewer than the expected number of 10 messages.
      We can get around this issue by using a mutex to lock the string so
      that only one thread can modify it at a time.
    Example
      m = new Mutex
      msgs = ""
      T = apply(10, i -> schedule(() -> (lock m; sayhello i; unlock m)))
      while not all(T, isReady) do null
      stack sort lines msgs
    Text
      With the mutex, all 10 messages are present.
  SeeAlso
    "parallel programming with threads and tasks"
    AtomicInt
  Subnodes
    (NewMethod, Mutex)
    (lock, Mutex)
    (tryLock, Mutex)
    (unlock, Mutex)
///

doc ///
  Key
    (NewMethod, Mutex)
  Headline
    construct a mutex
  Usage
    new Mutex
  Outputs
    :Mutex
  Description
    Text
      Construct a new @TO Mutex@ object.
    Example
      m = new Mutex
///

doc ///
  Key
     lock
    (lock, Mutex)
  Headline
    lock a mutex
  Usage
    lock m
  Inputs
    m:Mutex
  Description
    Text
      Locks a mutex.
    Example
      m = new Mutex
      lock m
    Text
      If the mutex is already locked, then the thread blocks until it is
      unlocked.  This is not interruptible.
  SeeAlso
    (tryLock, Mutex)
    (unlock, Mutex)
///

doc ///
  Key
     tryLock
    (tryLock, Mutex)
  Headline
    try locking a mutex
  Usage
    tryLock m
  Inputs
    m:Mutex
  Description
    Text
      Tries locking a mutex.
    Example
      m = new Mutex
      tryLock m
    Text
      If the mutex is already locked, then an error is raised.
    Example
      stopIfError = false
      tryLock m
  SeeAlso
    (lock, Mutex)
    (unlock, Mutex)
///

doc ///
  Key
     unlock
    (unlock, Mutex)
  Headline
    unlock a mutex
  Usage
    unlock m
  Inputs
    m:Mutex
  Description
    Text
      Unlocks a mutex.
    Example
      m = new Mutex
      lock m
      unlock m
  SeeAlso
    (lock, Mutex)
    (tryLock, Mutex)
///

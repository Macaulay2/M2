use M2;
declarations "
    #include <M2/gc-include.h>
    #include <../system/mutex.h>
";
export voidPointer := Pointer "void *";
export nullPointer() ::= Ccode(voidPointer,"((void *)0)");
export threadFunction := function(voidPointer):voidPointer;
export taskPointer := Pointer "struct ThreadTask *";
export nullTaskPointer() ::= Ccode(taskPointer,"((struct ThreadTask *)0)");
export Thread := arithmeticType "pthread_t";	 -- we may have to change arithmeticType to Type
export nullThread() ::= Thread(0);		 -- this may not always compile
export (x:Thread) === (y:Thread) ::= Ccode(bool, "(0 != pthread_equal(,",x,",",y,"))");
export ThreadMutex := atomicType "pthread_mutex_t";
header "pthread_mutex_t pthread0_newMutex = PTHREAD_MUTEX_INITIALIZER;";
import newMutex:ThreadMutex;
export ThreadAttr := atomicType "pthread_attr_t";
export ThreadMutexAttr := atomicType "pthread_mutexattr_t";
export ThreadCond := atomicType "pthread_cond_t";
export ThreadCondAttr := atomicType "pthread_condattr_t";
export ThreadKey := atomicType "pthread_key_t";
export ThreadOnce := atomicType "pthread_once_t";
export ThreadRWLock := atomicType "pthread_rwlock_t";
export ThreadRWLockAttr := atomicType "pthread_rwlockattr_t";
-- not available on the Mac:
-- export ThreadSpinLock := atomicType "pthread_spinlock_t";
-- export ThreadBarrier := atomicType "pthread_barrier_t";
-- export ThreadBarrierAttr := atomicType "pthread_barrierattr_t";
export SpinLock := atomicType "struct spinlockStructure";
export init(x:ThreadMutex) ::= Ccode(int, "pthread_mutex_init(&(",lvalue(x),"),NULL)");
export destroy(x:ThreadMutex) ::= Ccode(int, "pthread_mutex_destroy(&(",lvalue(x),"))");
export lock(x:ThreadMutex) ::= Ccode(int, "pthread_mutex_lock(&(",lvalue(x),"))");
export unlock(x:ThreadMutex) ::= Ccode(int, "pthread_mutex_unlock(&(",lvalue(x),"))");
export getthreadself() ::= Ccode(Thread, "pthread_self()");

header "spinLock pthread0_uninitializedSpinLock;";
import uninitializedSpinLock:SpinLock;
export init(x:SpinLock) ::= Ccode(void, "initializeSpinLock(&(",lvalue(x),"))");
export lock(x:SpinLock) ::= Ccode(void, "acquireSpinLock(&(",lvalue(x),"))");
export unlock(x:SpinLock) ::= Ccode(void, "releaseSpinLock(&(",lvalue(x),"))");

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d pthread0.o "
-- End:

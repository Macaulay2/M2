--		Copyright 1994-2006,2010 by Daniel R. Grayson

use atomic;
use arithmetic;

export threadLocal interruptShield := false;
export threadLocal interruptPending := false;
export threadLocal alarmedFlag := false;
export threadLocal steppingFlag := false;


import threadLocal interruptedFlag:atomicField;
declarations "
#define interrupted() test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field))
";


import threadLocal exceptionFlag:atomicField; -- indicates interrupt, stepping, or alarm

export determineExceptionFlag():void := (
     store(exceptionFlag, test(interruptedFlag) || steppingFlag || alarmedFlag);
     );
export alarm(x:uint) ::= Ccode(int,"
     #ifdef HAVE_ALARM
      alarm(",x,")
     #else
      -1
     #endif
     ");
export clearAlarm():void := alarm(uint(0));
export clearAllFlags():void := (
     store(exceptionFlag, false);
     compilerBarrier();
     store(interruptedFlag, false);
     steppingFlag = false;
     alarmedFlag = false;
     interruptPending = false;
     );
export setInterruptFlag():void := (
     --note ordering here, interrupt flag, then exception flag, then libfac interrupt flag. 
     store(interruptedFlag, true);
     --compiler barrier necessary to disable compiler reordering.  
     --On architectures that do not enforce memory write ordering, emit a memory barrier
     compilerBarrier();
     store(exceptionFlag, true);
     );
export setAlarmedFlag():void := (
     store(interruptedFlag, true); -- an alarm is an interrupt, as far as the engine is concerned
     alarmedFlag = true;
     store(exceptionFlag, true);
     );
export setSteppingFlag():void := (
     steppingFlag = true;
     store(exceptionFlag, true);
     );
export clearInterruptFlag():void := (
     --reverse previous order when undoing set.  
     store(interruptedFlag, false);
     compilerBarrier();
     determineExceptionFlag();
     );
export clearAlarmedFlag():void := (
     store(interruptedFlag, false);
     alarmedFlag = false;
     determineExceptionFlag();
     );
export stepCount := -1;
export microStepCount := -1;
export clearSteppingFlag():void := (
     stepCount = -1;
     microStepCount = -1;
     steppingFlag = false;
     determineExceptionFlag();
     );

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d interrupts.o "
-- End:

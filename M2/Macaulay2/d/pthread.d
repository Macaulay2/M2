use evaluate;
use expr;

header "#include \"../system/supervisorinterface.h\"";

threadCreate(f:function(TaskCellBody):null,tb:TaskCellBody) ::=  Ccode(void,
     "runM2Task((void *(*)(void *))(",f,"),(void *)(",tb,"))");


threadDetach(tid:Thread) ::=  Ccode(int,"pthread_detach(",tid,")");



startup(tb:TaskCellBody):null := (
     --warning wrong return type
     f := tb.fun; tb.fun = nullE;
     x := tb.arg; tb.arg = nullE;
     tb.thread = getthreadself();
     tb.exceptionFlagPointer = address(exceptionFlag);
     tb.interruptedFlagPointer = address(interruptedFlag);
     if notify then stderr << "--thread " << tb.tid << " started" << endl;
     --add thread to supervisor

     r := applyEE(f,x);
     when r is err:Error do (
	  printError(err);
	  if notify then stderr << "--thread " << tb.tid << " ended, after an error" << endl;
	  )
     else (
     	  tb.returnValue = r;
     	  if notify then stderr << "--thread " << tb.tid << " ready, result available " << endl;
	  );
     compilerBarrier();
     tb.done = true;
     null());

isFunction(e:Expr):bool := (
     when e
     is CompiledFunction do true
     is CompiledFunctionClosure do true
     is FunctionClosure do true
     is s:SpecialExpr do isFunction(s.e)
     else false);

cancelTask(tb:TaskCellBody):Expr := (
     if tb.resultRetrieved then return buildErrorPacket("thread reasult already retrieved");
     if tb.done then (
	  if notify then stderr << "--thread " << tb.tid << " done, cancellation not needed" << endl;
	  return nullE;
	  );
     if tb.cancellationRequested then return buildErrorPacket("thread cancellation already requested");
     when tb.interruptedFlagPointer is p:atomicFieldPointer do store(p,true)
     else return buildErrorPacket("thread cancellation: interruptedFlagPointer is null");
     when tb.exceptionFlagPointer is p:atomicFieldPointer do store(p,true)
     else return buildErrorPacket("thread cancellation: exceptionFlagPointer is null");
     tb.cancellationRequested = true;
     nullE);

cancelTask(e:Expr):Expr := when e is c:TaskCell do cancelTask(c.body) else WrongArg("a thread");
-- # typical value: cancelTask, Thread, Nothing
setupfun("cancelTask",cancelTask);

threadCellFinalizer(tc:TaskCell,p:null):void := (
     -- It is not safe to call any routines that depend on initialization of global variables here,
     -- because this finalizer may be called early, before all initialization is done.
     -- It is safe to write to stderr, because we've made output to it not depend on global variables being
     -- initialized.
     if tc.body.done then return;
     if tc.body.cancellationRequested then (
	  stderr << "--thread " << tc.body.tid << " inaccessible, cancelled but not ready yet" << endl;
	  )
     else (
	  if notify then stderr << "--cancelling inaccessible thread " << tc.body.tid << endl;
	  when cancelTask(tc.body) is err:Error do (printError(err);) else nothing));

header "#include <signal.h>";
schedule2(fun:Expr,arg:Expr):Expr := (
     if !isFunction(fun) then return WrongArg(1,"a function");
     -- FIX ME
     threadcounter := 0;
     tc := TaskCell(TaskCellBody(nullThread(), threadcounter, false, false, false, fun, arg, nullE, null(), null()));
     Ccode(void, "{ sigset_t s, old; sigemptyset(&s); sigaddset(&s,SIGINT); sigprocmask(SIG_BLOCK,&s,&old)");
     -- we are careful not to give the new thread the pointer tc, which we finalize:
     threadCreate(startup,tc.body);
     Ccode(void, "sigprocmask(SIG_SETMASK,&old,NULL); }");
     Ccode(void, "GC_REGISTER_FINALIZER(",tc,",(GC_finalization_proc)",threadCellFinalizer,",0,0,0)");
     Expr(tc));

schedule(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 2 then schedule2(args.0,args.1)
     else WrongNumArgs(1,2)
     else schedule2(e,emptySequenceE));
-- # typical value: schedule, Function, Thread
-- # typical value: schedule, Function, Thing, Thread
setupfun("schedule",schedule);	   

taskResult(e:Expr):Expr := (
     when e is c:TaskCell do
     if c.body.resultRetrieved then buildErrorPacket("thread result already retrieved")
     else if !c.body.done then buildErrorPacket("thread not done yet")
     else (
	  r := c.body.returnValue;
	  c.body.returnValue = nullE;
	  c.body.resultRetrieved = true;
	  r)
     else WrongArg("a thread"));
-- # typical value: taskResult, Thread, Thing
setupfun("taskResult",taskResult);


-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d pthread.o "
-- End:

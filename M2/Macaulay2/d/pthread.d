use evaluate;
use expr;

header "#include \"../system/supervisorinterface.h\"";

threadCreate(f:function(ThreadCellBody):null,tb:ThreadCellBody) ::=  Ccode(void,
     "runM2Task((void *(*)(void *))(",f,"),(void *)(",tb,"))");


threadDetach(tid:Thread) ::=  Ccode(int,"pthread_detach(",tid,")");

header "static void reverse_run(struct FUNCTION_CELL *p) { if (p) { reverse_run(p->next); (*p->fun)(); } }";

startup(tb:ThreadCellBody):null := (
     Ccode(void,"reverse_run(thread_prepare_list)"); -- re-initialize any thread local variables
     f := tb.fun; tb.fun = nullE;
     x := tb.arg; tb.arg = nullE;
     tb.thread = getthreadself();
     tb.tid = gettid();
     tb.exceptionFlagPointer = address(exceptionFlag);
     tb.interruptedFlagPointer = address(interruptedFlag);
     if notify then stderr << "--thread " << tb.tid << " started" << endl;
     --add thread to supervisor
     Ccode(void,"addThreadBody(",lvalue(tb.thread),",",lvalue(tb),")");
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

cancelThread(tb:ThreadCellBody):Expr := (
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

cancelThread(e:Expr):Expr := when e is c:ThreadCell do cancelThread(c.body) else WrongArg("a thread");
-- # typical value: cancelThread, Thread, Nothing
setupfun("cancelThread",cancelThread);

threadCellFinalizer(tc:ThreadCell,p:null):void := (
     -- It is not safe to call any routines that depend on initialization of global variables here,
     -- because this finalizer may be called early, before all initialization is done.
     -- It is safe to write to stderr, because we've made output to it not depend on global variables being
     -- initialized.
     Ccode(void,"delThread(",lvalue(tc.body.thread),")");
     if tc.body.done then return;
     if tc.body.cancellationRequested then (
	  stderr << "--thread " << tc.body.tid << " inaccessible, cancelled but not ready yet" << endl;
	  )
     else (
	  if notify then stderr << "--cancelling inaccessible thread " << tc.body.tid << endl;
	  when cancelThread(tc.body) is err:Error do (printError(err);) else nothing));

header "#include <signal.h>";

inThread2(fun:Expr,arg:Expr):Expr := (
     if !isFunction(fun) then return WrongArg(1,"a function");
     tc := ThreadCell(ThreadCellBody(nullThread(), 0, false, false, false, fun, arg, nullE, null(), null()));
     Ccode(void, "{ sigset_t s, old; sigemptyset(&s); sigaddset(&s,SIGINT); sigprocmask(SIG_BLOCK,&s,&old)");
     -- we are careful not to give the new thread the pointer tc, which we finalize:
     threadCreate(startup,tc.body);
     Ccode(void, "sigprocmask(SIG_SETMASK,&old,NULL); }");
     Ccode(void, "GC_REGISTER_FINALIZER(",tc,",(GC_finalization_proc)",threadCellFinalizer,",0,0,0)");
     Expr(tc));

inThread(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 2 then inThread2(args.0,args.1)
     else WrongNumArgs(1,2)
     else inThread2(e,emptySequenceE));
-- # typical value: inThread, Function, Thread
-- # typical value: inThread, Function, Thing, Thread
setupfun("inThread",inThread);	   

threadResult(e:Expr):Expr := (
     when e is c:ThreadCell do
     if c.body.resultRetrieved then buildErrorPacket("thread result already retrieved")
     else if !c.body.done then buildErrorPacket("thread not done yet")
     else (
	  r := c.body.returnValue;
	  c.body.returnValue = nullE;
	  c.body.resultRetrieved = true;
	  r)
     else WrongArg("a thread"));
-- # typical value: threadResult, Thread, Thing
setupfun("threadResult",threadResult);



export gettidfun(e:Expr):Expr := (
     when e
     is t:ThreadCell do (
	  while !isInitialized(t) do nothing;
	  toExpr(t.body.tid))
     is a:Sequence do (
	  if length(a) == 0
	  then toExpr(gettid())
	  else WrongNumArgs(0,1))
     else WrongNumArgs(0,1));


-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d pthread.o "
-- End:

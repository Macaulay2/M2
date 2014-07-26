use M2;
use evaluate;
use expr;

header "#include \"../system/supervisorinterface.h\"";


taskCreatePush(f:function(TaskCellBody):null,tb:TaskCellBody) ::=  Ccode(taskPointer,
     "runM2Task((void *(*)(void *))(",f,"),(void *)(",tb,"))");
taskCreate(f:function(TaskCellBody):null,tb:TaskCellBody) ::=  Ccode(taskPointer,
     "createM2Task((void *(*)(void *))(",f,"),(void *)(",tb,"))");

export taskDone(tp:taskPointer) ::= Ccode(int, "taskDone(",tp,")")==1;
export taskStarted(tp:taskPointer) ::=Ccode(int, "taskStarted(",tp,")")==1;
pushTask(tp:taskPointer) ::=Ccode(void, "pushTask(",tp,")");
taskResult(tp:taskPointer) ::=Ccode(voidPointer, "taskResult(",tp,")");
export taskKeepRunning(tp:taskPointer) ::= Ccode(int, "taskKeepRunning(",tp,")")==1;
export taskRunning(tp:taskPointer) ::= Ccode(int, "taskRunning(",tp,")")==1;
taskInterrupt(tp:taskPointer) ::= Ccode(void, "taskInterrupt(",tp,")");
taskAddCancelTask(tp:taskPointer, cancel:taskPointer) ::=Ccode(void, "addCancelTask(",tp,",",cancel,")");
taskAddStartTask(tp:taskPointer, start:taskPointer) ::=Ccode(void, "addStartTask(",tp,",",start,")");
taskAddDependency(tp:taskPointer, dep:taskPointer) ::=Ccode(void, "addDependency(",tp,",",dep,")");


startup(tb:TaskCellBody):null := (
     --warning wrong return type
     f := tb.fun; tb.fun = nullE;
     x := tb.arg; tb.arg = nullE;
     if notify then stderr << "--task " << tb.serialNumber << " started" << endl;
     --add thread to supervisor

     r := applyEE(f,x);
     when r is err:Error do (
	  printError(err);
	  if notify then stderr << "--task " << tb.serialNumber << " ended, after an error" << endl;
	  )
     else (
     	  tb.returnValue = r;
     	  if notify then stderr << "--task " << tb.serialNumber << " ready, result available " << endl;
	  );
     compilerBarrier();
     null());

isFunction(e:Expr):bool := (
     when e
     is CompiledFunction do true
     is CompiledFunctionClosure do true
     is FunctionClosure do true
     is s:SpecialExpr do isFunction(s.e)
     else false);

taskDone(tb:TaskCellBody):bool := tb.resultRetrieved || taskDone(tb.task);

cancelTask(tb:TaskCellBody):Expr := (
     if taskDone(tb) then (
	  if notify then stderr << "--task " << tb.serialNumber << " done, cancellation not needed" << endl;
	  return nullE;
	  );
     taskInterrupt(tb.task);
     nullE);

cancelTask(e:Expr):Expr := when e is c:TaskCell do cancelTask(c.body) else WrongArg("a thread");
-- # typical value: cancelTask, Task, Nothing
setupfun("cancelTask",cancelTask);

taskCellFinalizer(tc:TaskCell,p:null):void := (
     -- It is not safe to call any routines that depend on initialization of global variables here,
     -- because this finalizer may be called early, before all initialization is done.
     -- It is safe to write to stderr, because we've made output to it not depend on global variables being
     -- initialized.
     if taskDone(tc.body) then return;
     if notify then stderr << "--cancelling inaccessible task " << tc.body.serialNumber << " still running" << endl;
     when cancelTask(tc.body) is err:Error do (printError(err);) else nothing);

header "#include <signal.h>";

blockingSIGINT() ::= -- this macro and the next one are a matched pair: observe the braces within
     Ccode(void, "{ 
     	  #ifdef HAVE_SIGPROCMASK
	   sigset_t s, old; sigemptyset(&s); sigaddset(&s,SIGINT); sigprocmask(SIG_BLOCK,&s,&old)
          #else
     	   void (*old)(int) = signal(SIGINT,SIG_IGN);
	  #endif
	  ");
endBlockingSIGINT() ::=
     Ccode(void, "
     	  #ifdef HAVE_SIGPROCMASK
	   sigprocmask(SIG_SETMASK,&old,NULL); 
          #else
     	   signal(SIGINT,old);
	  #endif
	  }");

taskSerialNumber := 0;
nextTaskSerialNumber():int := (
     r := taskSerialNumber;
     taskSerialNumber = taskSerialNumber+1;		    -- race condition here, solve later
     taskSerialNumber);

createTask2(fun:Expr,arg:Expr):Expr :=(
     if !isFunction(fun) then return WrongArg(1,"a function");
     tc := TaskCell(TaskCellBody(nextHash(),nextTaskSerialNumber(),Ccode(taskPointer,"((void *)0)"), false, fun, arg, nullE ));
     blockingSIGINT();
     -- we are careful not to give the new thread the pointer tc, which we finalize:
     tc.body.task=taskCreate(startup,tc.body);
     endBlockingSIGINT();
     Ccode(void, "GC_REGISTER_FINALIZER(",tc,",(GC_finalization_proc)",taskCellFinalizer,",0,0,0)");
     Expr(tc));

createTask(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 2 then createTask2(args.0,args.1)
     else WrongNumArgs(1,2)
     else createTask2(e,emptySequenceE));

-- # typical value: createTask, Function, Thing, Task
-- # typical value: createTask, Function, Task
setupfun("createTask",createTask);

addStartTask2(e1:Expr,e2:Expr):Expr := (
     when e1 is task:TaskCell do
     when e2 is start:TaskCell do (
     	  taskAddStartTask(task.body.task,start.body.task); 
	  nullE)
     else WrongArg(2,"a task")
     else WrongArg(1,"a task"));

addStartTaskM2(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 2 then (addStartTask2(args.0,args.1); nullE)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
-- # typical value: addStartTask, Task, Task, Nothing
setupfun("addStartTask",addStartTaskM2);

addDependencyTask2(e1:Expr,e2:Expr):Expr := (
     when e1 is task:TaskCell do 
     when e2 is dep:TaskCell do (
     	  taskAddDependency(task.body.task,dep.body.task); 
	  nullE)
     else WrongArg(2,"a task")
     else WrongArg(1,"a task"));

addDependencyTaskM2(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 2 
     then (
	  addDependencyTask2(args.0,args.1); 
	  nullE)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
-- # typical value: addDependencyTask, Task, Task, Nothing
setupfun("addDependencyTask",addDependencyTaskM2);

addCancelTask2(e1:Expr,e2:Expr):Expr := (
     when e1 is task:TaskCell do
     when e2 is cancel:TaskCell do (
     	  taskAddCancelTask(task.body.task,cancel.body.task); 
	  nullE)
     else WrongArg(2,"a task")
     else WrongArg(1,"a task"));

addCancelTaskM2(e:Expr):Expr := (
     when e is args:Sequence do
     if length(args) == 2 then (addCancelTask2(args.0,args.1); nullE)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
-- # typical value: addCancelTask, Task, Task, Nothing
setupfun("addCancelTask",addCancelTaskM2);

schedule2(fun:Expr,arg:Expr):Expr := (
     if !isFunction(fun) then return WrongArg(1,"a function");
     tc := TaskCell(TaskCellBody(nextHash(),nextTaskSerialNumber(),Ccode(taskPointer,"((void *)0)"), false, fun, arg, nullE ));
     blockingSIGINT();
     -- we are careful not to give the new thread the pointer tc, which we finalize:
     tc.body.task=taskCreatePush(startup,tc.body);
     endBlockingSIGINT();
     Ccode(void, "GC_REGISTER_FINALIZER(",tc,",(GC_finalization_proc)",taskCellFinalizer,",0,0,0)");
     Expr(tc));

schedule1(task:TaskCell):Expr := (
     if taskStarted(task.body.task) then
     WrongArg("A task that hasn't started")
     else (
     pushTask(task.body.task); 
     Expr(task)
     )
);

schedule(e:Expr):Expr := (
     when e 
     is task:TaskCell do schedule1(task)
     is args:Sequence do
     if length(args) == 2 then schedule2(args.0,args.1)
     else WrongNumArgs(1,2)
     else schedule2(e,emptySequenceE));
-- # typical value: schedule, Task, Task
-- # typical value: schedule, Function, Task
-- # typical value: schedule, Function, Thing, Task
setupfun("schedule",schedule);	   

taskResult(e:Expr):Expr := (
     when e is c:TaskCell do
     if c.body.resultRetrieved then buildErrorPacket("task result already retrieved")
     else if !taskKeepRunning(c.body.task) then buildErrorPacket("task canceled")
     else if !taskDone(c.body.task) then buildErrorPacket("task not done yet")
     else (
	  r := c.body.returnValue;
	  c.body.returnValue = nullE;
	  c.body.resultRetrieved = true;
	  c.body.task = nullTaskPointer();
	  r)
     else WrongArg("a task"));
-- # typical value: taskResult, Task, Thing
setupfun("taskResult",taskResult);

setupfun("setIOSynchronized",setIOSynchronized);
setupfun("setIOUnSynchronized",setIOUnSynchronized);
setupfun("setIOExclusive",setIOExclusive);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d pthread.o "
-- End:

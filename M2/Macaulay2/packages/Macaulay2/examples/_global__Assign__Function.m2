X = new Type of MutableHashTable
x = new X
X.GlobalAssignHook = globalAssignFunction
X.GlobalReleaseHook = globalReleaseFunction
x' = new X
t = {x,x'}
x = x' = 44
t
code globalAssignFunction

-- check whether we can kill a thread
t = schedule ( () -> while true do nothing )
sleep 1
assert not isReady t
cancelTask t
sleep 1
-- assert isReady t
-- r = taskResult t
assert not isReady t
-- assert( null === r )

-- check whether we can get the result of a thread's computation: a function
t = schedule ( () -> 2+2 )
assert( 4 === taskResult t )

-- check whether we can get the result of a thread's computation: a function with an argument
t = schedule ( x -> x+2, 2 )
assert( 4 === taskResult t )

assert(hash schedule(() -> null) != hash schedule(() -> null))

-- check whether thread local variables have separate values in separate threads
threadVariable aaa
assert( aaa === null )
r = apply(3, i -> schedule (() -> ( aaa = i ; sleep 3 ; aaa )))
assert( {0,1,2} == taskResult \ r )
assert( aaa === null )

-- check whether storing into a mutable hash table is thread safe
setIOSynchronized()
n = 32
h = new MutableHashTable from apply(n, j -> j => null)
r = apply(3, i -> schedule (() -> (
	       for k from 0 to 150000 do (
		    if #h < 14 then h#(random n) = i else if #h > 18 then remove(h,random n)
		    else if random 1. < .5 then h#(random n) = i else remove(h,random n)))))
c = currentTime()
while not all(r,isReady) do (sleep 1; print tally values h)
print (currentTime()-c)

-- If this test fails, it could indicate that the order of *.o files on the linker command line
-- is wrong.  That affects the order of running the static initializers: in system/supervisor.cpp
-- there is a static initializer for maxNumThreads, and in d/actors5.d there is a call to 
-- getMaxAllowableThreads, which uses the value of maxNumThreads.  So we want the files system/*.o 
-- to come before the files d/*.o on the linker command line.
assert ( maxAllowableThreads != 0 )

-- thread modes
restoremode = i -> (
    if i == 0 then setIOUnSynchronized()
    else if i == 1 then setIOSynchronized()
    else if i == 2 then setIOExclusive()
    else error "unknown thread mode")

origmode = getIOThreadMode()

setIOExclusive()
setIOUnSynchronized()
assert Equation(getIOThreadMode(), 0)
assert Equation(getIOThreadMode stdio, 0)
assert Equation(getIOThreadMode stderr, 0)

setIOSynchronized()
assert Equation(getIOThreadMode(), 1)
assert Equation(getIOThreadMode stdio, 1)
assert Equation(getIOThreadMode stderr, 1)

setIOExclusive()
assert Equation(getIOThreadMode(), 2)
assert Equation(getIOThreadMode stdio, 2)
assert Equation(getIOThreadMode stderr, 2)

restoremode origmode

fn = temporaryFileName()
f = fn << "foo" << close

setIOExclusive f
setIOUnSynchronized f
assert Equation(getIOThreadMode f, 0)

setIOSynchronized f
assert Equation(getIOThreadMode f, 1)

setIOExclusive f
assert Equation(getIOThreadMode f, 2)

removeFile fn

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test threads.out"
-- End:

if not version#"threads" then (
     stderr << "--warning: threads not enabled, skipping tests" << endl;
     end)

-- check whether we can kill a thread
t = inThread ( () -> while true do nothing )
sleep 1
assert not isReady t
cancelThread t
sleep 1
assert isReady t
r = threadResult t
assert not isReady t
assert( null === r )

-- check whether we can get the result of a thread's computation: a function
t = inThread ( () -> 2+2 )
assert instance(threadID t, ZZ)
while not isReady t do nothing
assert( 4 === threadResult t )

-- check whether we can get the result of a thread's computation: a function with an argument
t = inThread ( x -> x+2, 2 )
while not isReady t do nothing
assert( 4 === threadResult t )

-- check whether thread local variables have separate values in separate threads
threadVariable aaa
assert( aaa === null )
r = apply(3, i -> inThread (() -> ( aaa = i ; sleep 3 ; aaa )))
while not all(r,isReady) do sleep 1
assert( {0,1,2} == threadResult \ r )
assert( aaa === null )

-- check whether storing into a mutable hash table is thread safe
n = 32
h = new MutableHashTable from apply(n, j -> j => null)
r = apply(3, i -> inThread (() -> (
	       for k from 0 to 150000 do (
		    if #h < 14 then h#(random n) = i else if #h > 18 then remove(h,random n)
		    else if random 1. < .5 then h#(random n) = i else remove(h,random n)))))
c = currentTime()
while not all(r,isReady) do (sleep 1; print tally values h)
print (currentTime()-c)
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test threads.out"
-- End:

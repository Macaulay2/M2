
end -- this test is disabled, because on machines with firewalls and on virtual machines, listening to a port may simply not work

f = openListener "$"
assert not isReady f

g = openInOut "$localhost"
assert isReady f
h = openInOut f
g << "hi there" << close
assert isReady h
x = get h
assert( x == "hi there" )
assert not isReady h
h << close


g = openInOut "$localhost"
assert isReady f
h = openInOut f
g << "ho there" << close
assert isReady h
x = get h
assert( x == "ho there" )
assert not isReady h
h << close

assert((last try 1/0 else lastError()) == "division by zero")
clearLastError()
assert(lastError() === null)

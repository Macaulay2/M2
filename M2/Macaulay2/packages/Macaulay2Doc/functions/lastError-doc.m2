doc ///
  Key
    lastError
  Headline
    get information about the last error
  Usage
    lastError()
  Outputs
    :Sequence
      of two elements, the @TO FilePosition@ of the code that generated
      the last error and a string containing the error message
  Description
    Example
      try 1/0
      lastError()
    Text
      The last error is local to each thread.
    Example
      taskResult schedule(() -> (try error "foo"; lastError()))
      lastError()
  SeeAlso
    clearLastError
///

doc ///
  Key
    clearLastError
  Headline
    clear the last error
  Usage
    clearLastError()
  Consequences
    Item
      The last error is set to @TO null@.
  Description
    Example
      try 1/0
      lastError()
      clearLastError()
      lastError()
  SeeAlso
    lastError
///

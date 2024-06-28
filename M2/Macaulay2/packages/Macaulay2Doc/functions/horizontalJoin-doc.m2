-- date: October 2018
-- author(s): Lily Silverstein
-- notes:

undocumented {
    (horizontalJoin, Net),
    (horizontalJoin, Nothing),
    (horizontalJoin, String),
    }

doc ///
 Key
   horizontalJoin
  (horizontalJoin, BasicList)
 Headline
  join nets or strings horizontally
 Usage
  horizontalJoin L
 Inputs
  L:BasicList
   a list or sequence containing nets and/or strings
 Outputs
   :Net
    or string obtained by concatenating the elements of {\tt L}
 Description
  Text
   If the list {\tt L} contains only strings, then {\tt horizontalJoin} acts
   like the command @TO concatenate@.
  Example
   L = {"some", "strings", "to", "join"}
   horizontalJoin L
   concatenate L
   demark(" ", L) --to insert spaces when concatenating strings
  Text
   Unlike {\tt concatenate}, {\tt horizontalJoin} can also be used on @TO Net@s.
  Example
   M = for i from 1 to 10 list if isPrime i then netList{toString i, toString i^2}
   horizontalJoin M
  Text 
   As the previous example shows, null arguments are allowed and ignored. 
   
   Nets and strings can be mixed in the input list. In this case, a string is interpreted as a net of 
   height one, with baseline below the string. The operator @TO(symbol ^, Net, ZZ)@ can be
   used to lower or raise the baseline of a string or net.
  Example
   R = QQ[x];
   N1 = for i from 1 to 5 list if isPrime i then netList{x^i, i*x, i:x, i} else toString i
   horizontalJoin N1
   N2 = for i from 1 to 5 list if isPrime i then netList{x^i, i*x, i:x, i} else (toString i)^-6
   horizontalJoin N2
  Text
   In the next example, we use {\tt horizontalJoin} to concatenate 
   the display of two random integer matrices. The
   matrices are converted to nets first with the command @TO net@.
  Example
   A = net matrix apply(3, i -> apply(3, j -> random(10)))
   B = net matrix apply(3, i -> apply(3, j -> random(10)))
   horizontalJoin(A,B)
  Text
   Nested sequences in the input are automatically spliced. For instance, in
   the next example the input is interpreted as {\tt \{A, B, A, B, A\}}. 
  Example
   horizontalJoin {(A, B), (A, B, (A))}
  Text
   However, the command {\tt horizontalJoin \{\{A, B\}, \{A, B, \{A\}\}\}} will
   throw an error, because nested lists are not automatically flattened. 
 Caveat
  This command will work with no arguments, or all null arguments. The net returned
  has zero @TO height@ and zero @TO depth@, which might be unexpected.
 SeeAlso
  Net
  netList
  (symbol |, Net, Net)
  (symbol ||, Net, Net)
  (symbol |, String, String)
  (symbol ^, Net, ZZ)
  stack
  "strings and nets"
///


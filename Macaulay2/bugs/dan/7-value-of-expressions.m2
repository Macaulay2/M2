value' Symbol := value					    -- do we really want this?

value' IndexedVariable := value				    -- do we really want this?

maybe we need

     ValueHolder = new Type of Expression
     value' ValueHolder := x -> x#0
     net ValueHolder := x -> net x#1
     toString ValueHolder := x -> toString x#1
     ...

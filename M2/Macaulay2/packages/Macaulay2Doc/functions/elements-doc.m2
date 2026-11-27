--- status: Draft
--- author(s): MES
--- notes: 

doc ///
  Key
    elements
    (elements,Tally)
    (elements,Set)
  Headline
    list of elements
  Usage
    elements S
  Inputs
    S:
      @ofClass Set@, or @ofClass Tally@
  Outputs
    :List
  Description
    Example
      set{1,2,3,1,2,4}
      elements oo
    Example
      tally apply(10, i -> random 5)
      elements oo
  SeeAlso
    tally
    set
///

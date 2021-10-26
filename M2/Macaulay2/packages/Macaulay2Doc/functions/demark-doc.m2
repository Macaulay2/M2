-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  demark
 Headline
  insert a string between elements of a list of strings
 Usage
  demark(d, L)
 Inputs
  d: String
  L: List
   of strings
 Outputs
  s: String
   the string obtained by concatenating the elements of {\tt L}, with copies
   of the string {\tt d} inserted between each pair
 Description
  Example
   demark("+", a..f)
   demark(" and ", 6:"more")
  Text
   To achieve a similar insertion while keeping the output as a list, see @TO mingle@.
  Example
   mingle(6: "more", 5: "and")
 SeeAlso
  insert
  join
  mingle
  "lists and sequences"
///
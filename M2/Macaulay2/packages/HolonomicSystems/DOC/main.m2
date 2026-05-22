doc ///
Node
  Key
    HolonomicSystems
  Headline
    Examples of Holonomic $D$-modules
  Description
    Text
      Examples and constructions of holonomic $D$-modules: GKZ
      hypergeometric systems, the Appell $F_1$ system, the canonical-series
      method for regular holonomic systems, and the ring of differential
      operators of a quotient ring.
    Tree
      :Some examples of $D$-modules
        @TOH "gkz"@
	@TOH "eulerOperators"@
	@TOH "toricIdealPartials"@
	@TOH "AppellF1"@
      :@TOH "Canonical Series Tutorial"@
        @TOH "distraction"@
	@TOH "cssExpts"@
	@TOH "cssExptsMult"@
	@TOH "isTorusFixed"@
	@TOH "solveFrobeniusIdeal"@
      :Differential Operators
        @TOH "diffOps"@
  Subnodes
   "gkz"
   "eulerOperators"
   "toricIdealPartials"
   "AppellF1"
   "Canonical Series Tutorial"
   "diffOps"

  References
    See the bibliography at @TO "WeylAlgebras :: Works Cited"@.
///

end--

restart
uninstallPackage "HolonomicSystems"
installPackage "HolonomicSystems"

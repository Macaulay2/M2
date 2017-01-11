needsPackage "Text"

assert ( html TEX ///\begin{pmatrix}b & c & d & e \\ c & d & e & f \end{pmatrix}/// == 
///
</i><table class="matrix" border="1"><tr><td><table><tr><td>b </td><td> c </td><td> d </td><td> e </td></tr><tr><td> c </td><td> d </td><td> e </td><td> f </td></tr></table></td></tr></table><i>
///
      )

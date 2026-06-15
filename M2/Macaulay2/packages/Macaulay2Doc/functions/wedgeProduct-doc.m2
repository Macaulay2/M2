doc ///
Key
     wedgeProduct
     (wedgeProduct, ZZ, ZZ, Module)
Headline
     the exterior multiplication map
Usage
     wedgeProduct(p, q, F)
Inputs
     p:ZZ
     q:ZZ
     F:Module
          must be free
Outputs
     :Matrix
          representing the multiplication map from @TT "exteriorPower(p, F) **
          exteriorPower(q, F)"@ to @TT "exteriorPower(p+q, F)"@
Description
     Example
          F = QQ^4
          wp = wedgeProduct(1, 1, F)
          L = exteriorPower(1, F)
          R = exteriorPower(1, F)
          wp * (L_0 ** R_0)
          wp * (L_0 ** R_1)
SeeAlso
     exteriorPower
///
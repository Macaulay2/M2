doc ///
  Key
     pfaffian
    (pfaffian, Matrix)
  Headline
    Pfaffian of a skew-symmetric matrix
  Usage
    pfaffian M
  Inputs
    M:Matrix -- must be skew-symmetric
  Outputs
    :{Number,RingElement} -- the Pfaffian of @VAR "M"@
  Description
    Text
      The @wikipedia "Pfaffian"@ of a $2n\times 2n$ skew-symmetric matrix
      $M=(m_{ij})$ is
      $$\frac{1}{2^nn!}\sum_{\sigma\in S_{2n}}\operatorname{sgn}(\sigma)\prod_{i=1}^nm_{\sigma(2i-1),\sigma(2i)},$$
      where $S_n$ is the symmetric group on $2n$ elements.
    Example
      M = matrix {{0, 1, 2, 3}, {-1, 0, 4, 5}, {-2, -4, 0, 6}, {-3, -5, -6, 0}}
      pfaffian M
    Text
      The square of the Pfaffian is the determinant.
    Example
      determinant M
    Text
      Skew-symmetric matrices with an odd number of rows and columns have
      Pfaffian zero.
    Example
      M = matrix {{0, 1, 2}, {-1, 0, 3}, {-2, -3, 0}}
      pfaffian M
 SeeAlso
   determinant
   pfaffians
///

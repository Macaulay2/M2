undocumented {(format, Sequence)}

doc ///
  Key
     format
    (format, CC)
    (format, RR)
    (format, String)
  Headline
    format a string or real number
  Synopsis
    Usage
      format s
    Inputs
      s:String
    Outputs
      :String
        obtained from @CODE "s"@ by inserting escape sequences, thereby
        preparing it for printing in a format suitable for reading in again
    Description
      Example
        s = "a\"b\"c"
        t = format s
        u = value t
        u == s
      Text
        The characters that are escaped are double quotes, backslashes, and
        all @wikipedia "control characters"@.  In particular, the outputted
        string will be valid @wikipedia "JSON"@.
      Example
        format(ascii(0..31) | "\"\\")
  Synopsis
    Usage
      format(s,k,l,t,e,x)
    Inputs
      s:ZZ
        the maximum number of significant decimal digits
        (default: @TO "printingPrecision"@).
        The special value @CODE "0"@ imposes no limit.
      k:ZZ
        how far to the right of the decimal point to go, at most
        (default: @TO "printingAccuracy"@).
        The special value @CODE "-1"@ imposes no limit.
      l:ZZ
        maximum number of leading zeroes
        (default: @TO "printingLeadLimit"@).
      t:ZZ
        maximum number of trailing zeroes
        (default: @TO "printingTrailLimit"@)
      e:String
        the separator between the mantissa and the exponent
        (default: @TO "printingSeparator"@)
      x:RR
        the number to be converted to a string
    Outputs
      :String
        the decimal representation of the number @CODE "x"@, prepared according
        to the parameters above
    Description
      Example
        format(10,1/3000.)
        format(10,6,1/3000.)
        format(10,6,2,1/3000.)
        format(10,300000.)
        format(10,-1,10,5,300000.)
        format(10,-1,10,4,300000.)
        format(10,-1,10,4,"E",300000.)
  SeeAlso
    toExternalString
///

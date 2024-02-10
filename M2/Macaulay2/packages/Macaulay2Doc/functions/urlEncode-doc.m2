doc ///
  Key
    urlEncode
    (urlEncode, Nothing)
    (urlEncode, String)
  Headline
    URL encoding
  Usage
    urlEncode s
  Inputs
    s:String
  Outputs
    :String
  Description
    Text
      Encode all of the characters in a URL except for "/", ":", "#", and
      the "unreserved characters" specified by
      @HREF{"https://www.rfc-editor.org/rfc/rfc3986.html", "RFC 3986"}@,
      i.e., the alphanumeric characters, "-", ".", "_", and "~", using
      @wikipedia "percent-encoding"@, a "%" followed by two
      hexadecimal digits representing the ASCII encoding of the
      character.
    Example
      urlEncode "https://en.wikipedia.org/wiki/Gröbner basis"
///

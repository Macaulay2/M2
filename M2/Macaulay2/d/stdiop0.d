use nets;

-- from stdiop
export Position := {
    filename:string,
    lineL:ushort, columnL:ushort, -- coordinates of the beginning
    lineR:ushort, columnR:ushort, -- coordinates of the endpoint
    lineF:ushort, columnF:ushort, -- coordinates of the focused location
    loadDepth:ushort };
export dummyPosition := Position(
    "-*dummy file name*-",
    ushort(0), ushort(0),
    ushort(0), ushort(0),
    ushort(0), ushort(0),
    loadDepth);
export tempPosition := Position(
   "-*temp*-",
    ushort(0), ushort(0),
    ushort(0), ushort(0),
    ushort(0), ushort(0),
    loadDepth);
export (s:Position) === (t:Position) : bool := (
    s == t || s.filename === t.filename
    && s.lineL == t.lineL && s.columnL == t.columnL
    && s.lineR == t.lineR && s.columnR == t.columnR
    && s.lineF == t.lineF && s.columnF == t.columnF);

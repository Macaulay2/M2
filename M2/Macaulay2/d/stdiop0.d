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

-- combine two positions with emphasis borrowed from beginning of the first one
export combinePositionL(L:Position, R:Position):Position := Position(
    L.filename, L.lineL, L.columnL, R.lineR, R.columnR, L.lineF, L.columnF, L.loadDepth);

-- combine two positions with emphasis borrowed from endpoint of the second one
export combinePositionR(L:Position, R:Position):Position := Position(
    L.filename, L.lineL, L.columnL, R.lineR, R.columnR, R.lineF, R.columnF, L.loadDepth);

-- combine two positions with emphasis borrowed from a third, central point
export combinePositionC(L:Position, R:Position, C:Position):Position := Position(
    L.filename, L.lineL, L.columnL, R.lineR, R.columnR, C.lineF, C.columnF, L.loadDepth);

-- combine two positions belonging to adjacent tokens (focus is on endpoint of the first one)
export combinePositionM(L:Position, R:Position):Position := Position(
    L.filename, L.lineL, L.columnL, R.lineR, R.columnR, L.lineR, L.columnR, L.loadDepth);

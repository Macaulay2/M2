str := x -> (
    if instance(x,VisibleList) then if #x==0 then "_" else concatenate apply(x,toString)
    else toString x
    )
almostsepdesc = (d,sep,Ktheory,Ktheory',Generic,Equivariant) -> (
    if d>9 then error "this value of d not implemented"; -- arbitrary
    l:=toList(0..d);
    s:=subsets l;
    if Equivariant then (
	if Generic then error "not implemented yet" else error "not implemented"; -- more precisely, doesn't exist!
	rhombi:=flatten flatten table(s,s,(a,b)->(
		s:=set a+set b-(set a)*(set b);
		-- TODO
		));
	) else rhombi = {};
    if Generic then (
	upTriangles = downTriangles = nonnull flatten table(s,s,(a,b)->
        if #a==#b+1 and isSubset(b,a) then {str a,str b, "↗"|toString first toList(set a - set b)}
        else if #b==#a+1 and isSubset(a,b) then {str a,str b, "↘"|toString first toList(set b - set a)}
        else if a==b then {str a,str b,if odd (#a) then "odd" else "even"});
	) else if Ktheory then (
	    upTriangles = nonnull flatten table(s,s,(a,b)->
        if #a==#b+1 and isSubset(b,a) then ( -- adding
            r := first toList(set a - set b);
            if all(b,s->s<r) then
            {str a,str b, "↗"|toString r}
            else if r>=sep then {str a,str b, "↗"|toString r,Kstyle}
            )
        else if #b==#a+1 and isSubset(a,b) then ( -- removing
            r = first toList(set b - set a);
            if all(a,s->s>r) then
            {str a,str b, "↘"|toString r}
            else if all(a,s->s>r or s<sep) then {str a,str b, "↘"|toString r,Kstyle}
            )
        else if a==b then if #a<=1 then {str a,str b,if odd (#a) then "odd" else "even"} else {str a,str b,if odd (#a) then "odd" else "even",Kstyle}
        );
    downTriangles = nonnull flatten table(s,s,(a,b)->
        if #a==#b+1 and isSubset(b,a) then (
            r := first toList(set a - set b);
            if all(b,s->s<r) then
            {str a,str b, "↗"|toString r}
            else if r<sep then {str a,str b, "↗"|toString r,Kstyle}
            )
        else if #b==#a+1 and isSubset(a,b) then (
            r = first toList(set b - set a);
            if all(a,s->s>r) then
            {str a,str b, "↘"|toString r}
            else if all(a,s->s>r or s>=sep) then {str a,str b, "↘"|toString r,Kstyle}
            )
        else if a==b and #a<=1 then {str a,str b,if odd (#a) then "odd" else "even"});
	) else (
    upTriangles = downTriangles = nonnull flatten table(s,s,(a,b)->
        if #a==#b+1 and isSubset(b,a) then (
            r := first toList(set a - set b);
            if all(b,s->s<r) then
            {str a,str b, "↗"|toString r}
            )
        else if #b==#a+1 and isSubset(a,b) then (
            r = first toList(set b - set a);
            if all(a,s->s>r) then
            {str a,str b, "↘"|toString r}
            )
        else if a==b and #a<=1 then {str a,str b,if odd (#a) then "odd" else "even"});
	);
    (upTriangles,downTriangles,rhombi)
    )

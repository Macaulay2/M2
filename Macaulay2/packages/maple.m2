if class mapleCommand === Symbol then mapleCommand = "!maple -q"

maple = s -> (
     MAPLE := openInOut mapleCommand << s << ";quit:" << endl << closeOut;
     r := get MAPLE;
     close MAPLE;
     while #r>0 and r#-1=="\n" do r = substring(r,0,#r-1);
     r
     )

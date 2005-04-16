if (pid = fork()) == 0 then (
     try "$:7500" << "hi there" << close;
     exit 0;
     )
sleep 2
get "$localhost:7500"
wait pid

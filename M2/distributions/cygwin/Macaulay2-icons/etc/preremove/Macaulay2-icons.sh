#! /bin/sh -e
LOGFILE=/var/log/Macaulay2-icons.log
echo logging stderr to $LOGFILE >&2
exec 2>$LOGFILE
date
set -x
for destination in DESKTOP STARTMENU
do case $destination in
       DESKTOP)
	   OPTS="--allusers --desktop"
	   SUBDIR=.
	   ;;
       STARTMENU)
	   OPTS="--allusers --smprograms" 
	   SUBDIR=Macaulay2
	   ;;
   esac
   rm -fv "`cygpath --unix $OPTS`"/"$SUBDIR/XWin + emacs + M2".lnk
   for display in 0
   do rm -fv "`cygpath --unix $OPTS`"/"$SUBDIR/emacs + M2 on $display".lnk
   done
   if [ $SUBDIR = Macaulay2 ]
   then destdir=`cygpath --unix $OPTS`
        [ -d "$destdir"/$SUBDIR ] && rmdir -v "$destdir"/$SUBDIR
   fi
done

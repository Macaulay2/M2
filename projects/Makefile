all: update ci comments index index-new
update:;	svn update
ci:;		svn ci -m 'update projects'
comments:;	egrep -l '^Title:' [a-z]* | while read f ; do egrep '^Title:' $$f | sed 's/^Title: *//' > $$f.comment ; done
index:;		index-html -s
index-new:;	umask 2; ~/to/Macaulay2/Scripts/index-new-html -t ~/to/Macaulay2/Style/trailer.html -l -s

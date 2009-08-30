include ../../Makefile.include
all: update ci comments index-new
update:;	svn update
ci:;		svn ci -m 'update projects'
comments:;	egrep -l '^Title:' [a-z]* | while read f ; do egrep '^Title:' $$f | sed 's/^Title: *//' > $$f.comment ; done
index-new:;	umask 2; index-new-html -l -s

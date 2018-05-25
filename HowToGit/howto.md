The best way to interact with the git repository of the workshop is to
make local copy and to interact with that copy.  You will need the
program git.

Then you can clone the workshop repository.  This is done only once.

    git clone https://github.com/Macaulay2/Workshop-2018-Leipzig.git

This will give you a local copy which from now on you can update with

    git pull

You use

    git push

to publish any commits you made to the main repository.  Before you
can push commits, you need to make some commits.  At this point you
shoud make some changes like editing a file or adding a file.  Then
use

    git add FILE

to tell git that you want to include the changes to FILE in your next
commit.  Once you are done listing the changes to be included in your
next commit, you can run

    git commit

to create a new commit.  Each commit is a point of reference for the
history and has a unique id (the id is a hash value).  At this point
you could and should git push your new commit to the main repository.

If there are some new remote changes you might need to pull again
before the push and merge those changes first.  At the workshop the
main repository will be very busy.  That's why we plan to work on
separate branches.  Please see:
https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell

The Workshop Area is a normal git repository.  Please refer to
tutorials on using git, e.g.: https://git-scm.com/doc

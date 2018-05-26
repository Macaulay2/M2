# Git for Workshop participants

The best way to interact with the git repository of the workshop is to
make local copy and to interact with that copy.  You will need the
program git.  See here https://git-scm.com/downloads if you don't have
it already.

## Interacting with the main respository

As a workshop participant, you are part of a github team and this
means you can directly interact with the workshop repository.

You can clone (i.e. get a local copy of) the workshop repository like
this.

    git clone https://github.com/Macaulay2/Workshop-2018-Leipzig.git

This is done only once.  From now on, you can update your local copy
with whatever is new on the github side using

    git pull

Later you will use

    git push

to publish any commits (i.e. changes) you made to the main repository.

## Making changes

A *commit* is a small bundle of changes to the state of the
repository.  The history of the repository consists of commits and the
relations among them.  Each commit has a unique ID which is a hash
value like (e703218f5caf...)

Making changes to the repository takes 3 steps.

1. dedicating changes to be included in the next commit
2. creating the commit
3. pushing the commit.

At this point we assume you have made some changes like editing a file
or adding a file to the directory you cloned.  You use

    git add FILE

to tell git that you want to include the changes to FILE in your next
commit.  Do this for all new files or changed files you want to
include in the next commit.  As a rule of thumb, one commit should
include all changes the belong to one logical change (i.e. fixing one
bug).  Commits should be as small as possible, but not smaller.

Once you are done adding changes you run

    git commit

to create a new commit.  Usually an editor will pop up and ask for a
commit message.  Each commit includes a text message describing the
commit.

Once you have at least one new commit, you can publish it on the main
repository using `git push`.  It will happen that there are some new
changes on the main repository.  You can not push unless you are
up-to-date.  You might need to `git pull` first which will merge any
remote changes into your history.  If this happens, the editor will
pop up again and ask you to write/confirm a new merge commit.  After
the merge you will be able to push.

## Slightly more advanced topics

- If you are just making changes to files that are already tracked,
  you can skip the adding phase and run `git commit -a` which
  automatically adds all changes to known files and creates a new
  commit.

- You can pass the commit message on the command line and skip the
  editor pop-up using the `-m` option:

    git commit -m 'This is the commit message'

- At the workshop the main repository will be very busy.  That's why
  we plan to work on separate branches.  Please see:
  https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell

- Please also refer to the github tutorial: 

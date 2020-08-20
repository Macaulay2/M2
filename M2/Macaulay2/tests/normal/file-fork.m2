-- Check fixes to https://github.com/Macaulay2/M2/issues/296

f1=rootPath|temporaryFileName();
f2=rootPath|temporaryFileName();
f2<<"2"<<close;
t=fork();
if (t==0) then (
  -- Is the child
  f1<<toString(1)<<close;
  exit(0);
) else (
  -- is the parent
  wait(t);
  assert(1===value get(f1));
  assert(2===value get(f2));
  f3=rootPath|temporaryFileName();
  f3<<"3"<<close;
  assert(3===value get(f3));
);


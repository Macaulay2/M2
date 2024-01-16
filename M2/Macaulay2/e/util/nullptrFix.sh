#! /bin/zsh
# Use this shell script in the e directory as ./util/nullPtrFix.sh
# This script fixes any uses of 0 or NULL for null pointers and replaces it with nullptr
for x in **/*.cpp(N) **/*.hpp(N); do
   echo $x
   /opt/homebrew/opt/llvm@16/bin/clang-tidy -p ../.. --checks="-*,modernize-use-nullptr" --fix $x
done

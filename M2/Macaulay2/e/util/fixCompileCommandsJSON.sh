#! /bin/zsh
# This script should be ran in the M2 directory as ./Macaulay2/e/util/fixCompileCommandsJSON.sh
sed 's;-Xclang;;' /Users/frank/Macaulay2/M2-frank/M2/BUILD/frank/builds.tmp/cmake-appleclang/compile_commands.json > temp.json
sed 's;-Xpreprocessor -fopenmp /opt/homebrew/opt/libomp/lib/libomp.dylib;;' temp.json > compile_commands.json
rm temp.json

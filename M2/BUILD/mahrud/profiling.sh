# Snippets for profiling M2 binaries

# Bloaty is Google's size profiler for binaries: https://github.com/google/bloaty
bloaty -s vm -d inputfiles `find ./build/Macaulay2/ | grep "\.o"`
bloaty -s vm -d inputfiles,symbols `find ./build/Macaulay2/ | grep "\.o"`

# Flamegraphs: http://www.brendangregg.com/perf.html#JIT_Symbols
perf record -F 99 -a -g -- ./M2 --script ../mahrud/fglm.m2
perf script | stackcollapse-perf.pl > out.perf-folded
flamegraph.pl out.perf-folded > perf-kernel.svg; eog perf-kernel.svg

# Also see: https://www-users.math.umn.edu/~mahrud/journal/profiler/


# not working:
perf record -e syscalls:sys_enter_mmap -a -g -- ./usr-dist/x86_64-Linux-Fedora-31/bin/M2-binary --script ../mahrud/fglm.m2
perf script | stackcollapse-perf.pl | flamegraph.pl --color=mem \
    --title="Heap Expansion Flame Graph" --countname="calls" > out.svg; eog out.svg


./usr-dist/x86_64-Linux-Fedora-31/bin/M2-binary --script ../mahrud/fglm.m2 & \
/usr/share/bcc/tools/stackcount -p $! -U c:malloc > out.stacks


CPUPROFILE=./prof.out ./usr-dist/x86_64-Linux-Fedora-31/bin/M2-binary --script ../mahrud/fglm.m2
pprof --web ./usr-dist/x86_64-Linux-Fedora-31/bin/M2-binary prof.out_*

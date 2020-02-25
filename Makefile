# this is a file to create build directories here
all:
	mkdir -p builds
	mkdir -p builds/debug
	cd builds/debug; cmake -DCMAKE_BUILD_TYPE=DEBUG ../../M2/

release:
	mkdir -p builds
	mkdir -p builds/release
	cd builds/release; cmake Verbose=1 -LA -DCMAKE_BUILD_TYPE=RELEASE ../../M2/

latestclang:
	mkdir -p builds
	mkdir -p builds/latestclang
	cd builds/latestclang; \
		CC="/usr/local/opt/llvm/bin/clang" \
		CXX="/usr/local/opt/llvm/bin/clang++" \
		cmake -DCMAKE_BUILD_TYPE=DEBUG ../../M2/

gcc9:
	mkdir -p builds
	mkdir -p builds/gcc9
	cd builds/gcc9; \
		CC=gcc-9 CXX=g++-9 \
		cmake -DCMAKE_BUILD_TYPE=RELWITHDEBINFO ../../M2/

xcode:
	mkdir -p builds
	mkdir -p builds/xcode
	cd builds/xcode; cmake -G Xcode ../../M2/


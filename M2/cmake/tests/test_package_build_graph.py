#!/usr/bin/env python3
"""Configure all real packages with a stub runtime; validate the generated graph."""
from pathlib import Path
import atexit,tempfile,shutil,subprocess,re
from collections import defaultdict, deque
repo=Path(__file__).resolve().parents[3]
root=Path(tempfile.mkdtemp(prefix='m2-full-package-graph-'))
atexit.register(shutil.rmtree, root)
package_count=sum(bool(re.fullmatch('[a-zA-Z0-9]+', line)) for line in (repo/'M2/Macaulay2/packages/=distributed-packages').read_text().splitlines())
src=root/'src';src.mkdir();(src/'cmake').mkdir()
shutil.copytree(repo/'M2/cmake/package-install',src/'cmake/package-install')
shutil.copy(repo/'M2/cmake/package-dependencies.cmake',src/'cmake/package-dependencies.cmake')
(src/'main.c').write_text('int main(void) {return 0;}\n')
(src/'latex.cmake').write_text('macro(_ADD_LATEX_TARGET)\nendmacro()\n')
(src/'CMakeLists.txt').write_text('''cmake_minimum_required(VERSION 3.30)
project(FullPackageGraph C)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
set(M2_DIST_PREFIX "${CMAKE_BINARY_DIR}/dist")
set(M2_INSTALL_BINDIR bin)
set(M2_INSTALL_LIBDIR lib)
set(M2_INSTALL_DATADIR share)
set(M2_INSTALL_DOCDIR doc)
set(M2_INSTALL_INFODIR info)
set(BUILD_DOCS ON)
set(BUILD_TESTING OFF)
file(MAKE_DIRECTORY "${M2_DIST_PREFIX}/share/Core")
configure_file(main.c "${M2_DIST_PREFIX}/share/Core/tvalues.m2" COPYONLY)
add_executable(M2-binary main.c)
add_custom_target(M2-core DEPENDS M2-binary)
file(STRINGS "'''+str(repo)+'''/M2/Macaulay2/packages/=distributed-packages" DISTRIBUTED_PACKAGES REGEX "^[a-zA-Z0-9]+$")
add_subdirectory("'''+str(repo)+'''/M2/Macaulay2/packages" packages)
''')
for generator in ['Ninja','Unix Makefiles']:
    build=root/generator.replace(' ','-')
    with (root/(generator+'.log')).open('w') as log:
        r=subprocess.run(['cmake','-S',str(src),'-B',str(build),'-G',generator],stdout=log,stderr=subprocess.STDOUT)
    assert r.returncode==0,(root/(generator+'.log')).read_text()
    if generator=='Ninja':
        r=subprocess.run(['ninja','-C',str(build),'-t','commands','install-packages'],capture_output=True,text=True)
        assert r.returncode==0,r.stdout+r.stderr
        assert r.stdout.count('installPackage(')==package_count,r.stdout[-2000:]
        dot=subprocess.check_output(['ninja','-C',str(build),'-t','graph','install-packages'],text=True)
        edges=defaultdict(set); degree=defaultdict(int)
        for a,b in re.findall(r'"([^"]+)" -> "([^"]+)"',dot):
            if b not in edges[a]:
                edges[a].add(b);degree[b]+=1
            degree[a]+=0
        queue=deque(n for n,d in degree.items() if d==0);count=0
        while queue:
            n=queue.popleft();count+=1
            for b in edges[n]:
                degree[b]-=1
                if degree[b]==0:queue.append(b)
        assert count==len(degree),'cycle in generated Ninja graph'
        print(f'PASS: all {package_count} real package installation targets have an acyclic Ninja build graph',flush=True)
    print('PASS: full package CMake configuration:',generator,flush=True)
print(root)

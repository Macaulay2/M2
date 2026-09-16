#!/usr/bin/env python3
"""Exercise the real package CMake rules with a small fake installer."""
from pathlib import Path
import atexit, subprocess, tempfile, shutil, time, sys
repo=Path(__file__).resolve().parents[3]
root=Path(tempfile.mkdtemp(prefix='m2-incremental-'))
atexit.register(shutil.rmtree, root)
print(root, flush=True)
for generator in ['Ninja','Unix Makefiles']:
    base=root/generator.replace(' ','-'); src=base/'src'; build=base/'build'
    packages=src/'packages'; packages.mkdir(parents=True)
    shutil.copy(repo/'M2/Macaulay2/packages/CMakeLists.txt',packages/'CMakeLists.txt')
    for pkg in ['Style','FirstPackage','Macaulay2Doc','Example']:
        (packages/f'{pkg}.m2').write_text(pkg)
        (packages/pkg).mkdir()
    (packages/'LanguageServer').mkdir()
    (packages/'LanguageServer/M2-language-server').touch()
    (src/'cmake').mkdir()
    shutil.copytree(repo/'M2/cmake/package-install', src/'cmake/package-install')
    (src/'cmake/package-dependencies.cmake').write_text('set(M2_DEPENDENCY_PACKAGES Style FirstPackage Macaulay2Doc Example)\n')
    (src/'latex.cmake').write_text('macro(_ADD_LATEX_TARGET)\nendmacro()\n')
    (src/'m2').mkdir()
    (src/'m2/core.m2').write_text('Core source fixture')
    (src/'main.c').write_text('int main(void) { return 0; }\n')
    (src/'fake-m2.py').write_text('''#!'''+sys.executable+'''
import sys,re
from pathlib import Path
root=Path(__file__).resolve().parents[2]
for operation, pkg in re.findall(r'(installPackage|check)\\("([^\"]+)"', ' '.join(sys.argv)):
    if operation == 'check':
        assert (root/'dist/lib/Macaulay2'/pkg/'.cmake-installed').exists(), 'checked before installation completed'
        with (root/'checks').open('a') as f: f.write(pkg+'\\n')
        if (root/('fail-check-'+pkg)).exists(): sys.exit(1)
        continue
    with (root/'calls').open('a') as f: f.write(pkg+'\\n')
    if (root/('fail-'+pkg)).exists(): sys.exit(1)
    marker=root/'dist/lib/Macaulay2'/pkg/'.installed'
    marker.parent.mkdir(parents=True,exist_ok=True)
    marker.touch()
    info=root/'dist/info'/ (pkg+'.info')
    info.parent.mkdir(parents=True,exist_ok=True)
    if not (root/('gzip-fail-'+pkg)).exists(): info.write_text(pkg)

''')
    (src/'fake-m2.py').chmod(0o755)
    (src/'CMakeLists.txt').write_text('''cmake_minimum_required(VERSION 3.30)
project(IncrementalPackages C)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
set(M2_DIST_PREFIX "${CMAKE_BINARY_DIR}/dist")
set(M2_INSTALL_BINDIR bin)
set(M2_INSTALL_LIBDIR lib)
set(M2_INSTALL_DATADIR share)
set(M2_INSTALL_DOCDIR doc)
set(M2_INSTALL_INFODIR info)
set(DISTRIBUTED_PACKAGES "Style;FirstPackage;Macaulay2Doc;Example")
set(BUILD_DOCS ON)
set(BUILD_TESTING OFF)
file(MAKE_DIRECTORY "${M2_DIST_PREFIX}/bin" "${M2_DIST_PREFIX}/share/Core")
configure_file(fake-m2.py "${M2_DIST_PREFIX}/bin/M2" COPYONLY)
configure_file(main.c "${M2_DIST_PREFIX}/share/Core/tvalues.m2" COPYONLY)
add_executable(M2-binary main.c)
add_custom_target(M2-core DEPENDS M2-binary)
add_subdirectory(packages)
''')
    def configure(*args):
        r=subprocess.run(['cmake','-S',str(src),'-B',str(build),'-G',generator,*args],capture_output=True,text=True)
        assert r.returncode==0,r.stdout+r.stderr
    def run(expected,fail=False,target='install-packages',checks=()):
        (build/'calls').write_text('')
        (build/'checks').write_text('')
        r=subprocess.run(['cmake','--build',str(build),'--target',target,'--parallel','4'],capture_output=True,text=True)
        assert (r.returncode!=0)==fail,r.stdout+r.stderr
        calls=(build/'calls').read_text().splitlines()
        assert sorted(calls)==sorted(expected),(generator,calls,expected,r.stdout+r.stderr)
        checked=(build/'checks').read_text().splitlines()
        assert sorted(checked)==sorted(checks),(generator,checked,checks,r.stdout+r.stderr)
    def touch(path):
        time.sleep(1.05); path.touch()
    configure()
    (build/'fail-Macaulay2Doc').touch()
    run(['Style','FirstPackage','Macaulay2Doc'],True)
    assert not (build/'dist/lib/Macaulay2/Macaulay2Doc/.cmake-installed').exists()
    (build/'fail-Macaulay2Doc').unlink()
    run(['Macaulay2Doc','Example'])
    run([])
    configure(); run([])
    touch(packages/'Example.m2'); run(['Example']); run([])
    touch(packages/'Style.m2'); run(['Style','FirstPackage','Macaulay2Doc','Example'])
    configure('-DRerunExamples=true'); run(['Style','FirstPackage','Macaulay2Doc','Example']); run([])
    touch(packages/'Example/new.m2'); run(['Example'])
    (packages/'Example/new.m2').unlink(); run(['Example'])
    (build/'gzip-fail-Example').touch(); touch(packages/'Example.m2')
    run(['Example'],True)
    assert not (build/'dist/lib/Macaulay2/Example/.cmake-installed').exists()
    (build/'gzip-fail-Example').unlink(); run(['Example']); run([])
    (build/'dist/info/Example.info.gz').unlink(); run(['Example'])
    (build/'dist/lib/Macaulay2/Example/.installed').unlink(); run(['Example'])
    touch(src/'m2/core.m2'); run(['Style','FirstPackage','Macaulay2Doc','Example']); run([])
    touch(src/'main.c'); run(['Style','FirstPackage','Macaulay2Doc','Example']); run([])
    print('PASS:',generator,'failure/restart, no-op, sources, prerequisites, options, source additions/removals, compression failure, missing outputs, runtime changes',flush=True)

    # Combined targets reuse installations, but checks are explicit actions.
    all_packages=['Style','FirstPackage','Macaulay2Doc','Example']
    run([],target='all-packages',checks=all_packages)
    run([],target='all-packages',checks=all_packages)
    run([],target='all-Example',checks=['Example'])
    touch(packages/'Example.m2')
    run(['Example'],target='all-Example',checks=['Example'])
    (build/'fail-Example').touch(); touch(packages/'Example.m2')
    run(['Example'],True,target='all-Example')
    (build/'fail-Example').unlink()
    run(['Example'],target='all-Example',checks=['Example'])
    (build/'fail-check-Example').touch()
    run([],True,target='all-Example',checks=['Example'])
    (build/'fail-check-Example').unlink()
    run([],target='all-Example',checks=['Example'])
    run([])
    print('PASS:',generator,'combined targets reuse completed installs, rerun checks, and handle install/check failures',flush=True)

    # A selected package must build its imports, even when omitted from PACKAGES.
    (packages/'Provider.m2').write_text('Provider')
    manifest=src/'cmake/package-dependencies.cmake'
    manifest.write_text('set(M2_DEPENDENCY_PACKAGES Style FirstPackage Macaulay2Doc Example Provider)\nset(M2_PACKAGE_IMPORTS_Example Provider)\nset(M2_PACKAGE_SOURCES_Example Provider)\n')
    top=src/'CMakeLists.txt'
    top.write_text(top.read_text().replace('Style;FirstPackage;Macaulay2Doc;Example', 'Example'))
    configure()
    (build/'fail-Provider').touch()
    run(['Provider'],True)
    (build/'fail-Provider').unlink()
    run(['Provider','Example']); run([])
    # Literal body imports may form a cycle. They affect freshness, not ordering.
    manifest.write_text(manifest.read_text()+'set(M2_PACKAGE_SOURCES_Provider Example)\n')
    configure(); run(['Provider','Example']); run([])
    touch(packages/'Example.m2'); run(['Provider','Example']); run([])
    print('PASS:',generator,'selected-package import closure and cyclic source dependencies',flush=True)

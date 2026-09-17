#!/usr/bin/env bash
set -euo pipefail

# These paths must remain the same in the publisher and every consumer.
source_dir=/opt/m2/source
build_dir=/opt/m2/build
mkdir -p /opt/m2/logs /opt/m2/artifacts
collect_logs() {
    if [[ -d "$build_dir" ]]; then
        (cd "$build_dir" && find . -type f \( -name '*.errors' -o -name CMakeCache.txt \
            -o -name CMakeConfigureLog.yaml -o -name LastTest.log -o -name LastTestsFailed.log \
            -o -name config.log -o -name '*-out.log' -o -name '*-err.log' \) -print0 |
            tar --null -czf /opt/m2/logs/diagnostics.tar.gz -T -) || true
    fi
}
trap 'status=$?; collect_logs; exit "$status"' EXIT

# Keep nanosecond mtimes outside the image-layer format: Ninja records them in
# .ninja_deps. PAX tar preserves them across registry image export/import.
if [[ -f /opt/m2/build-cache.tar ]]; then
    tar -xf /opt/m2/build-cache.tar -C /opt/m2
    rm /opt/m2/build-cache.tar
fi

python3 /input/.github/ci/prepare_source.py /input "$source_dir" "$build_dir"
revision=$(python3 -c 'import json; print(json.load(open("/input/.ci-snapshot.json"))["revision"])')
export CMAKE_BUILD_PARALLEL_LEVEL=${CMAKE_BUILD_PARALLEL_LEVEL:-2}
# Objects themselves are cached; avoid storing a second copy in ccache.
export CCACHE_DISABLE=1 OMP_NUM_THREADS=2 OPENBLAS_NUM_THREADS=1
cmake -S "$source_dir/M2" -B "$build_dir" -G Ninja \
    -DCMAKE_BUILD_TYPE=Release -DBUILD_NATIVE=OFF -DGIT_SUBMODULE=OFF \
    -DSTATIC_BOOST=OFF -DBUILD_TESTING=ON -DWITH_MAPLE=OFF -DRerunExamples=true -DCMAKE_INSTALL_PREFIX=/usr \
    -DPARALLEL_JOBS="$CMAKE_BUILD_PARALLEL_LEVEL" -DCOMMIT_COUNT=0 -DGIT_COMMIT="$revision"
cmake --build "$build_dir" --target build-libraries build-programs
cmake --build "$build_dir" --target M2-core M2-emacs M2-unit-tests \
    memtailor-unit-tests mathic-unit-tests mathicgb-unit-tests
# Install only stale packages; checks deliberately run on every invocation.
cmake --build "$build_dir" --target all-packages
"$build_dir/M2" -q --check 1
"$build_dir/M2" -q --check 2
"$build_dir/M2" -q --check 3
cmake --build "$build_dir" --target M2-tests
ctest --test-dir "$build_dir" -j 1 --output-on-failure -R unit-tests
ctest --test-dir "$build_dir" -j 2 --output-on-failure -R ComputationsBook
(cd "$build_dir" && cpack -G DEB)
cp "$build_dir"/Macaulay2-*.deb /opt/m2/artifacts/
printf '%s\n' "$revision" > /opt/m2/revision

if [[ "${SAVE_BUILD_CACHE:-false}" == true ]]; then
    collect_logs
    tar --format=pax -cf /opt/m2/build-cache.tar -C /opt/m2 source build
    rm -rf "$source_dir" "$build_dir"
fi

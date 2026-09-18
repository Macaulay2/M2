#!/usr/bin/env bash
set -euo pipefail
stage=${1:-build}
case "$stage" in dependencies|build|test) ;; *) echo "Unknown stage: $stage" >&2; exit 2 ;; esac

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

export CMAKE_BUILD_PARALLEL_LEVEL=${CMAKE_BUILD_PARALLEL_LEVEL:-2}
# Bound nested numerical threads; parallelism comes from independent build jobs.
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
export CCACHE_DIR=/opt/m2/ccache CCACHE_MAXSIZE=512M CCACHE_COMPILERCHECK=content
unset CCACHE_DISABLE
if [[ "$stage" != test ]]; then
    python3 /input/.github/ci/prepare_source.py /input "$source_dir" "$build_dir"
    revision=$(python3 -c 'import json; print(json.load(open("/input/.ci-snapshot.json"))["revision"])')
    source /input/.github/ci/container-environment.sh
    cmake -S "$source_dir/M2" -B "$build_dir" -G Ninja \
        "${cmake_environment_args[@]}" -DWITH_MAPLE=OFF \
        -DRerunExamples=true -DRespectCachedExampleOutput=ON -DCacheExampleOutput=false \
        -DPARALLEL_JOBS="$CMAKE_BUILD_PARALLEL_LEVEL" -DCOMMIT_COUNT=0 -DGIT_COMMIT="$revision"
    cmake --build "$build_dir" --target build-libraries build-programs
    if [[ "$stage" == build ]]; then
        cmake --build "$build_dir" --target M2-core M2-emacs M2-unit-tests \
            memtailor-unit-tests mathic-unit-tests mathicgb-unit-tests
        cmake --build "$build_dir" --target install-packages
    fi
    ccache --show-stats
    printf '%s\n' "$revision" > /opt/m2/revision
else
    # Package checks always run, using the installations from the build job.
    cmake --build "$build_dir" --target check-packages
    "$build_dir/M2" -q --check 1
    "$build_dir/M2" -q --check 2
    "$build_dir/M2" -q --check 3
    cmake --build "$build_dir" --target M2-tests
    ctest --test-dir "$build_dir" -j 1 --output-on-failure -R unit-tests
    ctest --test-dir "$build_dir" -j "$CMAKE_BUILD_PARALLEL_LEVEL" --output-on-failure -R ComputationsBook
    (cd "$build_dir" && cpack -G DEB)
    cp "$build_dir"/Macaulay2-*.deb /opt/m2/artifacts/
fi

if [[ "$stage" != test || "${SAVE_BUILD_CACHE:-false}" == true ]]; then
    collect_logs
    tar --format=pax -cf /opt/m2/build-cache.tar -C /opt/m2 source build
    rm -rf "$source_dir" "$build_dir"
fi

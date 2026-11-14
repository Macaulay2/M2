# Compiler/library configuration included in the environment cache key.
# Keep package-installation policy in container-build.sh, not in this array.
# Changes to generators, fixed paths or archive compatibility require a bump
# to cache-version even when these CMake options stay unchanged.
cmake_environment_args=(
    -DCMAKE_BUILD_TYPE=Release
    -DBUILD_NATIVE=OFF
    -DGIT_SUBMODULE=OFF
    -DSTATIC_BOOST=OFF
    -DBUILD_TESTING=ON
    -DCMAKE_INSTALL_PREFIX=/usr
)

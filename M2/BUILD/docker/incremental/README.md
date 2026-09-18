# Incremental Linux CMake CI

This CI uses the incremental package installation rules from #4704. It replaces
only the Linux/CMake variant of `test_build.yml`; macOS and autotools retain their
existing jobs.

## Development builds and PR builds

Every push to `development` runs
[Build development container](../../../../.github/workflows/development-container.yml).
The shared [container workflow](../../../../.github/workflows/cmake-container.yml)
first tries an image for the preceding commit, then the latest compatible
successful development build. If neither can be pulled, it builds a fresh
Ubuntu 24.04 environment from this directory's Dockerfile.

After a successful build and tests, it publishes to GitHub Container Registry
under the current repository owner's namespace. For the upstream repository:


- `ghcr.io/macaulay2/m2-ci-cmake:sha-<commit>-<environment-key>` identifies the
  development revision used to build the image.
- `ghcr.io/macaulay2/m2-ci-cmake:development-<environment-key>` is the moving cache
  tag. Before advancing it, the workflow checks that the built revision is still
  the development branch head. PRs prefer their base commit's image and otherwise
  use this cache tag. A stale cache affects performance, not which source is tested.

The selected image is resolved to a local image ID before use. PR jobs check out
and test GitHub's PR merge revision, including pinned submodules, on top of that
image. They never publish their resulting containers. Pushes to development also
run the tests before publication. The existing scheduled and manually dispatched
Build and Test workflow uses the same Linux/CMake job for its checked-out commit.

## Testing in a personal fork

The workflows also work in forks. In `dimpase/M2`, both publication and cache
lookup use `ghcr.io/dimpase/m2-ci-cmake`; they do not write to the upstream
registry. The OCI source label links to the fork. After merging upstream, the
same workflow automatically uses `ghcr.io/macaulay2/m2-ci-cmake`. Images do not
need transferring: the first upstream run builds and publishes its own cache.

To test before merging, enable Actions in the fork and set the repository Actions
variable `M2_CI_DEVELOPMENT_BRANCH` to `incremental-container-ci`. Then push a
commit to that branch. Only pushes to the selected branch can publish; PR jobs
never publish. The variable defaults to `development` when unset. Make the fork's
GHCR package public after its first publication, just as for the upstream package.

A subsequent push to the selected branch tests incremental reuse. To test PR
consumption, open a PR in the fork against `development` with these workflows in
its merge revision, or manually dispatch **Build and Test Macaulay2** on the test
branch if manual dispatch is available. Linux/CMake is enabled in forks; the
existing restrictions on other build variants are unchanged. Unset the variable
when finished testing, or switch it to the fork's intended development branch.

## What is reused

Sources live at `/opt/m2/source` and the CMake build at `/opt/m2/build` in both
publisher and consumer containers. The image retains object files, external
libraries, Ninja metadata, installed packages and their completion stamps inside
`/opt/m2/build-cache.tar`. This PAX-format archive preserves nanosecond timestamps
that Ninja records, independently of container image-layer timestamp precision.
Each consumer extracts it back to the same paths. Neither directory is a bind
mount: the publisher must be able to archive their contents.

The source snapshot comes from `git archive`, recursively including the pinned
submodules. Untracked files, local modifications, `.git` directories and checkout
credentials are excluded. The container overlays it using `rsync --checksum`
without preserving incoming timestamps. Identical files keep the cached source
mtime; changed files receive the current time, even when their Git/archive mtime
predates the cached outputs. CMake then configures and rebuilds normally.

Building a source archive also keeps the runtime's Git-description field at its
release fallback, instead of changing `Core/version.m2` for every commit and
invalidating all installed packages. Exact revision provenance is recorded in
`/opt/m2/revision`, the image tag and OCI revision label, and the Debian package
filename. This is a CI build asset, not a release image with Git checkout metadata.

Changes to the Dockerfile, CI build/transfer scripts, top-level CMake settings,
CMake environment/library modules, M2 version or submodule revisions select a new
environment key and therefore a clean environment. Ordinary package edits and
manifest changes keep the same key. Deleted files or file-type changes discard
the cached build directory, since configure-time staging copies and external
build stamps may otherwise retain removed inputs. This is a conservative fallback
until those deletion cases can be handled incrementally.

The container configures with `WITH_MAPLE=OFF` (from #4722) and
`RespectCachedExampleOutput=ON`. The latter inspects each loaded package's
resolved `UseCachedExampleOutput` option before installing it. Packages that
request shipped examples (for example NCAlgebra when Bergman is absent) receive
`RerunExamples=false`. Other packages retain `RerunExamples=true`; Maple's
per-package exceptions also remain in effect. All packages stay selected,
including their independent checks. Missing or stale shipped example caches
still cause errors rather than silently passing.

The build job uses `install-packages` to install only packages whose inputs
changed. It exports the completed container as a two-day workflow artifact. A
separate job on a fresh runner imports this filesystem, restores the PAX archive,
and uses `check-packages` to run package checks every time. Each job has its own
180-minute allowance. When testing fails, rerun failed jobs while the intermediate
artifact is retained; the successful build job need not run again. Intermediate
artifacts are not published as successful development images. Examples rerun when a package needs installation, including after a
dependency changes, subject to the package cache policy above. The job also runs
the Core checks, C++ unit tests and ComputationsBook tests, and produces a Debian
package.
Logs and packages are uploaded as workflow artifacts. A failed build or test never
replaces the development cache.

Published images are flattened with container export/import, avoiding an
unbounded accumulation of old object files in layers. Consequently each snapshot
uploads a full compressed filesystem; timings and download size should be
compared against the previous CI before claiming a particular speedup. Building
from scratch remains supported and is the fallback for a missing registry asset.

## Compiler cache and parallelism

The image retains a bounded 512 MiB ccache at `/opt/m2/ccache`, in addition to
compiled objects. It can reuse identical C/C++ compilations after outputs are
invalidated or removed, and its statistics are printed after each build. It does
not cache M2 package installations or examples. A change to M2-core can invalidate
all package installations; ccache does not eliminate that work. External build
systems are not automatically covered by CMake's compiler launcher.

Each runner chooses the smaller of `nproc` and its RAM divided by 2 GiB (minimum
one job), and reports that limit in the job summary. This controls CMake/Ninja,
external-library build parallelism and ComputationsBook tests. The container no
longer has a hard two-CPU quota. OpenMP and OpenBLAS use one thread per process to
avoid multiplying the package-level parallelism. Declared imports and the
Style/FirstPackage/Macaulay2Doc bootstrap still impose ordering; a package's own
checks run sequentially. C++ unit suites remain serial, as in the previous CI.
These limits balance memory use against CPU concurrency rather than assuming
that every command can safely run at once.

## Enabling and maintaining the registry asset

1. Merge the incremental package rules and this workflow into `development`.
2. Allow the development workflow to publish packages with its `GITHUB_TOKEN`.
   Only the publisher caller requests `packages: write`; PR jobs request only
   `contents: read`, and no token or Docker socket is passed into the container.
3. After the first successful publication, make the `m2-ci-cmake` GHCR package
   public in its package settings. New GHCR packages can initially be private,
   even for public source repositories. Anonymous pulls let fork PRs use the
   cache without secrets. Until public access is enabled, they fall back to a
   clean build. See [GitHub's Container registry documentation](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry).
4. Review required status-check names: the Linux/CMake job now calls a reusable
   workflow, so its displayed check name gains the called job's name.

To refresh OS packages or recover from an unsuitable cache, increment
`.github/ci/cache-version` and merge that change into `development`. This selects
a new environment key and rebuilds the system dependencies. Alternatively,
dispatch **Build development container** on `development` with **clean** enabled
once the workflow is available for manual dispatch (GitHub requires its dispatch
trigger to exist on the repository's default branch; see the
[workflow dispatch documentation](https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows#workflow_dispatch)). Apt package
updates are not detected by the source-derived environment key; periodically run
this clean refresh. Old commit-tagged images can be removed through GHCR package
version management; retain the desired development cache tags. This workflow
does not delete registry versions automatically.

## Local checks

From the repository root, with Python 3.12+, Git, rsync, CMake, Ninja and a C
compiler installed:

```sh
python3 -m unittest discover -s .github/ci -p 'test_*.py' -v
bash -n .github/ci/container-build.sh
```

The tests check pinned-submodule export, exclusion of Git credentials and
untracked files, cache-key changes, timestamp preservation, deletion handling,
and an actual incremental Ninja build after a PAX archive round trip. To reproduce the container build with
Docker (use an unused snapshot destination):

```sh
git submodule update --init --recursive
python3 .github/ci/source_snapshot.py . /tmp/m2-ci-source
docker build -t m2-ci-environment M2/BUILD/docker/incremental
docker run --rm --init --cap-drop ALL --security-opt no-new-privileges --cpus 2 \
  --mount type=bind,source=/tmp/m2-ci-source,target=/input,readonly \
  m2-ci-environment bash -c 'bash /input/.github/ci/container-build.sh build && bash /input/.github/ci/container-build.sh test'
```

The snapshot contains committed sources, so commit local changes before using
this reproduction command. Substitute a published development image for
`m2-ci-environment` to test reuse. These runs are disposable and publish nothing.

The container sets `CacheExampleOutput=false`: generated example output stays in
the build tree rather than being copied back into source `examples/` directories.
Reading committed example caches remains enabled according to each package's
policy. Writing caches into the source snapshot makes the next source overlay
look like an input change, causing unnecessary package reinstallations.

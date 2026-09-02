#! /usr/bin/env sh
# Build libCrysP and run its unit tests.
#
# Usage: test/test.sh [build_dir] [ctest options...]
#   build_dir   CMake build directory, relative to the libcrysp directory (default: build).
#               Must have been configured already, e.g. `cmake -B build -DCMAKE_BUILD_TYPE=debug`.
#   The remaining arguments are passed to ctest, e.g. `-V` for full output or `-R material` to select tests.
set -e

cd "$(dirname "$0")/.."

BUILD_DIR="${1:-build}"
[ $# -gt 0 ] && shift

if [ ! -f "$BUILD_DIR/CMakeCache.txt" ]; then
    echo "error: '$BUILD_DIR' is not a configured build directory. Run 'cmake -B $BUILD_DIR' first." >&2
    exit 1
fi

cmake --build "$BUILD_DIR" --parallel
ctest --test-dir "$BUILD_DIR" --output-on-failure "$@"

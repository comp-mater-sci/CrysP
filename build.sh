#! /usr/bin/env sh
set -e

#Builds VEF
#Options:
#   -c: compiler (ifort/gfortran; default ifort)
#   -b: build type (debug/release; default release)
#   -t: enable tracing

export FC="ifx"
BUILD_TYPE="release"
TRACE="0"


while [ True ]; do
if [ "$1" = "-c" ]; then
    export FC="$2"
    shift 2
elif [ "$1" = "-b" ]; then
    BUILD_TYPE="$2"
    shift 2
elif [ "$1" = "-t" ]; then
    TRACE="1"
    shift 1
elif [ "$1" = "--clean" ]; then
    rm -rf AlTay/release AlTay/debug VEF/build VEF/bin
    shift 1
else
    break
fi
done

cd AlTay
cmake -B $BUILD_TYPE/build -DCMAKE_BUILD_TYPE=$BUILD_TYPE
cmake --build ${BUILD_TYPE}/build --parallel --target install

cd ../VEF
cmake -B build -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_INSTALL_PREFIX=.
cmake --build build --parallel --target install

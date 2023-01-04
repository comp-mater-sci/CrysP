#! /usr/bin/env sh
set -e

#Builds VEF
#Options:
#   -c: compiler (ifort/gfortran; default ifort)
#   -b: build type (debug/release; default release)
#   -t: enable tracing

export FC="ifort"
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
else
    break
fi
done

cd AlTay
rm -rf release debug
cmake -B $BUILD_TYPE/build -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DTRACE=$TRACE
cmake --build ${BUILD_TYPE}/build --parallel --target install

cd ../VEF

rm -rf build bin
cmake -B build -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DTRACE=$TRACE -DCMAKE_INSTALL_PREFIX=.
cmake --build build --parallel --target install

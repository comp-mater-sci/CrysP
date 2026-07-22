#! /usr/bin/env sh
set -e

#Builds VEF
#Options:
#   -c: compiler (ifort/gfortran; default ifort)
#   -b: build type (debug/release; default release)
#   --clean: remove existing build files before building.

export FC="ifx"
BUILD_TYPE="release"

while [ True ]; do
if [ "$1" = "-c" ]; then
    export FC="$2"
    shift 2
elif [ "$1" = "-b" ]; then
    BUILD_TYPE="$2"
    shift 2
elif [ "$1" = "--clean" ]; then
    rm -rf libcrysp/build crysp-cli/build crysp-cli/bin
    shift 1
else
    break
fi
done

cd libcrysp
cmake -B build -DCMAKE_BUILD_TYPE=$BUILD_TYPE
cmake --build build --parallel --target install

cd ../crysp-cli
cmake -B build -DCMAKE_BUILD_TYPE=$BUILD_TYPE -DCMAKE_INSTALL_PREFIX=.
cmake --build build --parallel --target install

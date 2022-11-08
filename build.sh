#! /usr/bin/env sh


export FC="ifort"
BUILD_TYPE="release"

if [ $# -eq 1 ]; then
    BUILD_TYPE="$1"
elif [ $# -eq 2 ]; then
    export FC="$1"
    BUILD_TYPE="$2"
fi

for LIB in FCRI fopt AlTay VEF;do
  cd $LIB
  rm -rf release debug
  mkdir -p $BUILD_TYPE/build
  cd $BUILD_TYPE/build
  cmake ../..  -DCMAKE_BUILD_TYPE=$BUILD_TYPE
  make install
  cd ../../..
done

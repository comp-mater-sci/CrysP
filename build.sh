#! /usr/bin/env sh

BUILD_TYPE=release

for LIB in FCRI fopt AlTay VEF;do
  cd $LIB
  rm -rf release debug
  mkdir -p $BUILD_TYPE/build
  cd $BUILD_TYPE/build
  cmake ../..  -DCMAKE_BUILD_TYPE=release
  make install
  cd ../../..
done

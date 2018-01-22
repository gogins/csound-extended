#!/bin/bash
echo "Building all local  dependencies of csound-extended..."

echo "Building Ableton Link library..."
cd link
mkdir -p build
cd build
echo "cwd: `pwd`"
rm -f CMakeCache.txt
cmake .. -b ../../build-linux -DINSTALL_PREFIX:PATH="../../../../local-install"
make

cd ..

echo "Finished building all local  dependencies of csound-extended."

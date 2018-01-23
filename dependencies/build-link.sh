#!/bin/bash
echo "Building Ableton Link library..."
echo "Please note, Csound requires only headers; these binaries are for testing with LinkHut."
cd link
mkdir -p build
cd build
echo "cwd: `pwd`"
rm -f CMakeCache.txt
cmake .. 
make

cd ..

echo "Finished building all local  dependencies of csound-extended."

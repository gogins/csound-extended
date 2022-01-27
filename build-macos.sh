#!/bin/bash
echo "Building all for macOS..."
mkdir -p build-macos
cd dependencies
# Uncomment the next line, if you need to test the Ableton Link opcodes.
# bash build-link.sh
cd ..
cd build-macos
rm -f CMakeCache.txt
cmake -Wno-dev .. -DCMAKE_PREFIX_PATH=/usr/local:/usr 
make -j6 VERBOSE=1
sudo make install
echo "Finished building all for macOS."

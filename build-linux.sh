#!/bin/bash
echo "Building all for Linux..."
cd dependencies
bash build-link.sh
cd ..
cd build-linux
rm -f CMakeCache.txt
cmake ..
make -j6
# make install
sudo make package
cd ..
echo "Finished building all for Linux."
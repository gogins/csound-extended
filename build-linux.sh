#!/bin/bash
echo "Building all for Linux..."
mkdir -p build-linux
cd dependencies
bash build-link.sh
cd ..
cd build-linux
rm -f CMakeCache.txt
cmake ..
make -j6 VERBOSE=1
echo "Building packages..."
sudo make package
echo "Debian packages and contents..."
find . -name '*.deb' -ls -exec dpkg -f '{}' ';'
# find . -name '*.deb' -ls -exec dpkg -c '{}' ';' 
cd ..
echo "Running lintian..."
lintian --no-tag-display-limit build-linux/csound-extended-*.deb
bash executable-targets-linux.sh
echo "Finished building all for Linux."

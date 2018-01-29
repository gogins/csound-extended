#!/bin/bash
echo "Building all for Linux..."
mkdir -p build-linux
cd dependencies
bash build-link.sh
cd ..
cd build-linux
rm -f CMakeCache.txt
cmake ..
make -j6 
echo "Installing..."
sudo make install
echo "Building packages..."
sudo make package
echo "Debian package contents:"
find . -name '*.deb' -exec dpkg -f '{}' ';' 
find . -name '*.deb' -exec dpkg -c '{}' ';' 
cd ..
echo "Running lintian..."
lintian --no-tag-display-limit build-linux/csound-extended-6.11.0-beta-Linux.deb
echo "Finished building all for Linux."
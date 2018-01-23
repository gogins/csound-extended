#!/bin/bash
echo "Building all for Linux..."
cd dependencies
bash build-link.sh
cd ..
cd build-linux
rm -f CMakeCache.txt
cmake ..
make -j6 
sudo make install
sudo make package
echo "Debian package contents:"
dpkg -c *.deb
cd ..
echo "Finished building all for Linux."
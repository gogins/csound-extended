#!/bin/bash
echo "Building all for Linux..."
mkdir -p build-linux
cd dependencies
# Uncomment the next line, if you need to test the Ableton Link opcodes.
# bash build-link.sh
cd ..
cd build-linux
rm -f CMakeCache.txt
cmake -Wno-dev .. -DCMAKE_PREFIX_PATH=/usr/local:/usr
make -j6 VERBOSE=1
echo "Building packages..."
sudo make package
echo "Debian packages and contents..."
find . -name '*.deb' -ls -exec dpkg -f '{}' ';'
cd ..
echo "Running lintian..."
lintian --no-tag-display-limit --suppress-tags=spelling-error-in-changelog,non-dev-pkg-with-shlib-symlink -i build-linux/csound-extended-*.deb
bash executable-targets-linux.sh
echo "Finished building all for Linux."

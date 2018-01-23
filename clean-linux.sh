#!/bin/bash
cd build-linux
echo "Cleaning all Linux artifacts from `pwd`..."
make clean
rm -r --interactive=1 *.*
cd ..
echo "Finished cleaning all Linux artifacts from `pwd`."

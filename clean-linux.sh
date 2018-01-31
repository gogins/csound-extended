#!/bin/bash
cd build-linux
echo "Cleaning all Linux artifacts from `pwd`..."
make clean
rm -R --interactive=once *.*
cd ..
cd CsoundHtml5
make clean
rm CsoundHtml5
cd ..
echo "Finished cleaning all Linux artifacts from `pwd`."
cd ..

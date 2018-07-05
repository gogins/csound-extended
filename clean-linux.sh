#!/bin/bash
if [ -d "$build-linux" ]; then
    cd build-linux
    echo "Cleaning all Linux artifacts from `pwd`..."
    make clean
    rm -rf --interactive=once *
    cd ..
    cd CsoundHtml5
    make clean
    rm CsoundHtml5
    cd ..
    echo "Finished cleaning all Linux artifacts from `pwd`."
    cd ..
fi

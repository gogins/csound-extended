#!/bin/bash
echo "Building faust_guitar..."
echo "Generating C++ Csound opcode source..."
faust -double -a csound.cpp -uim -o modularInterpInstrMIDI.dsp.cpp modularInterpInstrMIDI.dsp
echo "Compiling C++ Csound opcode..."
clang++ modularInterpInstrMIDI.dsp.cpp -DOPCODE_NAME=modularInterpInstrMIDI -DUSE_DOUBLE -Dlinux --std=gnu++17 -lstdc++fs -Wno-write-strings -Ofast -march=native -g -fPIC -shared -I. -iquote /usr/local/include/csound -I$HOME/faust/examples/physicalModeling/faust-stk -I/usr/local/include -I/usr/local/include/csound -I/usr/include/csound -lm -omodularInterpInstrMIDI.so 
sudo cp modularInterpInstrMIDI.so /usr/local/lib/csound/plugins64-6.0/
echo "Finished building faust_guitar."


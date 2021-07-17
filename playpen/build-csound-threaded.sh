#!/bin/bash
echo Building Csound interface for Python...
swig -debug-classes -verbose -includeall -c++ -python -D__BUILDING_CSOUND_INTERFACES -I/usr/local/include/csound -I/usr/include/csound -I/usr/include/python3.8 csound.i
g++ -std=c++11 -Wno-attributes -Wno-format-security -fpermissive -fPIC -shared csound_wrap.cxx -I/usr/local/include/csound -I/usr/include/csound -I/usr/include/python3.8 -lpython3.8 -lcsound64 -lsndfile -lpthread -o_CsoundThreaded.so
echo Finished building Csound interface for Python.
ls -ll
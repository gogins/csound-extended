#!/bin/bash
echo "Began cleaning all Linux artifacts from `pwd`...."
sudo rm -rfd build-linux
cd CsoundHtml5
make clean
rm CsoundHtml5
cd ..
echo "Finished cleaning all Linux artifacts from `pwd`."

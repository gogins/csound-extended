#!/bin/bash
echo "Began cleaningand rebuilding all Linux artifacts from `pwd`...."
sudo rm -rfd build-linux
sudo rm -rfd csound.node/build/Release
bash build-linux.sh
echo "Finished cleaning and rebuilding all Linux artifacts from `pwd`."

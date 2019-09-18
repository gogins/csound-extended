#!/bin/bash
echo "Began cleaning all Linux artifacts from `pwd`...."
sudo rm -rfd build-linux
sudo rm -rfd csound.node/build/Release
echo "Finished cleaning all Linux artifacts from `pwd`."

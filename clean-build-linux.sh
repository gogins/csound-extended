#!/bin/bash
echo "Began cleaning and rebuilding all Linux artifacts from `pwd`...."
apt remove csound-extended-dev
rm -rfd build-linux
rm -rfd csound.node/build/Release
find . -wholename "*_pycache_*" -delete
bash build-linux.sh
echo "Finished cleaning and rebuilding all Linux artifacts from `pwd`."

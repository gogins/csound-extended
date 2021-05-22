#!/bin/bash
echo "Began cleaning and rebuilding all Linux artifacts from `pwd`...."
sudo apt remove csound-extended-dev
sudo rm -rfd build-linux
sudo find . -wholename "*_pycache_*" -delete
sudo -k
bash build-linux.sh
echo "Finished cleaning and rebuilding all Linux artifacts from `pwd`."

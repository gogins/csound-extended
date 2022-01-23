#!/bin/bash
echo "Began cleaning and rebuilding all macOS artifacts from `pwd`...."
sudo make uninstall
sudo rm -rfd build-macos
sudo find . -wholename "*_pycache_*" -delete
sudo -k
bash build-macos.sh
echo "Finished cleaning and rebuilding all macOS artifacts from `pwd`."

#!/bin/bash
echo "Starting completely fresh build based on Csound, for release package..."
git branch
git pull
bash update-dependencies.sh
cd dependencies
bash update-csound-dependency-packages.sh
cd ..
bash clean-linux.sh
bash build-linux.sh
echo "Finished completely fresh build."

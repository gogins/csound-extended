#!/bin/bash
echo "Starting completely fresh build..."
git branch
git pull
bash update-dependencies.sh
bash clean-build-linux.sh
echo "Finished completely fresh build."
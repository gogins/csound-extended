#!/bin/bash
echo "Starting completely fresh build..."
git branch
git pull
bash update-dependencies.sh
bash clean-linux.sh
bash build-linux.sh
echo "Finished completely fresh build."
#!/bin/bash
echo "Starting completely clean build..."
bash update-depedencies.sh
bash clean-linux.sh
bash build-linux.sh
echo "Finished completely clean build."
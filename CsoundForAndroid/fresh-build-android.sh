#!/bin/bash
echo "Starting completely fresh build..."
bash update-dependencies.sh
bash clean-linux.sh
bash build-linux.sh
echo "Finished completely fresh build."
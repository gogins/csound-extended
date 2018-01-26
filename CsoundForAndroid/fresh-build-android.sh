#!/bin/bash
echo "Starting completely fresh build..."
bash update-dependencies.sh
bash clean-android.sh
bash build-android.sh
echo "Finished completely fresh build."

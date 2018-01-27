#!/bin/bash
echo "Starting completely fresh build of native libraries for Csound for Android..."
bash update-dependencies.sh
bash clean-android.sh
bash build-android.sh
echo "Finished completely fresh build of native libraries for Csound for Android."

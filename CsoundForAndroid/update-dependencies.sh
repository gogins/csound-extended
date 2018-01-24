#!/bin/bash
echo "Updating all source dependencies of Csound for Android..."
cd dependencies
./update-dependency-submodules.sh
./update-dependency-repositories.sh
ls -ll
cd ..
echo "Finished updating all source dependencies of Csound for Android."

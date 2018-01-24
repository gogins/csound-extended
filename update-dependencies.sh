#!/bin/bash
echo "Updating all source dependencies of csound-extended (and Csound)..."
cd dependencies
./update-dependency-downloads.sh
./update-dependency-packages.sh
./update-dependency-submodules.sh
./update-dependency-repositories.sh
./patch-dependency-sources.sh
ls -ll
cd ..
echo "Finished updating all source dependencies of csound-extended (and Csound)."

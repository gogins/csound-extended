#!/bin/bash
echo "Updating all source dependencies of csound-extended (and Csound)..."
cd dependencies
bash update-dependency-downloads.sh
bash update-dependency-packages.sh
# bash update-csound-dependency-packages.sh
bash update-dependency-submodules.sh
bash update-dependency-repositories.sh
bash update-dependency-cdns.sh
bash patch-dependency-sources.sh
ls -ll
cd ..
echo "Finished updating all source dependencies of csound-extended (and Csound)."

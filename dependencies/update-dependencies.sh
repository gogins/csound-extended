#!/bin/bash
echo "Updating all dependencies of csound-extended (and Csound)..."
./update-dependency-downloads.sh
./update-dependency-packages.sh
./update-dependency-submodules.sh
./update-dependency-repositories.sh
ls -ll
echo "Finished updating all dependencies of csound-extended (and Csound)."

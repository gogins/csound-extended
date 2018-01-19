#!/bin/bash
echo "Updating all dependencies of csound-extended (and Csound)..."
./update-dependency-downloads
./update-dependency-packages
./update-dependency-submodules
echo "Finished updating all dependencies of csound-extended (and Csound)."

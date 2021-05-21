#!/bin/bash
echo "Updating all source dependencies of csound-extended..."
cd dependencies
sudo -k bash update-dependency-packages.sh
bash update-dependency-downloads.sh
bash update-dependency-submodules.sh
bash update-dependency-repositories.sh
bash update-dependency-cdns.sh
ls -ll
cd ..
echo "Finished updating all source dependencies of csound-extended."

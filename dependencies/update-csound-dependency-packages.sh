#!/bin/bash
echo "Updating all Debian packages for Csound required for csound-extended..."
echo "Updating csound..."
echo "(Re-installing in case a local build was installed.)"
sudo apt-get install csound --reinstall
sudo apt-get install libcsound64-dev --reinstall
sudo apt-get install libcsnd-dev --reinstall
sudo apt-get install libcsnd6-6.0v5 --reinstall
sudo apt-get install libcsound64-6.0 --reinstall
sudo apt-get install csound-utils --reinstall
sudo apt-get install csound-d-examploc --reinstall
sudo apt-get install csound-data --reinstall
sudo apt-get install csoundqt --reinstall
sudo apt-get install csoundqt-examples --reinstall
echo "Finished updating all Debian packages for Csound required for csound-extended."

#!/bin/bash
echo "Updating all direct downloads required for csound-extended..."
echo
echo "Updating ASIO SDK..."
wget "http://www.steinberg.net/sdk_downloads/asiosdk2.3.zip"
7z x -y "asiosdk2.3.zip"
#echo "Updating GMM..."
#wget "http://download-mirror.savannah.gnu.org/releases/getfem/stable/gmm-5.1.tar.gz"
#tar xvf gmm-5.1.tar.gz
#rm *.zip*
#rm *.gz*
echo "Finished updating all direct downloads required for csound-extended."

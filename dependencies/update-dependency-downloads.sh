#!/bin/bash
echo "Updating all direct downloads required for csound-extended..."
echo
echo "Updating ASIO SDK..."
wget --backups=1 "http://www.steinberg.net/sdk_downloads/asiosdk2.3.zip"
7z x -y "asiosdk2.3.zip"
sudo -k
echo "Finished updating all direct downloads required for csound-extended."

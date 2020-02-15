#!/bin/bash
echo "Executable targets built..."
find build-linux -type f -name *.so* -not -path "*/_CPack_Packages/*" -ls
find -type f -name csound.node -not -path "*/obj.target/*" -not -path "*/_CPack_Packages/*" -ls
find -type f -name *.deb -not -path "*/_CPack_Packages/*" -ls

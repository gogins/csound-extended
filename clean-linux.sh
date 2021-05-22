#!/bin/bash
echo "Began cleaning all Linux artifacts from `pwd`...."
rm -rfd build-linux
rm -rfd csound.node/build/Release
echo "Finished cleaning all Linux artifacts from `pwd`."

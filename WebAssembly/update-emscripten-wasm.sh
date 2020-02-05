clear
echo "Updating the Emscripten toolchain..."
export WEBASSEMBLY_HOME=`pwd`
echo "WEBASSEMBLY_HOME: $WEBASSEMBLY_HOME"
cd ~/emsdk
echo "Updating Emscripten and LLVM..."
git pull
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh
export EMSCRIPTEN_ROOT=$EMSCRIPTEN
cd $WEBASSEMBLY_HOME
echo "Updated the Emscripten toolchain."
echo
clear
echo "Building Csound for WebAssembly..."
echo "Updating the Emscripten toolchain..."
export WEBASSEMBLY_HOME=`pwd`
echo "WEBASSEMBLY_HOME: $WEBASSEMBLY_HOME"
cd ~/emsdk
./emsdk install sdk-incoming-64bit binaryen-master-64bit -j1
./emsdk activate sdk-incoming-64bit binaryen-master-64bit
source ./emsdk_env.sh
export EMSCRIPTEN_ROOT=$EMSCRIPTEN
cd $WEBASSEMBLY_HOME
echo "Deleting previous build..."
rm -rf build-wasm
echo "Building libsndfile for WebAssembly..."
bash download_and_build_libsndfile_wasm.sh
echo "Building the Csound library for WebAssembly..."
bash build-wasm.sh

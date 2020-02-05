clear
echo "Building Csound for WebAssembly..."
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
echo "Deleting previous build..."
rm -rf deps
rm -rf build-wasm
emcc --clear-cache
bash download_and_build_libsndfile_wasm.sh
bash build-wasm.sh

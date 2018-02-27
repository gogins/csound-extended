clear
echo "Building Csound for WebAssembly..."
echo "Updating the Emscripten toolchain..."
pushd `pwd`
cd ~/emsdk
./emsdk install sdk-incoming-64bit binaryen-master-64bit -j1
./emsdk activate sdk-incoming-64bit binaryen-master-64bit
source ./emsdk_env.sh
export EMSCRIPTEN_ROOT=$EMSCRIPTEN
popd
echo "Deleting previous build..."
rm -rf build-wasm
echo "Building libsndfile for WebAssembly..."
sh download_and_build_libsndfile_wasm.sh
echo "Building the Csound library for WebAssembly..."
sh build-wasm.sh

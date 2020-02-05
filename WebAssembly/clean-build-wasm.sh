clear
echo "Clean build of Csound for WebAssembly..."
export WEBASSEMBLY_HOME=`pwd`
echo "WEBASSEMBLY_HOME: $WEBASSEMBLY_HOME"
cd ~/emsdk
source ./emsdk_env.sh
export EMSCRIPTEN_ROOT=$EMSCRIPTEN
cd $WEBASSEMBLY_HOME
echo "Deleting previous build..."
rm -rf deps
rm -rf build-wasm
emcc --clear-cache
bash download_and_build_libsndfile_wasm.sh
bash build-wasm.sh
echo "Completed fresh build of Csound for WebAssembly."
echo

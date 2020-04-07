clear
echo "Clean build of Csound for WebAssembly..."
export WEBASSEMBLY_HOME=`pwd`
echo "WEBASSEMBLY_HOME: $WEBASSEMBLY_HOME"
~/emsdk/emsdk activate latest
source ~/emsdk/emsdk_env.sh
echo "Using EMSCRIPTEN_ROOT: $EMSCRIPTEN_ROOT."
echo "Deleting previous build..."
rm -rf deps
rm -rf build-wasm
emcc --clear-cache
bash download_and_build_libsndfile_wasm.sh
bash build-wasm.sh
echo "Completed fresh build of Csound for WebAssembly."
echo

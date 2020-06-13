#!/bin/bash
echo "Building csound-extended for WebAssembly..."

~/emsdk/emsdk activate latest
source ~/emsdk/emsdk_env.sh

echo "Using EMSCRIPTEN_ROOT: $EMSCRIPTEN_ROOT."

export CFLAGS="-Wno-implicit-int-float-conversion" 

mkdir -p build-wasm
cd build-wasm
rm -f CMakeCache.txt

emcmake cmake -G "Unix Makefiles" -Wno-dev ..
emmake make cmask csound-static csoundac-static -j6

echo "Packaging some resources..."

python $EMSCRIPTEN_ROOT/tools/file_packager.py csound_samples.data --preload ../../dependencies/csound/samples --js-output=csound_samples.js

echo "Compiling csound_embind..."

em++ -std=c++17 -s SAFE_HEAP=0 -s LINKABLE=1 -s ASSERTIONS=1 -DINIT_STATIC_MODULES=1 -s FORCE_FILESYSTEM=1 -iquote ../src -I../../dependencies/csound/include -I../../dependencies/csound/H -I../../dependencies/csound/interfaces -I../deps/libsndfile-1.0.25/src -c ../src/csound_embind.cpp -Iinclude -o csound_web_audio.bc

# Total memory for a WebAssembly module must be a multiple of 64 KB so...
# 1024 * 64 = 65536 is 64 KB
# 65536 * 1024 * 4 is 268435456

echo "Compiling CsoundAudioProcessor..."

em++ -s SAFE_HEAP=0 -v -O1 -std=c++17 --source-map-base . --bind --embed-file ../../dependencies/csound/samples@/ --pre-js ../src/CsoundAudioProcessor_prejs.js --post-js ../src/CsoundAudioProcessor_postjs.js -DINIT_STATIC_MODULES=1 -s FORCE_FILESYSTEM=1 -s WASM=1 -s ASSERTIONS=0 -s LINKABLE=1 -s RESERVED_FUNCTION_POINTERS=1 -s TOTAL_MEMORY=268435456 -s ALLOW_MEMORY_GROWTH=1 -s WASM_ASYNC_COMPILATION=0 -s NO_EXIT_RUNTIME=0 -s SINGLE_FILE=1 -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s FORCE_FILESYSTEM=1 csound_web_audio.bc csound/libcsound.a ../deps/lib/libsndfile.a ../deps/lib/libogg.a ../deps/lib/libvorbis.a ../deps/lib/libvorbisenc.a ../deps/lib/libFLAC.a -o CsoundAudioProcessor.js

echo "Compiling CsoundAC..."

em++ -s MODULARIZE -s EXPORT_NAME="createCsoundAC" -s EXPORT_ES6=0 -s USE_ES6_IMPORT_META=0 -s SAFE_HEAP=0 -v -O1 -std=c++11 --source-map-base . --bind ../CsoundAC/csoundac_embind.cpp -s WASM=1 -s ASSERTIONS=0 -s LINKABLE=1 -s RESERVED_FUNCTION_POINTERS=1 -s TOTAL_MEMORY=268435456 -s ALLOW_MEMORY_GROWTH=1 -s WASM_ASYNC_COMPILATION=1 -s NO_EXIT_RUNTIME=0 -s SINGLE_FILE=1 -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s FORCE_FILESYSTEM=1 -I ../deps/libsndfile-1.0.25/src -I../.. CsoundAC/libcsoundac-static.a ../deps/lib/libsndfile.a ../deps/lib/libogg.a ../deps/lib/libvorbis.a ../deps/lib/libvorbisenc.a ../deps/lib/libFLAC.a -o CsoundAC.js

cd ..
bash release-wasm.sh

echo "Finished building csound-extended for WebAssembly."

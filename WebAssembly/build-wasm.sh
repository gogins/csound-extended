 #!/bin/bash
echo "Building csound-extended for WebAssembly..."

export EMSCRIPTEN_ALLOW_NEWER_PYTHON=1

~/emsdk/emsdk activate latest
source ~/emsdk/emsdk_env.sh

echo "Using EMSCRIPTEN_ROOT: $EMSCRIPTEN_ROOT."
echo "Python version (must be 3 or higher):"
python --version

# Total memory for a WebAssembly module must be a multiple of 64 KB so...
# 1024 * 64 = 65536 is 64 KB
# 65536 * 1024 * 4 is 268435456

export CXX_FLAGS="-v -std=c++17 -Wno-implicit-int-float-conversion -DINIT_STATIC_MODULES=1" 

# Most emcc flags should be the same for both the 'compile' and the 'compile and link' passes.

export EMCC_FLAGS='-s ALLOW_MEMORY_GROWTH=1 -s ASSERTIONS=1 -s DEMANGLE_SUPPORT=1 -s FORCE_FILESYSTEM=1 -s INITIAL_MEMORY=268435456 -s LINKABLE=1 -s NO_EXIT_RUNTIME=0 -s SAFE_HEAP=0 -s USE_BOOST_HEADERS=1 -s WASM=1'

mkdir -p build-wasm
cd build-wasm
rm -f CMakeCache.txt

emcmake cmake -G "Unix Makefiles" -Wno-dev ..
emmake make cmask csound-static csoundac-static -j6

echo "Packaging some resources..."

python $EMSCRIPTEN_ROOT/tools/file_packager.py csound_samples.data --preload ../../dependencies/csound/samples --js-output=csound_samples.js

echo "Compiling csound_embind..."

em++ ${CXX_FLAGS} ${EMCC_FLAGS} -iquote ../src -I../../dependencies/csound/include -I../../dependencies/csound/H -I../../dependencies/csound/interfaces -I../deps/libsndfile-1.0.25/src -c ../src/csound_embind.cpp --bind -Iinclude -o csound_web_audio.bc

echo "Compiling CsoundAudioProcessor..."

em++ ${CXX_FLAGS} -O1 ${EMCC_FLAGS} --bind -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s RESERVED_FUNCTION_POINTERS=2 -s SINGLE_FILE=1 -s WASM_ASYNC_COMPILATION=0 --source-map-base . --embed-file ../../dependencies/csound/samples@/ --pre-js ../src/CsoundAudioProcessor_prejs.js --post-js ../src/CsoundAudioProcessor_postjs.js csound_web_audio.bc csound/libcsound.a ../deps/lib/libsndfile.a ../deps/lib/libogg.a ../deps/lib/libvorbis.a ../deps/lib/libvorbisenc.a ../deps/lib/libFLAC.a -o CsoundAudioProcessor.js

echo "Compiling CsoundAC..."

em++ ${CXX_FLAGS} ${EMCC_FLAGS} --bind -s EXPORT_ES6=0 -s EXPORT_NAME="createCsoundAC" -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]' -s MODULARIZE=1 -s RESERVED_FUNCTION_POINTERS=1 -s SINGLE_FILE=1 -s USE_ES6_IMPORT_META=0 -s WASM_ASYNC_COMPILATION=1 --source-map-base . ../CsoundAC/csoundac_embind.cpp -I ../deps/libsndfile-1.0.25/src -I../.. CsoundAC/libcsoundac-static.a ../deps/lib/libsndfile.a ../deps/lib/libogg.a ../deps/lib/libvorbis.a ../deps/lib/libvorbisenc.a ../deps/lib/libFLAC.a -o CsoundAC.js

cd ..
bash release-wasm.sh

echo "Finished building csound-extended for WebAssembly."

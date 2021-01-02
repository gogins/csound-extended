#!/bin/sh
echo "Creating a release for Csound for WebAssembly..."
export CSOUND_AUDIO_NODE_VERSION=1.1.0
export RELEASE_DIR=csound-extended-wasm-${CSOUND_AUDIO_NODE_VERSION}
#remove backup files ending with ~
find . -name "*~" -exec rm {} \;
rm -rf dist-wasm
mkdir dist-wasm
cp src/*.js dist-wasm/
cp build-wasm/cmask/*.js dist-wasm/
cp build-wasm/cmask/*.wasm dist-wasm/
cp build-wasm/CsoundAudio*.* dist-wasm/
cp build-wasm/CsoundAC.js dist-wasm/
cp build-wasm/csound_samples.* dist-wasm/
rm -rf $RELEASE_DIR
mkdir $RELEASE_DIR
cp -r dist-wasm/* examples
cp -f CsoundAC/piano-roll.js dist-wasm
cp -f CsoundAC/piano-roll.js examples/
#~ cp -r examples/* ${RELEASE_DIR}/
#~ cp -r examples/* ../docs
cp -f src/csound_loader.js ../silencio/js/
cp -f CsoundAC/piano-roll.js ../silencio/js/
cp -f examples/CsoundAC.js ../silencio/js/
cp -r dist-wasm/* ../docs/html/
cp -rf ../silencio/js/* examples/js/
cp -r examples/* ${RELEASE_DIR}/
cp -r examples/* ../docs
zip -r ${RELEASE_DIR}.zip ${RELEASE_DIR}
echo "Files in examples:"
ls -ll examples
echo "Files in ../docs:"
ls -ll ../docs
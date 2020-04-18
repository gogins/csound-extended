#!/bin/sh
echo "Creating a release for Csound for WebAssembly..."

export CSOUND_AUDIO_NODE_VERSION=1.1.0
export RELEASE_DIR=csound-extended-wasm-${CSOUND_AUDIO_NODE_VERSION}

#remove backup files ending with ~
find . -name "*~" -exec rm {} \;
rm -rf dist-wasm
mkdir dist-wasm
cp src/*.js dist-wasm/
cp build-wasm/CsoundAudio*.* dist-wasm/
cp build-wasm/csound_samples.* dist-wasm/
rm -rf $RELEASE_DIR
mkdir $RELEASE_DIR
cp -R cmask/*.js dist-wasm
cp -R cmask/*.wasm dist-wasm
cp -R dist-wasm/* examples
cp -R examples/* ${RELEASE_DIR}/
cp -R examples/* ../docs/
cp -f src/csound_loader.js ../silencio/js/
cp -R dist-wasm/* ../docs/html/

zip -r ${RELEASE_DIR}.zip ${RELEASE_DIR}
ls -ll examples/
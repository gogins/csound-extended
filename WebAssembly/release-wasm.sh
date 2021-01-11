#!/bin/sh
echo "Creating a release for Csound for WebAssembly..."
export CSOUND_AUDIO_NODE_VERSION=1.2.0
export RELEASE_DIR=csound-extended-wasm-${CSOUND_AUDIO_NODE_VERSION}
rm -rf $RELEASE_DIR
mkdir $RELEASE_DIR
#remove backup files ending with ~
find . -name "*~" -exec rm {} \;
cp src/*.js $RELEASE_DIR/
cp build-wasm/cmask/*.js $RELEASE_DIR/
cp build-wasm/cmask/*.wasm $RELEASE_DIR/
cp build-wasm/CsoundAudio*.* $RELEASE_DIR/
cp build-wasm/CsoundAC.js $RELEASE_DIR/
cp build-wasm/csound_samples.* $RELEASE_DIR/
cp -f CsoundAC/piano-roll.js $RELEASE_DIR/
cp -f src/csound_loader.js ../silencio/js/
cp -f CsoundAC/piano-roll.js ../silencio/js/
zip -r ${RELEASE_DIR}.zip ${RELEASE_DIR}

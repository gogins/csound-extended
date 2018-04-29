#!/bin/sh
echo "Creating a release for Csound for WebAssembly..."
export CS_VERSION="0.1.2-beta"
export RELEASE_DIR=csound-extended-wasm-${CS_VERSION}

#remove backup files ending with ~
find . -name "*~" -exec rm {} \;
rm -rf dist-wasm
mkdir dist-wasm
cp build-wasm/libcsound.js dist-wasm/
cp src/*.js dist-wasm/
cp build-wasm/csound_extended.* dist-wasm/
cp build-wasm/CsoundAudio*.* dist-wasm/
rm -rf $RELEASE_DIR
mkdir $RELEASE_DIR
cp -R cmask/*.js dist-wasm
cp -R cmask/*.wasm dist-wasm
cp -R dist-wasm/* examples
cp -R examples/* ${RELEASE_DIR}/
cp -R examples/* ../docs/

zip -r ${RELEASE_DIR}.zip ${RELEASE_DIR}
ls -ll examples/
#!/bin/sh
export CS_VERSION="6.11.1"
export RELEASE_DIR=csound-extended-wasm-${CS_VERSION}

#remove backup files ending with ~
find . -name "*~" -exec rm {} \;

rm -rf $RELEASE_DIR
mkdir $RELEASE_DIR
cp -R cmask/*.js dist-wasm
cp -R cmask/*.wasm dist-wasm
cp -R dist-wasm/* examples
cp -R examples/* ${RELEASE_DIR}/
cp -R examples/* ../docs/

zip -r ${RELEASE_DIR}.zip ${RELEASE_DIR}

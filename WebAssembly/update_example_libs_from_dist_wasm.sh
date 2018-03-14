#!/bin/sh
mkdir examples-wasm
cp -r examples-src/* examples-wasm/
cp dist-wasm/* examples-wasm/javascripts
mv examples-wasm/javascripts/csound_extended.wasm examples-wasm


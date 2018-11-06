# csound-extended Examples for Testing

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

This directory contains a number of functional examples that can be used to 
test that the csound-extended-dev package (and some of the other things that I 
maintain) are basically working after installation. 

These examples also demonstrate some, but by no means all, of the features and 
capabilities of csound-extended-dev, csound-extended-wasm, csound-aeolus 
(available from https://github.com/gogins/csound-aeolus), and csound-vst 
(available from https://michaelgogins.tumblr.com/csound_extended). 

As such, these examples can serve as templates for new compositions.

## Installation

Examples that use csound-aeolus require the example to be edited to find 
the Aeolus stops directory,

Some of the examples use the Pianoteq VST plugin, and thus require both 
vst4cs and the Pianoteq plugin to first be installed on the system, and the 
example to be edited to find the Pianoteq.

Examples that use csound.node require NW.js to first be installed on the 
system, and the creation of a package.json manifest file in the example 
directory. The NODE_PATH environment variable must also include the path where 
csound.node has been installed. In addition, in the test-examples/csound.node 
directory, you must create a symbolic link to the silencio directory.

## Testing

The very basic test plan that I use is:





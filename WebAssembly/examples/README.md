# csound-extended Examples for Testing

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

This directory contains a number of functional examples that can be used to 
test that the csound-extended-dev package (and also some of the other things 
that I maintain) are basically working after installation. 

These examples also demonstrate some, but by no means all, of the features and 
capabilities of csound-extended-dev, csound-extended-wasm, csound-aeolus 
(available from https://github.com/gogins/csound-aeolus), and csound-vst 
(available from https://michaelgogins.tumblr.com/csound_extended). 

As such, these examples can serve as templates for new compositions.

## Installation

A number of these pieces require configuration or editing before they will 
run.

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

### Pre-Requisites

Rebuild and reinstall:

- csound-extended-dev 

- csound-extended-wasm 

- The Csound for Android app

- csound-aeolus 

- csound-vst 

- OM2Csound
    
### Tests

Change to the `test-examples` directory and perform the following tests:

- csound: First, run the .csd file with the csound command to ensure that 
  Csound itself has been properly installed. 

- cmask: Run the .csd files with the csound command. 

- csound-ac: Compile the Parachronic-Piano.cpp file using the build script in 
  the comment at the top of the file, and execute 
  `./Parachronic-Piano --csound --audio PortAudio --device dac`. 
  Run the .py file and the .lua file. 
  
- csound-aeolus: Run the aeolus program to create some stops, and then 
  execute `sbcl --script Triphase-Aeolus.lisp`.

- csound-for-android: Run all the Gogins examples from the app.

- csound-html5: In your Web browser, go to 
  `https://gogins.github.io/csound-extended/message.html`, and click on the 
  _Play_ button.

- csound-link: Not tested at this time.

- csound.node: In the `test-examples` directory, execute `nwjs csound.node` 
  and select the _Play_ menu item.

- csound-vst: Run Reaper, and load and play the `csound-vst.RPP` file. 

- nudruz: Use Steel Bank Common Lisp to execute the .lisp files. 

- For the OpenMusic Csound interface, since it's a drop-in replacement for the 
  original OM2Csound, just run an OM2Csound synthesis example. 




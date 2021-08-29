# Csound Patches

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

## Introduction

This directory contains Csound patches written in such a way as to be 
completely modular. I often use these patches to compose my own pieces. Most 
of these patches do not depend on any external resources or code. A few 
patches depend on SoundFonts, VST plugins, or sound samples.

The approach here is generally to find existing instrument definitions that 
sound good to me and are free for me to use, and then to adapt them to my 
module system. At times, I extensively modify the instrument definitions. 
Occasionally, I write the instrument definitions from scratch.

There is some redundancy in the form of multiple implementations of similar 
instruments. For this I make no excuse. Use the one that best suits.

There are two pianos. One uses the excellent physically modeled piano by 
Modartt (as a VST plugin), the other uses a large SoundFont sampled from a 
Steinway Model C (with the Fluidsynth opcodes).

An (incomplete but still useful) exposition of the methodology behind these
modular patches may be found in<a href='modular_csound.pdf'>
<b><i>A Module System for Csound</b></i></a>.

Some of the patches are instruments, and some are effects. Effects are
usually scheduled using the `alwayson` opcode and connected in processing
chains using the `connect` opcode. See the Csound Reference Manual example
for the signal flow graph opcodes to see how this is done.

You can `#include` any of these patches in a Csound orchestra and expect it to 
render audio from any MIDI or score events. All audio output levels are
normalized so that MIDI velocity 80 produces about -6 dBFS. Each instrument
patch uses a releasing envelope. The patches are optimized for audio quality.
I have tried to reflect pitches, amplitudes, or durations that would not render 
well back into a safe domain before rendering.

All audio outputs and inputs are performed using the signal flow graph
opcodes. In the orchestra header, `#define USE_SPATIALIZATION` to output audio 
to a 16-channel Ambisonic B-format signal outlet named `abformat`, along with 
a mono `out` outlet that can be used as a reverb send; otherwise, instruments 
output plain stereo to outlets `outleft` and `outright`.

Control parameters are set by global variables using the naming convention
`gk_InstrumentName_control_variable_name`. Default values are set for all
control parameters. All control variables are automatically mapped to a Csound 
control channel of the same name, using`chnexport`. Each instrument
has a `gk_InstrumentName_level` control parameter calibrated in dB. Sometimes, 
the parameters to opcodes are the Csound Reference Manual parameter names for 
that opcode, prefixed by the instrument name.

A Python script, `test_instrument.py`, is provided to help with the writing
of new patches. It generates a range of notes for a patch, and prints output
levels that can be used to derive an audio level normalization factor.

The <a href='Spatialize.inc'>Spatialize.inc</a>
file implements a complete system for spatializing audio based on the
excellent work of <a href='http://www.sonicarchitecture.de/'>Jan Jacob 
Hofmann</a>. For a working example of the spatialization system, see 
<a href='SpatializedDrone.inc'>`SpatializedDrone`</a>

## Changes

See https://github.com/gogins/csound-extended/commits/develop for the commit 
log.

## License

These patches are licensed under the terms of the GNU Lesser General Public 
License, v2.2.

## Acknowledgments

I would like to thank the many authors of the Csound instrument definitions 
that I have borrowed both for writing them, and for making them available for 
re-use. I have tried to credit each original author in each patch file. 

The existence of such great code in such profusion is one of the outstanding 
strengths of the Csound ecosystem.

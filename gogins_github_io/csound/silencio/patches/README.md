# Csound Patches

## Copyright (C) 2017 by Michael Gogins
## These patches are licensed under the terms of the GNU Lesser General Public License version 2.

This directory contains Csound patches written in such a way as to be
completely modular. I use these patches to compose my own pieces. Most of
these patches do not depend on any external resources or code. A few patches
depend on SoundFonts or VST plugins.

There are two pianos. In the orchestra header, `#define USE_PIANOTEQ` to use the 
excellent commercial physically modeled piano VST plugin; otherwise, a large 
sampled Steinway SoundFont is used.

An (incomplete but still useful) exposition of the methodology behind these
modular patches may be found in<a href='modular_csound.pdf'><b><i>A Module System for Csound</b></i></a>.

Some of the patches are instruments, and some are effects. Effects are
usually scheduled using the `alwayson` opcode and connected in processing
chains using the `connect` opcode. See the Csound Reference Manual example
for the signal flow graph opcodes to see how this is done.

You can `#include` any of these patches in a Csound orchestra and expect it to
render audio from any MIDI or score events. All audio output levels are
normalized so that MIDI velocity 80 produces about -6 dBFS. Each instrument
patch uses a releasing envelope. The patches are optimized for audio quality.

All audio outputs and inputs are performed using the signal flow graph
opcodes. In the orchestra header, `#define USE_SPATIALIZATION` to output audio 
to a 16-channel Ambisonic B-format signal outlet named `abformat`, along with a 
mono `out` outlet that can be used as a reverb send; otherwise, instruments output 
plain stereo to outlets `outleft` and `outright`.

Control parameters are set by global variables using the naming convention
`gk_InstrumentName_control_variable_name`. Default values are set for all
control parameters, but in use they most likely would be set by k-rate
control channels in an "always on" Controller instrument. Each instrument
has a `gk_InstrumentName_level` control parameter calibrated in dB.

A Python script, `patch_calibrator.py`, is provided to help with the writing
of new patches. It generates a range of notes for a patch, and prints output
levels that can be used to derive an audio level normalization factor.

The <a href='Spatialize.inc'>Spatialize.inc</a>
file implements a complete system for spatializing audio based on the
excellent work of <a href='xxx'>Jan Jacob Hofmann</a>. For a working example 
of the spatialization system, see <a href='SpatializedDrone.inc'>`SpatializedDrone`</a>


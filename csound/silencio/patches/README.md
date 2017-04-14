# Csound Patches

## Copyright (C) 2017 by Michael Gogins
## These patches are licensed under the terms of the GNU Lesser General Public License version 2.

This directory contains Csound patches written in such a way as to be
completely modular. I use these patches to compose my own pieces. None of
these patches depends on any external resources or code.

Some of the patches are instruments, and some are effects. Effects are
usually scheduled using the `alwayson` opcode and connected in processing
chains using the `connect` opcode. See the Csound Reference Manual example
for the signal flow graph opcodes to see how this is done.

You can `#include` any of these patches in a Csound orchestra and expect it to
render audio from any MIDI or score events. All audio output levels are
normalized so thst MIDI velocity 80 produces -6 dBFS. Each instrument
patch uses a releasing envelope. The patches are optimized for audio quality.

All audio outputs and inputs are performed using the signal flow graph
opcodes, which are stereo pairs with standard names "inleft", "inright",
"outleft", and "outright".

Control parameters are set by global variables using the naming convention
"gk_InstrumentName_ControlVariableName". Default values are set for all
control parameters, but in use they most likely would be set by k-rate
control channels in an "always on" Controller instrument. Each instrument
has a "gk_InstrumentName_MasterLevel" control parameter calibrated in dB.

A Python script, "patch_calibrator.py", is provided to help with the writing
of new patches. It generates a range of notes for a patch, and prints output
levels that can be used to derive an audio level normalization factor.

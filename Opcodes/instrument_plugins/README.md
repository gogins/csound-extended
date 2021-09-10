# PLUGIN OPCODES AS INSTRUMENTS

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This directory defines a C++ base class that facilitates creating Csound 
instruments as opcodes written in C++.

It is not possible (yet?) to directly hook in to the instrument template list, 
but this facility gets as close to that as the current Csound internals 
permit. The protocol is:

1. Your instrument opcode must output all audio in `a_output[]`.
2. Your instrument opcode must read any required pfield data from these member 
functions, which directly read the `opds->insds` pfields. And your Csound score 
must define these pfields in the same order and using the same units:

   1. `p1_instrument()` - Instrument number, may have a fractional part.
   2. `p2_time()` - Onset time in beats, usually seconds.
   2. `p3_duration()` - Duration in beats, usually seconds.
   2. `p4_midi_key()` - Pitch as MIDI key, middle C = 60, may have a fractional part.
   2. `p5_midi_velocity()` - Loudness as MIDI velocity, mezzo-forte = 80, may have a fractional part.
   2. `p6_space_front_to_back()` - Spatial location in Ambisonic coordinates.
   2. `p7_space_left_to_right()` - Spatial location in Ambisonic coordinates, same 
        as stereo pan.
   2.  `p8_space_bottom_to_top()` - Spatial location in Ambisonic coordinates.
   2.  `p9_phase()` - Audio phase in radians, may be used e.g. for 
        phase-sychronous overlapped granular synthesis.
3.  Your instrument may send and receive Csound channel messages at any rate 
    and of any type through global variables. These should follow the naming 
    convention `gT_InstrumentName_channel_name`. If a variable of this type 
    already exists at run time, it is used. If it does not already exist, it 
    is created. To facilitate this, the InstrumentOpcodeBase class provides:
    ```   
    MYFLT CsoundInstrumentBase::receiveK(const char *name);
    CsoundInstrumentBase::sendK(const char *name, MYFLT value) const;
    CsoundInstrumentBase::receiveS(const char *name, char *buffer);
    CsoundInstrumentBase::sendS(const char *name, const char *value) const;
    MYFLT *CsoundInstrumentBase::receiveA(const char *name);
    CsoundInstrumentBase::sendA(const char *name, MYFLT *value) const;
    PVSDAT *CsoundInstrumentBase::receivePVS(const char *name);
    CsoundInstrumentBase::sendPVS(const char *name, PVSDAT *value) const;
    ```   
To create a Csound instrument in C++:

1.  Derive a new plugin opcode class from `InstrumentPluginBase`. 

2.  Your instrument must initialize itself and perform all i-rate calculations 
    in its `init` member function.

3.  Your instrument must perform all control-rate and audio-rate calculations 
    in its the 'kontrol` member function.

2.  Your instrument must output any audio to the builtin a-rate array variable 
    `a_output`. Your instrument must assume that `a_output` has as many samples 
    as Csound's ksmps, and as many channels as Csound's nchnls.
    
Once you have compiled your plugin instrument and installed it in your 
${OPCODE6DIR64} directory, using it can be as simple as this:
```
instr MyKlanger
a_out[] init nchnls
a_out my_klanger
outletv "output", a_out
endin
```

# Plugin Opcodes as Instruments

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This directory defines a C++ base class that facilitates creating complete Csound 
instruments as opcodes written in C++, for example using some efficient and 
user-friendly digital signal processing library such as the [STK](https://ccrma.stanford.edu/software/stk/).

It is not possible (yet?) to directly hook in to the instrument template list, 
but this facility gets as close to that as the current Csound internals 
permit. The protocol is:

1. Your instrument opcode must output all audio in `a_output[]`. Its shape is 
   `a_output[nchnls][ksmps]`.
3. Your instrument opcode must read any required pfield data from these member 
   functions, which directly read the `opds->insds` pfields. And your Csound score 
   must define these pfields in the same order and using the same units:

   1.  `pfield(1)` - Instrument number, may have a fractional part.
   2.  `pfield(2)` - Onset time in beats, usually seconds.
   2.  `pfield(3)` - Duration in beats, usually seconds.
   2.  `pfield(4)` - Pitch as MIDI key, middle C = 60, may have a fractional part.
   2.  `pfield(5)` - Loudness as MIDI velocity, mezzo-forte = 80, may have a fractional part.
   2.  `pfield(6)` - Spatial location in Ambisonic coordinates.
   2.  `pfield(7)` - Spatial location in Ambisonic coordinates, same 
       as stereo pan.
   2.  `pfield(8)` - Spatial location in Ambisonic coordinates.
   2.  `pfield(9)` - Audio phase in radians, may be used e.g. for 
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
2.  Your instrument must initialize itself and perform all i-rate computation  
    in an overridden `int CsoundInstrumentBase::init(CSOUND *)` member function.
3.  Your must perform all k-rate and a-rate computation in its overridden 
    `int CsoundInstrumentBase::kontrol(CSOUND *)` member function.
5.  Your instrument must output any audio to the builtin a-rate array variable 
    `a_output`. Your instrument must assume that `a_output` has as many samples 
    as Csound's ksmps, and as many channels as Csound's nchnls.
5.  Your instrument module must register all plugins that it defines using 
    `csoundOpcodeAppend` in an exported `csoundModuleInit` function.
7.  If your instrument module uses any global variables, they must be allocated 
    in the `csoundModuleInit` function.
8.  If your instrument module uses any global variables, they must be deallocated 
    in a `csoundModuleDestroy` function.
    
Once you have compiled your plugin instrument and installed it in your 
`${OPCODE6DIR64}` directory, using it can be as simple as this:
```
instr MyKlanger
a_out[] init nchnls
a_out my_klanger
outletv "output", a_out
endin
```

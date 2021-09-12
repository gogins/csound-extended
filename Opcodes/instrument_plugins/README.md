# Plugin Opcodes as Instruments

Michael Gogins<br>
https://github.com/gogins<br>
http://michaelgogins.tumblr.com

This directory defines a C++ base class that facilitates creating complete Csound 
instruments as opcodes written in C++, for example using some efficient and 
user-friendly digital signal processing library such as the [STK](https://ccrma.stanford.edu/software/stk/),
[AuLib](https://github.com/AuLib/AuLib), or [Gamma](https://github.com/LancePutnam/Gamma).

## Protocol

1. Your instrument opcode must output all audio in the `a_output[]` member variable. 
   Its type and shape is `MYFLT a_output[nchnls][ksmps]`.
3. Your instrument opcode must read any required pfield data from the `p` 
   (for "pfield") member function, which directly read the `opds->insds` pfields. And your Csound score 
   should perhaps define these pfields in the following order and units:

   1.  `p(1)` - Instrument number, may have a fractional part.
   2.  `p(2)` - Onset time in beats, usually seconds.
   2.  `p(3)` - Duration in beats, usually seconds.
   2.  `p(4)` - Pitch as MIDI key, middle C = 60, may have a fractional part.
   2.  `p(5)` - Loudness as MIDI velocity, mezzo-forte = 80, may have a fractional part.
   2.  `p(6)` - Spatial _x_ coordinate from behind the listener to in front of the listener, 
       (Ambisonic coordinate).
   4.  `p(7)` - Spatial _y_ coordinate from the left of the listener to the right of the 
       listener (Ambisonic coordinate), same as stereo pan.
   2.  `p(8)` - Spatial _z_ coordinate from below the listener to above the listener 
       (Ambisonic coordinate).
   4.  `p(9)` - Audio phase in radians, may be used e.g. for 
       phase-sychronous overlapped granular synthesis.
        
3.  Your instrument may send and receive Csound channel messages at any rate 
    and of any type through global variables. These should follow the naming 
    convention `gT_InstrumentName_channel_name`, where `T` is a Csound type code such as 
    `i`, `k`, `S`, or `f`. If a variable of this type already exists at run time, 
    it is used. If it does not already exist, it is created as a global variable and 
    exported as a control channel. To facilitate this, the InstrumentPluginBase class 
    provides the following member functions:
    ```   
    MYFLT InstrumentPluginBase::receiveK(CSOUND *csound, const char *name);
    void InstrumentPluginBase::sendK(CSOUND *csound, const char *name, MYFLT k_value) const;
    int InstrumentPluginBase::receiveS(CSOUND *csound, const char *name, char *buffer);
    void InstrumentPluginBase::sendS(CSOUND *csound, const char *name, const char *value) const;
    void InstrumentPluginBase::receiveA(CSOUND *csound, const char *name, MYFLT *a_value);
    void InstrumentPluginBase::sendA(CSOUND *csound, const char *name, MYFLT *a_value) const;
    int InstrumentPluginBase::receivePVS(CSOUND *csound, const char *name, PVSDATEXT *value);
    int InstrumentPluginBase::sendPVS(CSOUND *csound, const char *name, PVSDATEXT *value) const;
    ```   
Audio channel values here have the shape `a_value[ksmps]`.

## Creation

To create a Csound instrument in C++:

1.  Derive a new plugin opcode class from `InstrumentPluginBase`. 
2.  Your instrument must initialize itself and perform all i-rate computation 
    in an overridden `int CsoundInstrumentBase::init(CSOUND *csound)` member function.
3.  Your instrument must perform all k-rate and a-rate computation in an overridden 
    `int CsoundInstrumentBase::kontrol(CSOUND *csound)` member function.
5.  Your instrument must output any audio to the builtin a-rate array variable 
    `a_output`. Your instrument must assume that `a_output` has as many samples 
    as Csound's ksmps, and as many channels as Csound's nchnls.
5.  Your instrument module must register all plugins that it defines using 
    `csoundOpcodeAppend` in the module's exported `int csoundModuleInit(CSOUND *csound)` 
    function, in the form 
    ```
    csound->AppendOpcode(CSOUND *csound, const char *opcode_name,
        size_t opcode_size, 
        int flags = 0,
        int thread = 3, 
        const char *outypes, 
        const char *intypes,
        int (iopadr*)(CSOUND *, void *),
        int (kopadr*)(CSOUND *, void *),
        nullptr);
    ```
7.  If your instrument module uses any global variables, they must be allocated 
    in the module's `int csoundModuleInit(CSOUND *csound)` function.
8.  If your instrument module uses any global variables, they must be deallocated 
    in the module's `int csoundModuleDestroy(CSOUND *csound)` function.
    
## Usage

Once you have compiled your plugin instrument and installed it in your 
`${OPCODE6DIR64}` directory, use the plugin as an opcode, but in the 
Csound orchestra header. Each plugin will create an instrument template with the 
same name as the plugin, and the instruments will be numbered in the same way as 
regular Csound `instr` templates, that is, in the order that they are 
declared, starting with instrument number 1. 

To configure the instrument with non-default control values, simply assign 
new values to its control channels in the orchestra header. Here is an example:
```
i_instrument_1 MyPlugin1
i_instrument 2 MyPlugin2
gk_MyPlugin2_level init 5
gk_MyPlugin2_fm_index init 3
```


# MVerb

MVerb - Implements Jon Christopher Nelson's waveguide mesh reverb.
 
## Description

Implements Jon Christopher Nelson's waveguide mesh reverb, originally created 
as a Cabbage VST plugin, as a C++ Csound plugin opcode.

MVerb is a modified five-by-five 2D waveguide mesh reverberation effect. It is 
highly flexible and can generate compelling and unique effects timbres ranging 
from traditional spaces to infinite morphing spaces or the simulation of 
metallic plates or cymbals. The plugin incorporates a 10-band parametric 
equalizer for timbral control and delay randomization to create more unusual 
effects.

## Syntax
```
aoutleft, aoutright MVerb ainleft, ainright, Spreset a, a[[, Sparameter, xvalue ],...]
```
## Initialization

*Spreset* -- Name of a built-in preset, one of:

- Default

The order of initialization is:

1. Default values of preset fields.
2. User choice of preset.
3. Default value of non-preset "control channels" (opcode parameters).
4. User-defined opcode parameters (see below).

## Performance

*aoutleft* - Left channel of the output signal.

*aoutright* - Right channel of the output signal.

*[[, Sparameter, xvalue ],...]* -- Any number of control parameters, as name-value pairs:

- *mix* -- Fraction of the output signal that is processed.
- *res1*...res25 -- Resonant frequency of nodes 1 through 25 in the waveguide mesh.
    MYFLT FB;
    MYFLT DFact;
    MYFLT Q;
    MYFLT ERamp;
    MYFLT ERSelect;
    MYFLT EQSelect;
    
The order of processing is:

1. 2 DC blockers for the stereo input signal.
2. 2 multitap delays for early reflections.
3. 25 mesh nodes for the reverb, each with:
3.1 4 variable delays, with optionally randomized delay times.
3.2 4 equalizers, each with:
   3.2.1 10 parametric equalizers (biquad filters).
   3.2.2 1 level balancer.
   3.2.3 1 DC blocker.
9. 2 DC blockers for the stereo output signal.

## Credits

Jon Christopher Nelson wrote the original Cabbage VST plugin.

Michael Gogins adapted Nelson's plugin as a Csound plugin opcode in C++.

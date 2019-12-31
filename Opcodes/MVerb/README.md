# MVerb

MVerb - Implements Jon Christopher Nelson's waveguide mesh reverb.
 
## Description

Implements Jon Christopher Nelson's waveguide mesh reverb, originally created 
as a Cabbage VST plugin, as a C++ Csound plugin opcode.

MVerb is a plugin opcode that is based on a modified five-by-five 2D waveguide 
mesh developed in Csound using the Cabbage framework. MVerb is highly flexible 
and can generate compelling and unique reverberation effects ranging from 
traditional spaces to infinite morphing spaces or the simulation of metallic 
plates or cymbals. The plugin incorporates a 10-band parametric equalizer for 
timbral control and delay randomization to create more unusual effects.

## Syntax
```
aoutleft, aoutright MVerb ainleft, ainright, Spreset a, a[[, Sparameter, xvalue ],...]
```
## Initialization

*Spreset* -- Name of a built-in preset, one of:

- Default

 

 
## Performance

#aoutleft# - Left channel of the output signal.
#aoutright# - Right channel of the output signal.

#[[, Sparameter, xvalue ],...]# - Any number of control parameters, as name-value pairs:

- *mix* -- Fraction of the output signal that is processed.
- *res1*...res25 -- Resonant frequency of nodes 1 through 25 in the waveguide mesh.
    MYFLT FB;
    MYFLT DFact;
    MYFLT Q;
    MYFLT ERamp;
    MYFLT ERSelect;
    MYFLT EQSelect;


## Credits

Original Csound user-defined VST plugin by Jon Christopher Nelson.

Adaptation to C++ plugin opcode by Michael Gogins.


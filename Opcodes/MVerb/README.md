# MVerb

MVerb - Implements Jon Christopher Nelson's waveguide mesh reverb.
 
## Description

Implements Jon Christopher Nelson's waveguide mesh reverb, originally created 
as a Cabbage VST plugin, as a C++ Csound plugin opcode.

MVerb is a modified five-by-five 2D waveguide mesh reverberator. It is highly 
flexible and can generate compelling and unique effects timbres ranging from 
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

The order of initialization is:

1. Default values of all parameters.
2. The user's choice of preset determines most parameters.
3. The user can override any parameter(s) using opcode parameters.

## Performance

*aoutleft* - Left channel of the output signal.

*aoutright* - Right channel of the output signal.

*[[, Sparameter, xvalue ],...]* -- Any number of the following control 
parameters, as *name, value* pairs. These are real-valued unless they are 
strings.

- *mix* -- Fraction of the output signal that is not processed.
- *res1* -- Resonant frequency of node 1 in the mesh.
- *res2* -- Resonant frequency of node 2 in the mesh.
- *res3* -- Resonant frequency of node 3 in the mesh.
- *res4* -- Resonant frequency of node 4 in the mesh.
- *res5* -- Resonant frequency of node 5 in the mesh.
- *res6* -- Resonant frequency of node 6 in the mesh.
- *res7* -- Resonant frequency of node 7 in the mesh.
- *res8* -- Resonant frequency of node 8 in the mesh.
- *res9* -- Resonant frequency of node 9 in the mesh.
- *res10* -- Resonant frequency of node 10 in the mesh.
- *res11* -- Resonant frequency of node 11 in the mesh.
- *res12* -- Resonant frequency of node 12 in the mesh.
- *res13* -- Resonant frequency of node 13 in the mesh.
- *res14* -- Resonant frequency of node 14 in the mesh.
- *res15* -- Resonant frequency of node 15 in the mesh.
- *res16* -- Resonant frequency of node 16 in the mesh.
- *res17* -- Resonant frequency of node 17 in the mesh.
- *res18* -- Resonant frequency of node 18 in the mesh.
- *res19* -- Resonant frequency of node 19 in the mesh.
- *res20* -- Resonant frequency of node 20 in the mesh.
- *res21* -- Resonant frequency of node 21 in the mesh.
- *res22* -- Resonant frequency of node 22 in the mesh.
- *res23* -- Resonant frequency of node 23 in the mesh.
- *res24* -- Resonant frequency of node 24 in the mesh.
- *res25* -- Resonant frequency of node 25 in the mesh.
- *FB* -- Delay feedback.
- *DFact* -- Delay factor.
- *Q* -- Q of the equalizer filters.
- *ERamp* -- Amplitude of early reflections.
- *ERselect* -- Name of the early reflections preset, one of:
- *EQselect* -- Name of the equalization preset, one of:
- *eq1* -- Gain of equalizer band 1.
- *eq2* -- Gain of equalizer band 2.
- *eq3* -- Gain of equalizer band 3.
- *eq4* -- Gain of equalizer band 4.
- *eq5* -- Gain of equalizer band 5.
- *eq6* -- Gain of equalizer band 6.
- *eq7* -- Gain of equalizer band 7.
- *eq8* -- Gain of equalizer band 8.
- *eq9* -- Gain of equalizer band 9.
- *eq10* -- Gain of equalizer band 10.
- *random* -- Whether (1) or not (0) the randomization of mesh delays is enabled.
- *rslow* -- Lower limit of mesh delay randomization frequency.
- *rfast* -- Upper limit of mesh delay randomization frequency.;
- *FBclear* -- Amount by which feedback delays are cleared.
    
The order of processing is:

1.  2 DC blockers for the stereo input signal.
2.  2 multitap delays for stereo early reflections.
3.  25 mesh nodes for the reverb, each with:
    1.  4 variable delays, with optionally randomized delay times.
    1.  4 equalizers, each with:    
        1.  10 parametric biquad filters.
        2.  1 level balancer.
        3.  1 DC blocker.        
4.  2 DC blockers for the stereo output signal.

## Credits

Jon Christopher Nelson wrote the original Cabbage VST plugin.

Michael Gogins adapted Nelson's plugin as a Csound plugin opcode in C++.
